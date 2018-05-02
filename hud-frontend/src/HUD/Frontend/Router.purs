
module HUD.Frontend.Router (
    ROUTE,
    Route(..),
    PublicRoute(..),
    AuthedRoute(..),
    match
) where

import Prelude

import HUD.Data.OAuth (OAuthCode)
import HUD.Dashboard.Data.Relational (AccountKey)
import HUD.Frontend.Web.Route (class IsRoute, type (:/), Capture, HashParam, L, PathInfo(..), QueryParam, Target, fromPathInfo, route, unroute, (:->))

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut (class DecodeJson, Json, decodeJson, toObject, (.?))
import Data.Either (Either(Left), either)
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
import Data.Tuple (uncurry)
import Type.Proxy (Proxy(..))

--
--
--

foreign import data ROUTE :: Effect

--

foreign import getPathInfo_ :: forall e. Eff (route :: ROUTE | e) Json

--
--
--

data Route
    = Public PublicRoute
    | Authed AuthedRoute

derive instance eqRoute :: Eq Route

instance isRouteRoute :: IsRoute Route where
    fromPathInfo px =
        route pLogin px (Public Login)
        <|>
        route pOnboard px (Public Onboard)
        <|>
        route pError px (Public Error)
        <|>
        route pSelectAccount px (Authed SelectAccount)
        <|>
        route pApps px (Authed Apps)
        <|>
        route pAuthGithub px (Authed <<< AuthGithub)
        <|>
        route pAuthTrello px (Authed <<< AuthTrello)
        <|>
        route pAuthHeroku px (Authed <<< AuthHeroku)
        <|>
        route pList px (Authed <<< List)
        <|>
        route pHUD px (Authed <<< HUD)
    toPath (Public pr) = case pr of
        Login -> unroute pLogin unit
        Onboard -> unroute pOnboard unit
        Error -> unroute pError unit
    toPath (Authed ar) = case ar of
        SelectAccount -> unroute pSelectAccount unit
        Apps -> unroute pApps unit
        AuthGithub code -> unroute pAuthGithub (code :-> unit)
        AuthTrello code -> unroute pAuthTrello (code :-> unit)
        AuthHeroku code -> unroute pAuthHeroku (code :-> unit)
        List acc -> unroute pList (acc :-> unit)
        HUD acc -> unroute pHUD (acc :-> unit)

--

data PublicRoute
    = Login
    | Onboard
    | Error

derive instance eqPublicRoute :: Eq PublicRoute

type Login = L "login" :/ Target Route

pLogin :: Proxy Login
pLogin = Proxy

type Onboard = L "onboard" :/ Target Route

pOnboard :: Proxy Onboard
pOnboard = Proxy

type Error = L "error" :/ Target Route

pError :: Proxy Error
pError = Proxy

--

data AuthedRoute
    = SelectAccount
    | Apps
    | AuthGithub (Maybe (OAuthCode Unit))
    | AuthTrello (Maybe (OAuthCode Unit))
    | AuthHeroku (Maybe (OAuthCode Unit))
    | List AccountKey
    | HUD AccountKey

derive instance eqAuthedRoute :: Eq AuthedRoute

type SelectAccount = Target Route

pSelectAccount :: Proxy SelectAccount
pSelectAccount = Proxy

type Apps = L "apps" :/ Target Route

pApps :: Proxy Apps
pApps = Proxy

type AuthGithub = L "apps" :/ L "github" :/ L "auth" :/ QueryParam "code" (OAuthCode Unit) :/ Target Route

pAuthGithub :: Proxy AuthGithub
pAuthGithub = Proxy

type AuthTrello = L "apps" :/ L "trello" :/ L "auth" :/ HashParam "token" (OAuthCode Unit) :/ Target Route

pAuthTrello :: Proxy AuthTrello
pAuthTrello = Proxy

type AuthHeroku = L "apps" :/ L "heroku" :/ L "auth" :/ QueryParam "code" (OAuthCode Unit) :/ Target Route

pAuthHeroku :: Proxy AuthHeroku
pAuthHeroku = Proxy

type List = AccountPath (Target Route)

pList :: Proxy List
pList = Proxy

type HUD = AccountPath (L "hud" :/ Target Route)

pHUD :: Proxy HUD
pHUD = Proxy

--
--
--

type AccountPath p = Capture "account" AccountKey :/ p

--
--
--

match :: forall m e. MonadAff (route :: ROUTE | e) m => m (Maybe Route)
match = liftAff match'

match' :: forall e. Aff (route :: ROUTE | e) (Maybe Route)
match' = (fromPathInfo <=< parsePathInfo) <$> liftEff getPathInfo_

parsePathInfo :: Json -> Maybe PathInfo
parsePathInfo j = do
    PI i <- either (const Nothing) Just (decodeJson j)
    pure (PathInfo i.px i.qpx i.hpx)

--
--
--

compose2 :: forall a b c d. (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 g f a = g <<< f a

infixr 9 compose2 as <<<<

--
--
--

data PI = PI {
    px :: Array String,
    qpx :: SM.StrMap String,
    hpx :: SM.StrMap String
}

instance decodePI :: DecodeJson PI where
    decodeJson j = case toObject j of
        Just o -> do
            px <- o .? "px"
            qpx <- o .? "qpx"
            hpx <- o .? "hpx"
            (pure <<< PI) {
                px: px,
                qpx: qpx,
                hpx: hpx
            }
        _ -> Left "No parse"
