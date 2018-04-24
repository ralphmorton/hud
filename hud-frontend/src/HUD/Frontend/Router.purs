
module HUD.Frontend.Router (
    ROUTE,
    Route(..),
    PublicRoute(..),
    AuthedRoute(..),
    match
) where

import Prelude

import HUD.Dashboard.Data.Relational (AccountKey)
import HUD.Frontend.Web.Route (class IsRoute, type (:/), Capture, L, PathInfo(..), Target, fromPathInfo, route, unroute, (:->))

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut (Json, decodeJson)
import Data.Either (either)
import Data.Maybe (Maybe(..))
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
        route pList px (Authed <<< List)
    toPath (Public pr) = case pr of
        Login -> unroute pLogin unit
        Onboard -> unroute pOnboard unit
        Error -> unroute pError unit
    toPath (Authed ar) = case ar of
        SelectAccount -> unroute pSelectAccount unit
        List acc -> unroute pList (acc :-> unit)

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
    | List AccountKey

derive instance eqAuthedRoute :: Eq AuthedRoute

type SelectAccount = Target Route

pSelectAccount :: Proxy SelectAccount
pSelectAccount = Proxy

type List = Capture "account" AccountKey :/ Target Route

pList :: Proxy List
pList = Proxy

--
--
--

match :: forall m e. MonadAff (route :: ROUTE | e) m => m (Maybe Route)
match = liftAff match'

match' :: forall e. Aff (route :: ROUTE | e) (Maybe Route)
match' = (fromPathInfo <=< parsePathInfo) <$> liftEff getPathInfo_

parsePathInfo :: Json -> Maybe PathInfo
parsePathInfo j = uncurry PathInfo <$> either (const Nothing) Just (decodeJson j)

--
--
--

compose2 :: forall a b c d. (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 g f a = g <<< f a

infixr 9 compose2 as <<<<
