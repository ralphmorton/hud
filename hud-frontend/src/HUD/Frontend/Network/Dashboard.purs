
module HUD.Frontend.Network.Dashboard (
    login,
    setPassword,
    listAccounts,
    getTokenState,
    authGithub,
    authTrello,
    authHeroku,
    hud
) where

import Prelude

import HUD.Data.Common (EmailAddress)
import HUD.Data.HUD (Req, Rsp)
import HUD.Data.Identity (Token, unToken)
import HUD.Data.OAuth (OAuthCode)
import HUD.Dashboard.Data (TokenState)
import HUD.Dashboard.Data.Relational (Account, AccountKey, UserLevel)
import HUD.Frontend.Operational (IdentityInfo(..), OpM)
import HUD.Frontend.Network.HTTP (class Requestable, AJAX, Method(GET, POST), Request, RequestHeader(..), addHeaders, buildReq, http, httpJSON, jsonData, noData)

import Control.Monad ((<=<))
import Control.Monad.Reader (ask)
import Data.Bifunctor (bimap)
import Data.Generic (class Generic)
import Data.Lens ((^.))
import Data.Maybe (Maybe)
import Data.Time.Duration (Seconds(..))
import Data.Tuple (Tuple(..))

--
--
--

login :: forall e i c. EmailAddress -> String -> OpM i c (ajax :: AJAX | e) (Tuple Seconds Token)
login addr pw = bimap Seconds id <$> m
    where m = httpJSON =<< unauthedReq POST "api/auth/login" (jsonData $ Tuple addr pw)

--
--
--

setPassword :: forall e i c. Token -> String -> OpM i c (ajax :: AJAX | e) Unit
setPassword tok password = do
    req <- unauthedReq POST "api/auth/password/set" (jsonData password)
    let req' = addHeaders [RequestHeader "Authorization" (tok ^. unToken)] req
    http req'

--
--
--

listAccounts :: forall e c. OpM IdentityInfo c (ajax :: AJAX | e) (Array (Tuple (Tuple AccountKey Account) UserLevel))
listAccounts = httpJSON =<< authedReq GET "api/accounts" noData

--
--
--

getTokenState :: forall e c. OpM IdentityInfo c (ajax :: AJAX | e) TokenState
getTokenState = httpJSON =<< authedReq GET "api/auth/oauth/state" noData

--
--
--

authGithub :: forall e c a. Generic a => OAuthCode a -> OpM IdentityInfo c (ajax :: AJAX | e) Unit
authGithub = http <=< authedReq POST "api/auth/oauth/github" <<< jsonData

--
--
--

authTrello :: forall e c a. Generic a => OAuthCode a -> OpM IdentityInfo c (ajax :: AJAX | e) Unit
authTrello = http <=< authedReq POST "api/auth/oauth/trello" <<< jsonData

--
--
--

authHeroku :: forall e c a. Generic a => OAuthCode a -> OpM IdentityInfo c (ajax :: AJAX | e) Unit
authHeroku = http <=< authedReq POST "api/auth/oauth/heroku" <<< jsonData

--
--
--

hud :: forall e c. Req -> OpM IdentityInfo c (ajax :: AJAX | e) Rsp
hud = httpJSON <=< authedReq POST "api/hud" <<< jsonData

--
--
--

unauthedReq :: forall a i c e. Requestable a => Method -> String -> Maybe a -> OpM i c e (Request a)
unauthedReq mthd path body = do
    baseURL <- _.config.dashboard.url <$> ask
    let url = baseURL <> path
    let ctype = RequestHeader "Content-Type" "application/json"
    addHeaders [ctype] <$> buildReq mthd url body

--
--
--

authedReq :: forall a c e. Requestable a => Method -> String -> Maybe a -> OpM IdentityInfo c e (Request a)
authedReq mthd path body = do
    tok <- token
    baseURL <- _.config.dashboard.url <$> ask
    let url = baseURL <> path
    let ctype = RequestHeader "Content-Type" "application/json"
    let auth = RequestHeader "Authorization" (tok ^. unToken)
    addHeaders [ctype, auth] <$> buildReq mthd url body

--

token :: forall c e. OpM IdentityInfo c e Token
token = do
    { i: IIUser tok } <- ask
    pure tok
