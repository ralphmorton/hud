
module HUD.Frontend.Network.Identity (
    validateEmailAddress,
    confirmEmailAddress
) where

import Prelude

import HUD.Data.Common (EmailAddress)
import HUD.Data.Identity (Token)
import HUD.Frontend.Operational (OpM)
import HUD.Frontend.Network.HTTP (class Requestable, AJAX, Method(POST), Request, RequestHeader(..), addHeaders, buildReq, http, httpJSON, jsonData)

import Control.Monad.Reader (ask)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))

--
--
--

validateEmailAddress :: forall e i c. EmailAddress -> OpM i c (ajax :: AJAX | e) Unit
validateEmailAddress = http <=< (req POST "api/v1/email/identify" <<< jsonData)

--
--
--

confirmEmailAddress :: forall e i c. EmailAddress -> String -> OpM i c (ajax :: AJAX | e) Token
confirmEmailAddress addr = httpJSON <=< (req POST "api/v1/email/confirm" <<< jsonData <<< Tuple addr)

--
--
--

req :: forall a i c e. Requestable a => Method -> String -> Maybe a -> OpM i c e (Request a)
req mthd path body = do
    baseURL <- _.config.identity.url <$> ask
    let url = baseURL <> path
    let ctype = RequestHeader "Content-Type" "application/json"
    addHeaders [ctype] <$> buildReq mthd url body
