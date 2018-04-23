
module HUD.Frontend.Network.HTTP (
    module Data.HTTP.Method,
    module Network.HTTP.Affjax,
    module Network.HTTP.Affjax.Request,
    module Network.HTTP.Affjax.Response,
    module Network.HTTP.RequestHeader,
    class AsHttpException,
    HttpException(..),
    Request,
    asHttpException,
    buildReq,
    addHeaders,
    http,
    httpJSON,
    noData,
    jsonData
) where

import Prelude

import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonParser)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax, Affjax)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable, fromResponse)
import Network.HTTP.RequestHeader (RequestHeader(..), requestHeaderName, requestHeaderValue)
import Network.HTTP.StatusCode (StatusCode(..))

--
--
--

class AsHttpException e where
    asHttpException :: HttpException -> e

data HttpException
    = NetworkException Error
    | StatusCodeException Int
    | JsonParseException String String

instance showHttpException :: Show HttpException where
    show (NetworkException e) = "NetworkException " <> show e
    show (StatusCodeException c) = "StatusCodeException " <> show c
    show (JsonParseException e j) = "JsonParseException " <> show e <> " " <> show j

instance asHttpExceptionHttpException :: AsHttpException HttpException where
    asHttpException = id

--
--
--

type Request a = AffjaxRequest a

--
--
--

buildReq :: forall m a. Applicative m => Method -> String -> Maybe a -> m (AffjaxRequest a)
buildReq mthd url dta = pure req
    where
    req = {
        method: Left mthd,
        url: url,
        headers: [],
        content: dta,
        username: Nothing,
        password: Nothing,
        withCredentials: false
    }

--
--
--

addHeaders :: forall a. Array RequestHeader -> AffjaxRequest a -> AffjaxRequest a
addHeaders hx r = r { headers = r.headers <> hx }

--
--
--

http :: forall m e a r ex.
    MonadAff (ajax :: AJAX | e) m =>
    MonadError ex m =>
    AsHttpException ex =>
    Requestable a =>
    Respondable r => AffjaxRequest a -> m r
http req = either (throwError <<< asHttpException) pure =<< httpSafe req

--

httpSafe :: forall m e a b ex.
    MonadAff (ajax :: AJAX | e) m =>
    MonadError ex m =>
    Requestable a =>
    Respondable b => AffjaxRequest a -> m (Either HttpException b)
httpSafe req = do
    res <- liftAff (catchError (Right <$> affjax req) (pure <<< Left))
    case res of
        Left e -> (pure <<< Left) (NetworkException e)
        Right { status : StatusCode code, response: body } ->
            case code < 300 of
                true -> pure (pure body)
                false -> pure (Left $ StatusCodeException code)

--
--
--

httpJSON :: forall m e a r ex.
    MonadAff (ajax :: AJAX | e) m =>
    MonadError ex m =>
    AsHttpException ex =>
    Requestable a =>
    DecodeJson r => AffjaxRequest a -> m r
httpJSON req = do
    json <- http req
    case (decodeJson <=< jsonParser) json of
        Left e -> (throwError <<< asHttpException) (JsonParseException e json)
        Right r -> pure r

--
--
--

noData :: Maybe Unit
noData = Nothing

--
--
--

jsonData :: forall a. EncodeJson a => a -> Maybe Json
jsonData = pure <<< encodeJson
