{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Heroku.Authoriser (
    authorise
) where

import Prelude hiding (log)

import HUD.Context (viewC)
import HUD.Data
import HUD.Operational
import HUD.Names (Heroku)
import HUD.Logging
import HUD.IPC.Server
import HUD.Heroku.Types

import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson hiding (Error, Result, Success)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Conduit hiding (Request, http)
import Network.HTTP.Types (methodPost)
import UnliftIO (MonadUnliftIO)

--
--
--

authorise :: (
    MonadUnliftIO m,
    MonadThrow m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r HttpManager,
    HasContext r MinLogLevel,
    HasContext r HerokuClient) => m ()
authorise = serveIPC auth

--
--
--

auth :: (
    MonadUnliftIO m,
    MonadThrow m,
    ContextReader r m,
    HasContext r HttpManager,
    HasContext r MinLogLevel,
    HasContext r HerokuClient) => OAuthCode Heroku -> m (Result (OAuthResult Heroku))
auth (OAuthCode code) = do
    body <- buildReqBody code
    ireq <- parseRequest "https://id.heroku.com/oauth/token"
    rsp <- http $ urlEncodedBody body ireq {
        method = methodPost,
        requestHeaders =
            ("User-Agent", "HUD V1.0") :
            ("Content-Type", "application/x-www-form-urlencoded") :
            ("Accept", "application/json") :
            requestHeaders ireq,
        responseTimeout = responseTimeoutMicro 30000000
    }
    case decode (responseBody rsp) of
        Just (RefreshToken tok) ->
            (pure . Success) (ORSuccess (OAuthToken tok))
        Nothing -> do
            logAuthException code rsp
            pure (Success ORFailure)

--

buildReqBody :: (
    ContextReader r m,
    HasContext r HerokuClient) => Text -> m [(B.ByteString, B.ByteString)]
buildReqBody code = do
    HerokuClient _ clientSecret <- viewC
    pure
        [
            ("grant_type", "authorization_code"),
            ("code", encodeUtf8 code),
            ("client_secret", encodeUtf8 clientSecret)
        ]

--

logAuthException :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r MinLogLevel) => Text -> Response BL.ByteString -> m ()
logAuthException code rsp = log Log {
    logTime = (),
    logLevel = Error,
    logSource = "HUD.Heroku.Authoriser.logAuthException",
    logTitle = "Heroku authorisation failed",
    logData = M.fromList $ [
        ("code", code),
        ("response", (pack . show) rsp)
    ]
}

--
--
--

newtype RefreshToken = RefreshToken Text

instance FromJSON RefreshToken where
    parseJSON (Object o) = RefreshToken <$> o .: "refresh_token"
    parseJSON _ = mzero
