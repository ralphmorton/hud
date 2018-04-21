{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Github.Authoriser (
    GithubClient(..),
    authorise
) where

import Prelude hiding (log)

import HUD.Context (viewC)
import HUD.Data
import HUD.Operational
import HUD.Names (Github)
import HUD.Logging
import HUD.IPC.Server

import Control.Monad.Catch (MonadThrow)
import Data.Aeson hiding (Error, Result, Success)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as M
import Data.Text (Text, pack)
import Network.HTTP.Conduit hiding (Request, http)
import Network.HTTP.Types (methodPost)
import UnliftIO (MonadUnliftIO)

--
--
--

data GithubClient = GithubClient {
    ghcClientID :: Text,
    ghcClientSecret :: Text
}

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
    HasContext r GithubClient) => m ()
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
    HasContext r GithubClient) => OAuthCode Github -> m (Result (OAuthResult Github))
auth (OAuthCode code) = do
    reqBody <- buildReqBody code
    ireq <- parseRequest "https://github.com/login/oauth/access_token"
    rsp <- http ireq {
        method = methodPost,
        requestHeaders =
            ("User-Agent", "HUD V1.0") :
            ("Content-Type", "application/json") :
            ("Accept", "application/json") :
            requestHeaders ireq,
        requestBody = (RequestBodyLBS . encode) reqBody,
        responseTimeout = responseTimeoutMicro 30000000
    }
    case decode (responseBody rsp) of
        Just tok ->
            (pure . Success) (ORSuccess tok)
        Nothing -> do
            logAuthException code rsp
            pure (Success ORFailure)

--

buildReqBody :: (
    ContextReader r m,
    HasContext r GithubClient) => Text -> m GithubAuthReq
buildReqBody code = do
    GithubClient clientID clientSecret <- viewC
    pure GithubAuthReq {
        gaqClientID = clientID,
        gaqClientSecret = clientSecret,
        gaqCode = code
    }

--

logAuthException :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r MinLogLevel) => Text -> Response ByteString -> m ()
logAuthException code rsp = log Log {
    logTime = (),
    logLevel = Error,
    logSource = "HUD.Github.Authoriser.logAuthException",
    logTitle = "Github authorisation failed",
    logData = M.fromList $ [
        ("code", code),
        ("response", (pack . show) rsp)
    ]
}


--
--
--

data GithubAuthReq = GithubAuthReq {
    gaqClientID :: Text,
    gaqClientSecret :: Text,
    gaqCode :: Text
}

instance ToJSON GithubAuthReq where
    toJSON r =
        object [
            "client_id" .= gaqClientID r,
            "client_secret" .= gaqClientSecret r,
            "code" .= gaqCode r
        ]
