{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Identity.Server.Github (
    GithubClient(..),
    authoriseGithub
) where

import HUD.Context (viewC)
import HUD.Data
import HUD.Operational
import HUD.Names ()
import HUD.Identity.Crypto
import HUD.Identity.Server.Common

import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.HTTP.Conduit hiding (Request, http)
import Network.HTTP.Types (methodPost)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO)

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

authoriseGithub :: (
    MonadUnliftIO m,
    MonadThrow m,
    ContextReader r m,
    HasContext r HttpManager,
    HasContext r HMACKey,
    HasContext r GithubClient) => Text -> m Token
authoriseGithub code = do
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
    ident <- parseIdentity rsp
    encodeToken ident tokenTTL

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

parseIdentity :: MonadUnliftIO m => Response ByteString -> m Identity
parseIdentity rsp = maybe (throwAuthFailed rsp) pure secret
    where secret = GithubIdentity . unClientSecret <$> decode (responseBody rsp)

--

throwAuthFailed :: MonadUnliftIO m => Response ByteString -> m a
throwAuthFailed rsp = throwIO (GithubAuthorisationFailed body)
    where body = (responseBody rsp)

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

--

newtype ClientSecret = ClientSecret {
    unClientSecret :: GithubToken
}

instance FromJSON ClientSecret where
    parseJSON (Object o) = ClientSecret . GithubOAuthToken <$> o .: "access_token"
    parseJSON _ = mzero
