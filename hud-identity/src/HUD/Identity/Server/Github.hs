{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Identity.Server.Github (
    authoriseGithub
) where

import HUD.Data
import HUD.Operational
import HUD.Names ()
import HUD.Identity.Crypto
import HUD.Identity.Server.Common

import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Network.HTTP.Conduit hiding (Request, http)
import Network.HTTP.Types (methodPost)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO)

--
--
--

authoriseGithub :: (
    MonadUnliftIO m,
    MonadThrow m,
    ContextReader r m,
    HasContext r HttpManager,
    HasContext r HMACKey) => Text -> m Token
authoriseGithub code = do
    ireq <- parseRequest (unpack $ "https://github.com/login/oauth/" <> code)
    rsp <- http ireq {
        method = methodPost,
        requestHeaders = ("Accept", "application/json") : requestHeaders ireq,
        responseTimeout = responseTimeoutMicro 30000000
    }
    ident <- parseIdentity rsp
    encodeToken ident tokenTTL

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

newtype ClientSecret = ClientSecret {
    unClientSecret :: GithubToken
}

instance FromJSON ClientSecret where
    parseJSON (Object o) = ClientSecret . GithubOAuthToken <$> o .: "access_token"
    parseJSON _ = mzero
