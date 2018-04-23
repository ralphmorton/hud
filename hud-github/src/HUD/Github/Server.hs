{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Github.Server (
    serve
) where

import Prelude hiding (log)

import HUD.Data (OAuthToken, Log'(..), LogLevel(..))
import HUD.Data.HUD.Github
import HUD.Operational
import HUD.Names (Github)
import HUD.Logging
import HUD.IPC.Server
import HUD.Github.Server.Repo

import Control.Exception (SomeException)
import qualified Data.Map as M
import Data.Text (pack)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (catchAny)

--
--
--

serve :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r MinLogLevel) => m ()
serve = serveIPC (uncurry request)

--
--
--

request :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r MinLogLevel) => OAuthToken Github -> GithubHUDReq -> m (Result GithubHUDRsp)
request tok req = catchAny (Success <$> runRequest tok req) $ \e -> do
    logRequestException req e
    pure (Success GHHRSFailure)

--
--
--

runRequest :: MonadUnliftIO m => OAuthToken Github -> GithubHUDReq -> m GithubHUDRsp
runRequest tok (GHHRQRepoPRs account repo) =
    GHHRSRepoPRs <$> repoPRs tok account repo
runRequest tok (GHHRQRepoPR account repo num) =
    GHHRSRepoPR <$> repoPR tok account repo num

--
--
--

logRequestException :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r MinLogLevel) => GithubHUDReq -> SomeException -> m ()
logRequestException req e = log Log {
    logTime = (),
    logLevel = Error,
    logSource = "HUD.Github.Server.logRequestException",
    logTitle = "Github request failed",
    logData = M.fromList $ [
        ("request", (pack . show) req),
        ("exception", (pack . show) e)
    ]
}
