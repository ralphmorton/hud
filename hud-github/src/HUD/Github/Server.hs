{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Github.Server (
    serveGithub
) where

import Prelude hiding (log)

import HUD.Data (GithubToken, Log'(..), LogLevel(..))
import HUD.Data.HUD.Github
import HUD.Operational
import HUD.Names ()
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

serveGithub :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r MinLogLevel) => m ()
serveGithub = serveIPC (uncurry request)

--
--
--

request :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r MinLogLevel) => GithubToken -> HUDReq -> m (Result HUDRsp)
request tok req = catchAny (Success <$> runRequest tok req) $ \e -> do
    logRequestException req e
    pure (Success HRSFailure)

--
--
--

runRequest :: MonadUnliftIO m => GithubToken -> HUDReq -> m HUDRsp
runRequest tok (HRQRepoPRs account repo) =
    HRSRepoPRs <$> repoPRs tok account repo
runRequest tok (HRQRepoPR account repo num) =
    HRSRepoPR <$> repoPR tok account repo num

--
--
--

logRequestException :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r MinLogLevel) => HUDReq -> SomeException -> m ()
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
