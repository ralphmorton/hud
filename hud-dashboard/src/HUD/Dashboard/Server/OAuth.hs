{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Dashboard.Server.OAuth (
    authGithub
) where

import Prelude hiding (log)

import HUD.Data
import HUD.Operational
import HUD.Names (Github)
import HUD.Logging
import HUD.IPC.Client
import HUD.Dashboard.Data
import HUD.Dashboard.Persistence (setUserGithubToken)

import Control.Monad (void)
import qualified Data.Map as M
import Data.Text (pack)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO)

--
--
--

authGithub :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r SqlPool,
    HasContext r MinLogLevel) => (UserKey, User) -> OAuthCode Github -> m ()
authGithub (uk, _) code = do
    res <- ipc 10 code
    case res of
        Just (IPCResult (ORSuccess tok)) ->
            (void . sql) (setUserGithubToken uk (pure tok))
        _ -> do
            logGithubFailure uk res
            throwIO InternalFailure

--

logGithubFailure :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r MinLogLevel) => UserKey -> Maybe (IPCResult (OAuthResult Github)) -> m ()
logGithubFailure uk res = log Log {
    logTime = (),
    logLevel = Error,
    logSource = "HUD.Dashboard.Server.OAuth.logGithubFailure",
    logTitle = "Github authorisation failed",
    logData = M.fromList $ [
        ("user", pack $ show uk),
        ("result", pack $ show res)
    ]
}
