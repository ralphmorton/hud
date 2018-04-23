{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Dashboard.Server.HUD (
    fetchHUD
) where

import Prelude hiding (log)

import HUD.Data
import HUD.Data.HUD
import HUD.Data.HUD.Github (GithubHUDReq, GithubHUDRsp)
import HUD.Operational
import HUD.Names (Github)
import HUD.Logging
import HUD.IPC.Client
import HUD.Dashboard.Data

import qualified Data.Map as M
import Data.Text (pack)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO)

--
--
--

fetchHUD :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r MinLogLevel) => (UserKey, User) -> HUDReq -> m HUDRsp
fetchHUD = maybe missingToken fetchHUD' . userGithubToken . snd
    where missingToken = const (throwIO MissingGithubToken)
    
--

fetchHUD' :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r MinLogLevel) => OAuthToken Github -> HUDReq -> m HUDRsp
fetchHUD' tok (HRQGithub greq) = do
    res <- ipc 30 (tok, greq)
    case res of
        Just (IPCResult rsp) ->
            pure (HRSGithub rsp)
        _ -> do
            logGithubFailure greq res
            throwIO InternalFailure

--

logGithubFailure :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r MinLogLevel) => GithubHUDReq -> Maybe (IPCResult GithubHUDRsp) -> m ()
logGithubFailure req res = log Log {
    logTime = (),
    logLevel = Warning,
    logSource = "HUD.Dashboard.Server.HUD.logGithubFailure",
    logTitle = "Github HUD generation failed",
    logData = M.fromList $ [
        ("request", pack $ show req),
        ("result", pack $ show res)
    ]
}
