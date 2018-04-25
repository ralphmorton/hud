{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Dashboard.Server.HUD (
    query
) where

import Prelude hiding (log)

import HUD.Data
import HUD.Data.HUD
import HUD.Operational
import HUD.Names ()
import HUD.Logging
import HUD.IPC.Client
import HUD.Dashboard.Data

import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO)

--
--
--

query :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r MinLogLevel) => (UserKey, User) -> Req -> m Rsp
query = query' . snd

--

query' :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r MinLogLevel) => User -> Req -> m Rsp
query' user (RQGithub req) = case userGithubToken user of
    Nothing -> throwIO MissingGithubToken
    Just tok -> do
        res <- ipc 30 (tok, req)
        case res of
            Just (IPCResult rsp) ->
                pure (RSGithub rsp)
            _ -> do
                logFailure "Github" req res
                throwIO InternalFailure
query' user (RQTrello req) = case userTrelloToken user of
    Nothing -> throwIO MissingTrelloToken
    Just tok -> do
        res <- ipc 30 (tok, req)
        case res of
            Just (IPCResult rsp) ->
                pure (RSTrello rsp)
            _ -> do
                logFailure "Trello" req res
                throwIO InternalFailure

--

logFailure :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r MinLogLevel) => (Show a, Show b) => Text -> a -> Maybe (IPCResult b) -> m ()
logFailure svc req res = log Log {
    logTime = (),
    logLevel = Warning,
    logSource = "HUD.Dashboard.Server.HUD.logFailure",
    logTitle = svc <> " HUD generation failed",
    logData = M.fromList $ [
        ("request", pack $ show req),
        ("result", pack $ show res)
    ]
}
