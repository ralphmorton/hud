{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Dashboard.Server.OAuth (
    getTokenState,
    authGithub,
    authTrello,
    authHeroku
) where

import Prelude hiding (log)

import HUD.Data
import HUD.Operational
import HUD.Names (Github, Heroku, Trello)
import HUD.Logging
import HUD.IPC.Client
import HUD.Dashboard.Data
import HUD.Dashboard.Persistence (setUserGithubToken, setUserHerokuToken, setUserTrelloToken)

import Control.Monad (void)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO)

--
--
--

getTokenState :: User -> TokenState
getTokenState u = TokenState {
    tsGithub = isJust (userGithubToken u),
    tsTrello = isJust (userTrelloToken u),
    tsHeroku = isJust (userHerokuToken u)
}

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
            logOAuthFailure "Github" uk res
            throwIO InternalFailure

--
--
--

authTrello :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r SqlPool,
    HasContext r MinLogLevel) => (UserKey, User) -> OAuthCode Trello -> m ()
authTrello (uk, _) = void . sql . setUserTrelloToken uk . pure . OAuthToken . unOAuthCode

--
--
--

authHeroku :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r SqlPool,
    HasContext r MinLogLevel) => (UserKey, User) -> OAuthCode Heroku -> m ()
authHeroku (uk, _) code = do
    res <- ipc 10 code
    case res of
        Just (IPCResult (ORSuccess tok)) ->
            (void . sql) (setUserHerokuToken uk (pure tok))
        _ -> do
            logOAuthFailure "Heroku" uk res
            throwIO InternalFailure

--
--
--

logOAuthFailure :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r MinLogLevel) => Text -> UserKey -> Maybe (IPCResult (OAuthResult a)) -> m ()
logOAuthFailure name uk res = log Log {
    logTime = (),
    logLevel = Error,
    logSource = "HUD.Dashboard.Server.OAuth.logOAuthFailure",
    logTitle = name <> " authorisation failed",
    logData = M.fromList $ [
        ("user", pack $ show uk),
        ("result", pack $ show res)
    ]
}
