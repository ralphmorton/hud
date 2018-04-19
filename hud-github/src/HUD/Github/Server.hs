{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module HUD.Github.Server (
    API,
    server
) where

import Prelude hiding (log)

import HUD.Context (Context)
import HUD.Data
import HUD.Operational
import HUD.Logging
import HUD.Names ()
import HUD.IPC.Client (IPCResult(IPCResult), ipc)
import HUD.Github.Types
import HUD.Github.Server.Repo

import Control.Exception (handle)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text (pack)
import Servant hiding (Context)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO)

--
--
--

type API = Header "Authorization" Token :> "api" :> "v1" :>
    (
        "repo" :>
        (
            "prs" :> ReqBody '[JSON] (GithubAccount, GithubRepo) :> Post '[JSON] [PR]
            :<|>
            "pr" :> ReqBody '[JSON] (GithubAccount, GithubRepo, GithubPR) :> Post '[JSON] PRDetails
        )
    )

--
--
--

server :: (
    HasContext r AmqpPool,
    HasContext r MinLogLevel,
    HasContext r HttpManager) => Context r -> Server API
server ctx = hoistServer (Proxy :: Proxy API) nat server'
    where
    nat f = do
        res <- liftIO $ handle (pure . Left) (Right <$> runReaderT f ctx)
        either throwE pure res

throwE :: GithubError -> Handler a
throwE MissingAuthToken =
    throwError err401 {
        errBody = "Missing auth token"
    }
throwE BadAuthToken =
    throwError err403 {
        errBody = "Bad auth token"
    }
throwE (GithubError e) =
    throwError err400 {
        errBody = "Github error: " <> BL.pack (show e)
    }

--

server' :: (
    MonadUnliftIO m,
    MonadThrow m,
    ContextReader r m,
    HasContext r HttpManager,
    HasContext r AmqpPool,
    HasContext r MinLogLevel) => ServerT API m
server' tok =
    (
       authUser tok . uncurry repoPRs
       :<|>
       authUser tok . repoPR
    )

--
--
--


--
--
--

authUser :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r MinLogLevel) => Maybe Token -> (GithubToken -> m a) -> m a
authUser Nothing _ = throwIO MissingAuthToken
authUser (Just tok) f = do
    ident <- ipc 10 tok
    case ident of
        Just (IPCResult (Just (GithubIdentity gtok))) ->
            f gtok
        _ -> do
            logAuthFailure tok ident
            throwIO BadAuthToken

--

logAuthFailure :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r MinLogLevel) => Token -> Maybe (IPCResult (Maybe Identity)) -> m ()
logAuthFailure tok res = log Log {
    logTime = (),
    logLevel = Info,
    logSource = "HUD.Github.Server.logAuthFailure",
    logTitle = "A user authentication attempt failed",
    logData = M.fromList $ [
        ("token", pack $ show tok),
        ("result", pack $ show res)
    ]
}
