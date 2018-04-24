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

module HUD.Dashboard.Server (
    server
) where

import Prelude hiding (log)

import HUD.Context (Context)
import HUD.Data
import HUD.Operational
import HUD.Logging
import HUD.IPC.Client
import HUD.Dashboard.API
import HUD.Dashboard.Data
import HUD.Dashboard.Persistence
import HUD.Dashboard.Server.Auth
import HUD.Dashboard.Server.OAuth
import HUD.Dashboard.Server.HUD

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as M
import Data.Proxy (Proxy)
import Data.Text (pack)
import Servant hiding (BadPassword, Context)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (handle, throwIO)

--
--
--

server :: (
    HasContext r AmqpPool,
    HasContext r SqlPool,
    HasContext r MinLogLevel) => Context r -> Server API
server ctx = hoistServer (Proxy :: Proxy API) nat server'
    where
    nat f = do
        res <- liftIO $ handle (pure . Left) (Right <$> runReaderT f ctx)
        case res of
            Left e -> (throwError . errorCtor e) (encode e)
            Right r -> pure r

errorCtor :: HandlerException -> ByteString -> ServantErr
errorCtor MissingAuthToken s = err401 { errBody = s }
errorCtor BadAuthToken s = err401 { errBody = s }
errorCtor BadPassword s = err403 { errBody = s }
errorCtor UnknownUser s = err403 { errBody = s }
errorCtor IllegalPassword s = err400 { errBody = s }
errorCtor NotFound s = err404 { errBody = s }
errorCtor InternalFailure s = err500 { errBody = s }
errorCtor MissingGithubToken s = err400 { errBody = s }

--

server' :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r SqlPool,
    HasContext r MinLogLevel) => ServerT API m
server' =
    (
        uncurry login
        :<|>
        (\tok pw -> authUser tok $ flip setPassword pw . fst)
        :<|>
        (\tok ->
            authUser tok . flip authGithub
        )
    )
    :<|>
    (\tok ->
        authUser tok (sql . getUserAccounts . fst)
    )
    :<|>
    (\tok ->
        (
            authUser tok . flip fetchHUD
        )
    )

--

authUser :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r SqlPool,
    HasContext r MinLogLevel) => Maybe Token -> ((UserKey, User) -> m a) -> m a
authUser Nothing _ = throwIO MissingAuthToken
authUser (Just tok) f = do
    ident <- ipc 10 tok
    case ident of
        Just (IPCResult (Just (EmailIdentity addr))) ->
            maybe (throwIO UnknownUser) f =<< (sql . runMaybeT) (getUserByEmail addr)
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
    logSource = "HUD.Dashboard.Server.logAuthFailure",
    logTitle = "A user authentication attempt failed",
    logData = M.fromList $ [
        ("token", pack $ show tok),
        ("result", pack $ show res)
    ]
}
