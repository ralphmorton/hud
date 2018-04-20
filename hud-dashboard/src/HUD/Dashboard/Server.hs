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
import HUD.Data.HUD
import HUD.Logging
import HUD.Operational
import HUD.Names ()
import HUD.IPC.Client (IPCResult(IPCResult), ipc)
import HUD.Dashboard.API

import Control.Exception (Exception)
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Proxy (Proxy)
import Servant hiding (BadPassword, Context)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (handleAny, throwIO)
import Debug.Trace

data HandlerException
    = NoAuth
    | BadAuth
    | Failed
    deriving Show

instance Exception HandlerException

--
--
--

server :: (
    HasContext r AmqpPool,
    HasContext r MinLogLevel) => Context r -> Server API
server ctx = hoistServer (Proxy :: Proxy API) nat server'
    where
    nat f = do
        res <- liftIO $ handleAny (pure . Left) (Right <$> runReaderT f ctx)
        case res of
            Right r -> pure r
            Left e -> throwError err500 {
                    errBody = BL.pack (show e)
                }

--

server' :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r MinLogLevel) => ServerT API m
server' tok =
    (
        (
            withGithub tok . flip handleHUDReq
        )
    )

--

withGithub :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r MinLogLevel) => Maybe Token -> (GithubToken -> m a) -> m a
withGithub Nothing _ = throwIO NoAuth
withGithub (Just tok) f = do
    ident <- ipc 10 tok
    case ident of
        Just (IPCResult (Just (GithubIdentity gtok))) -> f gtok
        _ -> throwIO BadAuth

--

handleHUDReq :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r MinLogLevel) => GithubToken -> HUDReq -> m HUDRsp
handleHUDReq tok (HRQGithub greq) = do
    res <- ipc 30 (tok, greq)
    case res of
        Just (IPCResult rsp) -> pure (HRSGithub rsp)
        _ -> trace (show res) $ throwIO Failed
