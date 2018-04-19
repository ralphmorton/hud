{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HUD.Email.Send (
    send
) where

import Prelude hiding (log)

import HUD.Operational
import HUD.Names ()
import HUD.Logging (MinLogLevel)
import HUD.IPC.Server
import HUD.Email.Data

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans (liftIO)
import Data.Time (getCurrentTime)
import UnliftIO (MonadUnliftIO)

--
--
--

send :: (
    MonadThrow m,
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r MongoPool,
    HasContext r HttpManager,
    HasContext r SendGridCreds,
    HasContext r MinLogLevel) => m ()
send = serveIPC sendEmail

--
--
--

sendEmail :: (
    MonadThrow m,
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r MongoPool,
    HasContext r HttpManager,
    HasContext r SendGridCreds,
    HasContext r MinLogLevel) => Email -> m (Result ())
sendEmail eml = do
    now <- liftIO getCurrentTime
    email eml
    insertEmail (SentEmail now eml)
    pure (Success ())
