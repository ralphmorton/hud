{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HUD.Identity.Signer (
    module HUD.Identity.Crypto,
    signer
) where

import Prelude hiding (log)

import HUD.Operational
import HUD.Names ()
import HUD.Logging (MinLogLevel)
import HUD.IPC.Server
import HUD.Identity.Crypto

import UnliftIO (MonadUnliftIO)

--
--
--

signer :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r MinLogLevel,
    HasContext r HMACKey) => m ()
signer = serveIPC (fmap Success . uncurry encodeToken)
