{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HUD.Identity.Verifier (
    module HUD.Identity.Crypto,
    verifier
) where

import Prelude hiding (log)

import HUD.Data (JWT(jwtPayload))
import HUD.Operational
import HUD.Names ()
import HUD.Logging (MinLogLevel)
import HUD.IPC.Server
import HUD.Identity.Crypto

import UnliftIO (MonadUnliftIO)

--
--
--

verifier :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r MinLogLevel,
    HasContext r HMACKey) => m ()
verifier = serveIPC (fmap (Success . fmap jwtPayload) . decodeToken)
