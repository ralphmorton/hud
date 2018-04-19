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

module HUD.Identity.Server (
    SenderAddr(..),
    API,
    server
) where

import HUD.Context (Context)
import HUD.Data
import HUD.Operational
import HUD.Identity.Crypto
import HUD.Identity.Server.Common
import HUD.Identity.Server.Email

import Control.Exception (handle)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Proxy
import Data.Text (Text)
import Servant hiding (Context)
import UnliftIO (MonadUnliftIO)

--
--
--

type API = "api" :> "v1" :>
    (
        "email" :>
        (
            "identify" :> ReqBody '[JSON] EmailAddress :> Post '[JSON] ()
            :<|>
            "confirm" :> ReqBody '[JSON] (EmailAddress, Text) :> Post '[JSON] Token
        )
    )

--
--
--

server :: (
    HasContext r AmqpPool,
    HasContext r RedisPool,
    HasContext r HttpManager,
    HasContext r HMACKey,
    HasContext r SenderAddr) => Context r -> Server API
server ctx = hoistServer (Proxy :: Proxy API) nat server'
    where
    nat f = do
        res <- liftIO $ handle (pure . Left) (Right <$> runReaderT f ctx)
        either throwRE pure res

throwRE :: ResponseException -> Handler a
throwRE BadEmailToken = throwError err400 {
    errBody = "Invalid email token"
}

--

server' :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r RedisPool,
    HasContext r HttpManager,
    HasContext r HMACKey,
    HasContext r SenderAddr) => ServerT API m
server' =
    (
        identifyEmail
        :<|>
        uncurry confirmEmailIdentity
    )
