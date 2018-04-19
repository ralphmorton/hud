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
    GithubClient(..),
    API,
    server
) where

import HUD.Context (Context)
import HUD.Data
import HUD.Operational
import HUD.Identity.Crypto
import HUD.Identity.Server.Common
import HUD.Identity.Server.Email
import HUD.Identity.Server.Github

import Control.Exception (handle)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Monoid ((<>))
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
        :<|>
        "github" :>
        (
            "authorise" :> ReqBody '[JSON] Text :> Post '[JSON] Token
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
    HasContext r SenderAddr,
    HasContext r GithubClient) => Context r -> Server API
server ctx = hoistServer (Proxy :: Proxy API) nat server'
    where
    nat f = do
        res <- liftIO $ handle (pure . Left) (Right <$> runReaderT f ctx)
        either throwRE pure res

throwRE :: ResponseException -> Handler a
throwRE BadEmailToken =
    throwError err400 {
        errBody = "Invalid email token"
    }
throwRE (GithubAuthorisationFailed body) =
    throwError err400 {
        errBody = "Github authorisation failed: " <> body
    }

--

server' :: (
    MonadUnliftIO m,
    MonadThrow m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r RedisPool,
    HasContext r HttpManager,
    HasContext r HMACKey,
    HasContext r SenderAddr,
    HasContext r GithubClient) => ServerT API m
server' =
    (
        identifyEmail
        :<|>
        uncurry confirmEmailIdentity
    )
    :<|>
    (
        authoriseGithub
    )
