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

module HUD.Dashboard.API (
    API
) where

import HUD.Data
import HUD.Data.HUD
import HUD.Names (Github, Heroku, Trello)
import HUD.Dashboard.Data (Account, AccountKey, TokenState, UserLevel)

import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Servant

--
--
--

type API = "api" :>
    (
        "auth" :>
        (
            -- Login.
            "login" :> ReqBody '[JSON] (EmailAddress, Text) :> Post '[JSON] (NominalDiffTime, Token)
            :<|>
            "password" :>
            (
                -- Set/reset password.
                Header "Authorization" Token :> "set" :> ReqBody '[JSON] Text :> Post '[JSON] ()
            )
            :<|>
            "oauth" :> Header "Authorization" Token :>
            (
                "state" :> Get '[JSON] TokenState
                :<|>
                "github" :> ReqBody '[JSON] (OAuthCode Github) :> Post '[JSON] ()
                :<|>
                "trello" :> ReqBody '[JSON] (OAuthCode Trello) :> Post '[JSON] ()
                :<|>
                "heroku" :> ReqBody '[JSON] (OAuthCode Heroku) :> Post '[JSON] ()
            )
        )
        :<|>
        "accounts" :> Header "Authorization" Token :>
        (
            Get '[JSON] [((AccountKey, Account), UserLevel)] 
        )
        :<|>
        "hud" :> Header "Authorization" Token :>
        (
            ReqBody '[JSON] Req :> Post '[JSON] Rsp
        )
    )
