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
import HUD.Names (Github)

import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Servant

--
--
--

type API = "api" :> "v1" :>
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
                "github" :> ReqBody '[JSON] (OAuthCode Github) :> Post '[JSON] ()
            )
        )
        :<|>
        "hud" :> Header "Authorization" Token :>
        (
            ReqBody '[JSON] HUDReq :> Post '[JSON] HUDRsp
        )
    )
