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

import Servant

--
--
--

type API = Header "Authorization" Token :> "api" :> "v1" :>
    (
        "hud" :>
        (
            ReqBody '[JSON] HUDReq :> Post '[JSON] HUDRsp
        )
    )
