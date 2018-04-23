{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD (
    HUDReq(..),
    HUDRsp(..)
) where

import HUD.Bridge (Bridge)
import HUD.Data.HUD.Github (GithubHUDReq, GithubHUDRsp)

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

--
--
--

data HUDReq
    = HRQGithub GithubHUDReq
    deriving (Bridge, Show, Generic, FromJSON, ToJSON)

--
--
--

data HUDRsp
    = HRSGithub GithubHUDRsp
    deriving (Bridge, Show, Generic, FromJSON, ToJSON)
