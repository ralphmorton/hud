{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD (
    HUDReq(..),
    HUDRsp(..)
) where

import HUD.Bridge (Bridge)
import qualified HUD.Data.HUD.Github as GH

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

--
--
--

data HUDReq
    = HRQGithub GH.HUDReq
    deriving (Bridge, Show, Generic, FromJSON, ToJSON)

--
--
--

data HUDRsp
    = HRSGithub GH.HUDRsp
    deriving (Bridge, Show, Generic, FromJSON, ToJSON)
