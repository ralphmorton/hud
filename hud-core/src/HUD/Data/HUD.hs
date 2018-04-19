{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD (
    module HUD.Data.HUD.Github,
    HUD(..)
) where

import HUD.Bridge (Bridge)
import HUD.Data.HUD.Github

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

--
--
--

data HUD
    = GithubHUD GithubHUD
    deriving (Bridge, Eq, Ord, Show, Generic, FromJSON, ToJSON)
