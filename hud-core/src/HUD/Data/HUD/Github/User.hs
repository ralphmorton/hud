{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD.Github.User (
    AvatarURL(..)
) where

import HUD.Bridge (Bridge)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

--
--
--

newtype AvatarURL = AvatarURL {
    unAvatarURL :: Text
} deriving (Bridge, Eq, Show, Generic, FromJSON, ToJSON)
