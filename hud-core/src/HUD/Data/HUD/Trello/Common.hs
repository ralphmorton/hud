{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD.Trello.Common (
    Member(..),
    Board(..)
) where

import HUD.Bridge (Bridge)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

--
--
--

newtype Member = Member {
    unMember :: Text
} deriving (Bridge, Eq, Ord, Show, Generic, FromJSON, ToJSON)

--
--
--

newtype Board = Board {
    unBoard :: Text
} deriving (Bridge, Eq, Ord, Show, Generic, FromJSON, ToJSON)
