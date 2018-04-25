{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD.Trello.Board (
    Board(..),
    BoardOverview(..)
) where

import HUD.Bridge (Bridge)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

--
--
--

newtype Board = Board {
    unBoard :: Text
} deriving (Bridge, Eq, Ord, Show, Generic, FromJSON, ToJSON)

--
--
--

data BoardOverview = BoardOverview {
    boName :: Text
} deriving (Bridge, Show, Generic, FromJSON, ToJSON)
