{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD.Trello.Board (
    BoardOverview(..)
) where

import HUD.Bridge (Bridge)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

--
--
--

data BoardOverview = BoardOverview {
    boName :: Text
} deriving (Bridge, Show, Generic, FromJSON, ToJSON)
