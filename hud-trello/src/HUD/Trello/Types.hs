{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Trello.Types (
    TrelloClientKey(..)
) where

import HUD.Bridge (Bridge)

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)

--
--
--

newtype TrelloClientKey = TrelloClientKey {
    unTrelloClientKey :: Text
} deriving (Bridge, Show, Generic, FromJSON, ToJSON)
