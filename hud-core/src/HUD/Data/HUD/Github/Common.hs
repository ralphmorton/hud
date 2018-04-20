{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD.Github.Common (
    Account(..),
    Repo(..),
    PRNum(..),
    PRID(..)
) where

import HUD.Bridge (Bridge)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

--
--
--

newtype Account = Account {
    unAccount :: Text
} deriving (Bridge, Eq, Ord, Show, Generic, FromJSON, ToJSON)

--
--
--

newtype Repo = Repo {
    unRepo :: Text
} deriving (Bridge, Eq, Ord, Show, Generic, FromJSON, ToJSON)

--
--
--

newtype PRNum = PRNum {
    unPRNum :: Int
} deriving (Bridge, Eq, Ord, Show, Generic, FromJSON, ToJSON)

--
--
--

newtype PRID = PRID {
    unPRID :: Int
} deriving (Bridge, Eq, Ord, Show, Generic, FromJSON, ToJSON)
