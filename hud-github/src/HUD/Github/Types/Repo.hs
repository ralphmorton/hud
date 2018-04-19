{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Github.Types.Repo (
    RepoOverview(..)
) where

import HUD.Bridge (Bridge)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

--
--
--

data RepoOverview = RepoOverview {
    roDescription :: Maybe Text,
    roCreatedAt :: Maybe UTCTime,
    roHtmlUrl :: Text
} deriving (Bridge, Eq, Ord, Show, Generic, FromJSON, ToJSON)
