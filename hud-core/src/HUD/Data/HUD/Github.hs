{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD.Github (
    GithubHUD(..),
    GithubAccount(..),
    GithubRepo(..)
) where

import HUD.Bridge (Bridge)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

--
--
--

data GithubHUD
    = RepoOverview GithubAccount GithubRepo
    deriving (Bridge, Eq, Ord, Show, Generic, FromJSON, ToJSON)

--
--
--

newtype GithubAccount = GithubAccount {
    unGithubAccount :: Text
} deriving (Bridge, Eq, Ord, Show, Generic, FromJSON, ToJSON)

--
--
--

newtype GithubRepo = GithubRepo {
    unGithubRepo :: Text
} deriving (Bridge, Eq, Ord, Show, Generic, FromJSON, ToJSON)