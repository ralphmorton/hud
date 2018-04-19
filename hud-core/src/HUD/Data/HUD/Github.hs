{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD.Github (
    GithubHUD(..),
    GithubAccount(..),
    GithubRepo(..),
    GithubPR(..)
) where

import HUD.Bridge (Bridge)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

--
--
--

data GithubHUD
    = GHHRepoPRs GithubAccount GithubRepo
    | GHHRepoPR GithubAccount GithubRepo GithubPR
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

--
--
--

newtype GithubPR = GithubPR {
    unGithubPR :: Int
} deriving (Bridge, Eq, Ord, Show, Generic, FromJSON, ToJSON)
