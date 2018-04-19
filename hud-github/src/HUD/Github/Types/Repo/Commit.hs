{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Github.Types.Repo.Commit (
    Commit(..),
    CommitSHA(..),
    CommitStats(..)
) where

import HUD.Bridge (Bridge)
import HUD.Data (GithubAccount)
import HUD.Github.Types.User (AvatarURL)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

--
--
--

data Commit = Commit {
    cmSHA :: CommitSHA,
    cmURL :: Text,
    cmCommitter :: Maybe (GithubAccount, AvatarURL),
    cmAuthor :: Maybe (GithubAccount, AvatarURL),
    cmStats :: Maybe CommitStats,
    cmMessage :: Text
} deriving (Bridge, Show, Generic, FromJSON, ToJSON)

--
--
--

newtype CommitSHA = CommitSHA {
    unCommitSHA :: Text
} deriving (Bridge, Eq, Show, Generic, FromJSON, ToJSON)

--
--
--

data CommitStats = CommitStats {
    cmsAdditions :: Int,
    cmsDeletions :: Int,
    cmsTotal :: Int
} deriving (Bridge, Show, Generic, FromJSON, ToJSON)
