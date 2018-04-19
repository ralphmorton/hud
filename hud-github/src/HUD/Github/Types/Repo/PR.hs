{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Github.Types.Repo.PR (
    PR(..),
    PRID(..),
    PRNum(..),
    PRState(..)
) where

import HUD.Bridge (Bridge)
import HUD.Data (GithubAccount)
import HUD.Github.Types.User (AvatarURL)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

--
--
--

data PR = PR {
    prID :: PRID,
    prNumber :: PRNum,
    prCreatedAt :: UTCTime,
    prUpdatedAt :: UTCTime,
    prClosedAt :: Maybe UTCTime,
    prMergedAt :: Maybe UTCTime,
    prUser :: (GithubAccount, AvatarURL),
    prHtmlURL :: Text,
    prTitle :: Text,
    prBody :: Maybe Text,
    prState :: PRState,
    prAssignees :: [(GithubAccount, AvatarURL)],
    prReviewers :: [(GithubAccount, AvatarURL)]
} deriving (Bridge, Show, Generic, FromJSON, ToJSON)

--
--
--

newtype PRID = PRID {
    unPRID :: Int
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

data PRState
    = PROpen
    | PRClosed
    deriving (Bridge, Eq, Show, Generic, FromJSON, ToJSON)
