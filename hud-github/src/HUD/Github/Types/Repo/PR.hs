{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Github.Types.Repo.PR (
    PRDetails(..),
    PR(..),
    PRID(..),
    PRState(..)
) where

import HUD.Bridge (Bridge)
import HUD.Data (GithubAccount, GithubPR)
import HUD.Github.Types.User (AvatarURL)
import HUD.Github.Types.Repo.Commit (Commit)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

--
--
--

data PRDetails = PRDetails {
    prdPR :: PR,
    prdCommits :: [Commit]
} deriving (Bridge, Show, Generic, FromJSON, ToJSON)

--
--
--

data PR = PR {
    prID :: PRID,
    prNumber :: GithubPR,
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

data PRState
    = PROpen
    | PRClosed
    deriving (Bridge, Eq, Show, Generic, FromJSON, ToJSON)
