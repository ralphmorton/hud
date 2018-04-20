{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD.Github.Repo.PR (
    PR(..),
    PRState(..)
) where

import HUD.Bridge (Bridge)
import HUD.Data.HUD.Github.Common (Account, PRID, PRNum)
import HUD.Data.HUD.Github.User (AvatarURL)

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
    prUser :: (Account, AvatarURL),
    prHtmlURL :: Text,
    prTitle :: Text,
    prBody :: Maybe Text,
    prState :: PRState,
    prAssignees :: [(Account, AvatarURL)],
    prReviewers :: [(Account, AvatarURL)]
} deriving (Bridge, Show, Generic, FromJSON, ToJSON)

--
--
--

data PRState
    = PROpen
    | PRClosed
    deriving (Bridge, Eq, Show, Generic, FromJSON, ToJSON)
