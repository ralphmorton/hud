{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD.Github.Repo.Comment (
    IssueCommentID(..),
    IssueComment(..),
    CommentID(..),
    Comment(..)
) where

import HUD.Bridge (Bridge)
import HUD.Data.HUD.Github.Common (Account)
import HUD.Data.HUD.Github.User (AvatarURL)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

--
--
--

newtype IssueCommentID = IssueCommentID {
    unIssueCommentID :: Int
} deriving (Bridge, Eq, Ord, Show, Generic, FromJSON, ToJSON)

--
--
--

data IssueComment = IssueComment {
    icoID :: IssueCommentID,
    icoCreatedAt :: UTCTime,
    icoUpdatedAt :: UTCTime,
    icoURL :: Text,
    icoHtmlURL :: Text,
    icoUser :: (Account, AvatarURL),
    icoBody :: Text
} deriving (Bridge, Show, Generic, FromJSON, ToJSON)

--
--
--

newtype CommentID = CommentID {
    unCommentID :: Int
} deriving (Bridge, Eq, Ord, Show, Generic, FromJSON, ToJSON)

--
--
--

data Comment = Comment {
    coID :: CommentID,
    coPosition :: Maybe Int,
    coLine :: Maybe Int,
    coCreatedAt :: Maybe UTCTime,
    coUpdatedAt :: UTCTime,
    coURL :: Text,
    coHtmlURL :: Maybe Text,
    coUser :: (Account, AvatarURL),
    coBody :: Text
} deriving (Bridge, Show, Generic, FromJSON, ToJSON)
