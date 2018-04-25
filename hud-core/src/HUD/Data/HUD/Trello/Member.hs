{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD.Trello.Member (
    MemberID(..),
    Member(..)
) where

import HUD.Bridge (Bridge)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

--
--
--

newtype MemberID = MemberID {
    unMemberID :: Text
} deriving (Bridge, Eq, Ord, Show, Generic, FromJSON, ToJSON)

--
--
--

data Member = Member {
    mID :: MemberID,
    mAvatarURL :: Text,
    mFullName :: Text
} deriving (Bridge, Eq, Ord, Show, Generic, FromJSON, ToJSON)
