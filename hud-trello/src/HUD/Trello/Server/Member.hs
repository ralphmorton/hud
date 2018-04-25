{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Trello.Server.Member (
    member
) where

import HUD.Data
import HUD.Data.HUD.Trello
import HUD.Operational
import HUD.Names (Trello)
import HUD.Trello.Types
import HUD.Trello.Server.Request

import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson
import Data.Text (Text)
import Network.HTTP.Types (methodGet)
import UnliftIO (MonadUnliftIO)

--
--
--

member :: (
    MonadUnliftIO m,
    MonadThrow m,
    ContextReader r m,
    HasContext r HttpManager,
    HasContext r TrelloClientKey) => OAuthToken Trello -> m Member
member oauth = do
    m <- trelloReq oauth methodGet "https://api.trello.com/1/members/me" noData
    pure Member {
        mID = mdID m,
        mAvatarURL = mdAvatarURL m,
        mFullName = mdFullName m
    }

--

data MemberData = MemberData {
    mdID :: MemberID,
    mdAvatarURL :: Text,
    mdFullName :: Text
}

instance FromJSON MemberData where
    parseJSON (Object o) = do
        mid <- MemberID <$> o .: "id"
        avatar <- o .: "avatarUrl"
        name <- o .: "fullName"
        pure MemberData {
            mdID = mid,
            mdAvatarURL = avatar,
            mdFullName = name
        }
    parseJSON _ = mzero
