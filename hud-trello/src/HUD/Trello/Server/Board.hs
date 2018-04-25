{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Trello.Server.Board (
    boards,
    boardOverview
) where

import HUD.Data
import HUD.Data.HUD.Trello
import HUD.Operational
import HUD.Names (Trello)
import HUD.Trello.Types
import HUD.Trello.Server.Request
import HUD.Trello.Server.Member

import Control.Arrow ((&&&))
import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Network.HTTP.Types (methodGet)
import UnliftIO (MonadUnliftIO)

--
--
--

boards :: (
    MonadUnliftIO m,
    MonadThrow m,
    ContextReader r m,
    HasContext r HttpManager,
    HasContext r TrelloClientKey) => OAuthToken Trello -> m [(Board, Text)]
boards oauth = do
    m <- member oauth
    let url = "https://api.trello.com/1/members/" <> unMemberID (mID m) <> "/boards"
    fmap (bdID &&& bdName) <$> trelloReq oauth methodGet url noData

--
--
--

boardOverview :: (
    MonadUnliftIO m,
    MonadThrow m,
    ContextReader r m,
    HasContext r HttpManager,
    HasContext r TrelloClientKey) => OAuthToken Trello -> Board -> m BoardOverview
boardOverview oauth (Board bid) = do
    let url = "https://api.trello.com/1/boards/" <> bid
    b <- trelloReq oauth methodGet url noData
    pure BoardOverview {
        boName = bdName b
    }

--

data BoardData = BoardData {
    bdID :: Board,
    bdName :: Text
}

instance FromJSON BoardData where
    parseJSON (Object o) = do
        bid <- Board <$> o .: "id"
        name <- o .: "name"
        pure BoardData {
            bdID = bid,
            bdName = name
        }
    parseJSON _ = mzero
