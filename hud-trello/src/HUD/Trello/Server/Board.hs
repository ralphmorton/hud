{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Trello.Server.Board (
    boardOverview
) where

import HUD.Context (viewC)
import HUD.Data
import HUD.Data.HUD.Trello
import HUD.Operational
import HUD.Names (Trello)
import HUD.Trello.Types

import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Monoid
import Data.Text (Text, unpack)
import Network.HTTP.Conduit hiding (Request, http)
import Network.HTTP.Types (methodGet, status200, status401, status403, statusCode)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO)

--
--
--

boardOverview :: (
    MonadUnliftIO m,
    MonadThrow m,
    ContextReader r m,
    HasContext r HttpManager,
    HasContext r TrelloClientKey) => OAuthToken Trello -> Board -> m BoardOverview
boardOverview (OAuthToken tok) (Board bid) = do
    TrelloClientKey key <- viewC
    let url = "https://api.trello.com/1/boards/" <> unpack bid <> "?key=" <> unpack key <> "&token=" <> unpack tok
    ireq <- parseRequest url
    rsp <- http ireq {
        method = methodGet,
        requestHeaders =
            ("User-Agent", "HUD V1.0") :
            ("Content-Type", "application/json") :
            ("Accept", "application/json") :
            requestHeaders ireq,
        responseTimeout = responseTimeoutMicro 30000000
    }
    case responseStatus rsp of
        s | s == status200 -> parseBoardOverview (responseBody rsp)
        s | s == status401 -> throwIO TRREAuthFailure
        s | s == status403 -> throwIO TRREAuthFailure
        s -> throwIO (TRREUnknownFailure $ statusCode s)

--

parseBoardOverview :: MonadUnliftIO m => ByteString -> m BoardOverview
parseBoardOverview raw = maybe (throwIO TRREParseFailure) pure $ do
    b <- decode raw
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
