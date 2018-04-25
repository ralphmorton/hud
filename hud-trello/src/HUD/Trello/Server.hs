{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Trello.Server (
    TrelloClientKey(..),
    serve
) where

import Prelude hiding (log)

import HUD.Data (OAuthToken, Log'(..), LogLevel(..))
import HUD.Data.HUD.Trello
import HUD.Operational
import HUD.Names (Trello)
import HUD.Logging
import HUD.IPC.Server
import HUD.Trello.Types
import HUD.Trello.Server.Board

import Control.Monad.Catch (MonadThrow)
import qualified Data.Map as M
import Data.Text (pack)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (catch)

--
--
--

serve :: (
    MonadUnliftIO m,
    MonadThrow m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r HttpManager,
    HasContext r MinLogLevel,
    HasContext r TrelloClientKey) => m ()
serve = serveIPC (uncurry request)

--
--
--

request :: (
    MonadUnliftIO m,
    MonadThrow m,
    ContextReader r m,
    HasContext r HttpManager,
    HasContext r MinLogLevel,
    HasContext r TrelloClientKey) => OAuthToken Trello -> TrelloHUDReq -> m (Result TrelloHUDRsp)
request tok req = catch (Success <$> runRequest tok req) $ \e -> do
    logRequestException req e
    (pure . Success) (TRHRSFailure e)

--
--
--

runRequest :: (
    MonadUnliftIO m,
    MonadThrow m,
    ContextReader r m,
    HasContext r HttpManager,
    HasContext r TrelloClientKey) => OAuthToken Trello -> TrelloHUDReq -> m TrelloHUDRsp
runRequest tok (TRHRQBoardOverview board) =
    TRHRSBoardOverview <$> boardOverview tok board

--
--
--

logRequestException :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r MinLogLevel) => TrelloHUDReq -> TrelloRequestException -> m ()
logRequestException req e = log Log {
    logTime = (),
    logLevel = Error,
    logSource = "HUD.Trello.Server.logRequestException",
    logTitle = "Trello request failed",
    logData = M.fromList $ [
        ("request", (pack . show) req),
        ("exception", (pack . show) e)
    ]
}
