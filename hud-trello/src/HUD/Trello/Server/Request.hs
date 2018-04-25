{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Trello.Server.Request (
    trelloReq,
    noData
) where

import HUD.Context (viewC)
import HUD.Data
import HUD.Data.HUD.Trello
import HUD.Operational
import HUD.Names (Trello)
import HUD.Trello.Types

import Control.Monad.Catch (MonadThrow)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Monoid
import Data.Text (Text, unpack)
import Network.HTTP.Conduit hiding (http)
import Network.HTTP.Types (Method, status200, status401, status403, statusCode)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO)

--
--
--

trelloReq :: (
    MonadUnliftIO m,
    MonadThrow m,
    ContextReader r m,
    HasContext r HttpManager,
    HasContext r TrelloClientKey,
    ToJSON a,
    FromJSON b) => OAuthToken Trello -> Method -> Text -> Maybe a -> m b
trelloReq (OAuthToken tok) mthd url body = do
    TrelloClientKey key <- viewC
    ireq <- parseRequest (unpack url <> "?key=" <> unpack key <> "&token=" <> unpack tok)
    rsp <- http ireq {
        method = mthd,
        requestHeaders =
            ("User-Agent", "HUD V1.0") :
            ("Content-Type", "application/json") :
            ("Accept", "application/json") :
            requestHeaders ireq,
        requestBody = maybe (RequestBodyLBS "") (RequestBodyLBS . encode) body,
        responseTimeout = responseTimeoutMicro 30000000
    }
    case responseStatus rsp of
        s | s == status200 -> parse (responseBody rsp)
        s | s == status401 -> throwIO TRREAuthFailure
        s | s == status403 -> throwIO TRREAuthFailure
        s -> throwIO (TRREUnknownFailure $ statusCode s)

--

parse :: (MonadUnliftIO m, FromJSON a) => ByteString -> m a
parse raw = maybe (throwIO TRREParseFailure) pure (decode raw)

--
--
--

noData :: Maybe ()
noData = Nothing
