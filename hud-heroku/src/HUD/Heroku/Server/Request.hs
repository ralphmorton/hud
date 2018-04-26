{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Heroku.Server.Request (
    herokuReq,
    noData
) where

import HUD.Context (viewC)
import HUD.Data hiding (Token)
import HUD.Data.HUD.Heroku
import HUD.Operational
import HUD.Names (Heroku)
import HUD.Heroku.Types

import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Monoid
import Data.String (fromString)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Conduit hiding (http)
import Network.HTTP.Types (Method, methodPost, status200, status401, status403, statusCode)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO)

--
--
--

herokuReq :: (
    MonadUnliftIO m,
    MonadThrow m,
    ContextReader r m,
    HasContext r HttpManager,
    HasContext r HerokuClient,
    ToJSON a,
    FromJSON b) => OAuthToken Heroku -> Method -> Text -> Maybe a -> m b
herokuReq refreshToken mthd url body = do
    tok <- token refreshToken
    ireq <- parseRequest (unpack url)
    rsp <- http ireq {
        method = mthd,
        requestHeaders =
            ("User-Agent", "HUD V1.0") :
            ("Content-Type", "application/json") :
            ("Accept", "application/vnd.heroku+json; version=3") :
            ("Authorization", fromString $ "Bearer " <> unpack tok) :
            requestHeaders ireq,
        requestBody = maybe (RequestBodyLBS "") (RequestBodyLBS . encode) body,
        responseTimeout = responseTimeoutMicro 30000000
    }
    case responseStatus rsp of
        s | s == status200 -> parse (responseBody rsp)
        s | s == status401 -> throwIO HRREAuthFailure
        s | s == status403 -> throwIO HRREAuthFailure
        s -> throwIO (HRREUnknownFailure $ statusCode s)

--

parse :: (MonadUnliftIO m, FromJSON a) => BL.ByteString -> m a
parse raw = maybe (throwIO HRREParseFailure) pure (decode raw)

--
--
--

noData :: Maybe ()
noData = Nothing

--
--
--

token :: (
    MonadUnliftIO m,
    MonadThrow m,
    ContextReader r m,
    HasContext r HttpManager,
    HasContext r HerokuClient) => OAuthToken Heroku -> m Text
token refreshToken = do
    body <- buildReqBody refreshToken
    ireq <- parseRequest "https://id.heroku.com/oauth/token"
    rsp <- http $ urlEncodedBody body ireq {
        method = methodPost,
        requestHeaders =
            ("User-Agent", "HUD V1.0") :
            ("Content-Type", "application/x-www-form-urlencoded") :
            ("Accept", "application/json") :
            requestHeaders ireq,
        responseTimeout = responseTimeoutMicro 30000000
    }
    case decode (responseBody rsp) of
        Just (Token tok) -> pure tok
        _ -> throwIO HRREAuthFailure

--
    
buildReqBody :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r HerokuClient) => OAuthToken Heroku -> m [(B.ByteString, B.ByteString)]
buildReqBody (OAuthToken refreshToken) = do
    HerokuClient _ secret <- viewC
    pure
        [
            ("grant_type", "refresh_token"),
            ("refresh_token", encodeUtf8 refreshToken),
            ("client_secret", encodeUtf8 secret)
        ]
        
--
--
--

newtype Token = Token Text

instance FromJSON Token where
    parseJSON (Object o) = Token <$> o .: "access_token"
    parseJSON _ = mzero
