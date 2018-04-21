{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Identity.Server.Common (
    ResponseException(..),
    tokenTTL,
    genConfirmCode,
    getConfirmCodeData
) where

import HUD.Operational

import Control.Exception (Exception)
import Control.Monad (replicateM)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (NominalDiffTime)
import Database.Redis (Redis, Reply, get, sendRequest)
import System.Random (randomRIO)
import UnliftIO (MonadUnliftIO)

--
--
--

data ResponseException
    = BadEmailToken
    deriving Show

instance Exception ResponseException

--
--
--

confirmCodeTTL :: NominalDiffTime
confirmCodeTTL = 14400 -- 4 hours

--
--
--

tokenTTL :: NominalDiffTime
tokenTTL = 28800 -- 8 hours

--
--
--

genConfirmCode :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r RedisPool) => Text -> m Text
genConfirmCode dta = do
    let ttlSecs = (B.pack . show) (floor confirmCodeTTL :: Int)
    code <- fmap pack . liftIO . replicateM 8 $ randomRIO ('A','Z')
    res <- redis (sendRequest ["SET", encodeUtf8 code, encodeUtf8 dta, "EX", ttlSecs, "NX"] :: Redis (Either Reply B.ByteString))
    either (const $ genConfirmCode dta) (const $ pure code) res

--
--
--

getConfirmCodeData :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r RedisPool) => Text -> m (Maybe Text)
getConfirmCodeData code = do
    let canonical = B.map toUpper (encodeUtf8 code)
    res <- redis (get canonical)
    case res of
        Right (Just v) -> (pure . Just) (decodeUtf8 v)
        _ -> pure Nothing
