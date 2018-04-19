{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Identity.Crypto (
    Base64,
    HMACKey(..),
    encodeToken,
    decodeToken
) where

import HUD.Context (viewC)
import HUD.Operational
import HUD.Data (Identity(..), JWT(..), Token(..))

import Control.Monad.Trans (liftIO)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC(..), hmac)
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64.URL as B64
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime)
import UnliftIO (MonadUnliftIO)

--
--
--

type Base64 = B.ByteString

--
--
--

newtype HMACKey = HMACKey {
    unHMACKey :: Base64
}

--
--
--

encodeToken :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r HMACKey) => Identity -> NominalDiffTime -> m Token
encodeToken ident ttl = do
    key <- viewC
    now <- liftIO getCurrentTime
    pure $ encodeJWT key JWT {
        jwtISS = "hud",
        jwtSUB = "identity",
        jwtAUD = "hud",
        jwtEXP = addUTCTime ttl now,
        jwtIAT = now,
        jwtPayload = ident
    }

--
--
--

decodeToken :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r HMACKey) => Token -> m (Maybe (JWT Identity))
decodeToken tok = do
    key <- viewC
    now <- liftIO getCurrentTime
    let mJWT = decodeJWT key tok
    pure $ do
        jwt <- mJWT
        case now < jwtEXP jwt of
            True -> pure jwt
            False -> Nothing

--
--
--

encodeJWT :: ToJSON a => HMACKey -> JWT a -> Token
encodeJWT key jwt = Token (decodeUtf8 str)
    where
    header = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9"
    payload = B.filter (/='=') $ B64.encode (BL.toStrict $ encode jwt)
    sig = hmac' key (header <> "." <> payload)
    str = header <> "." <> payload <> "." <> sig

--
--
--

decodeJWT :: FromJSON a => HMACKey -> Token -> Maybe (JWT a)
decodeJWT key (Token tok) = case T.splitOn "." tok of
    ["eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9", body, sig] -> do
        let sig' = hmac' key (encodeUtf8 $ "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9" <> "." <> body)
        case decodeUtf8 sig' == sig of
            False -> Nothing
            True -> (decodeStrict . B64.decodeLenient) (encodeUtf8 body)
    _ -> Nothing

--
--
--

hmac' :: HMACKey -> B.ByteString -> B.ByteString
hmac' (HMACKey key) message = B.takeWhile (/='=') b64
    where
    secret = B64.decodeLenient key
    b64 = B64.encode b16
    b16 = fst (B16.decode bs)
    bs = B.pack (show digest)
    digest = hmacGetDigest (hmac secret message :: HMAC SHA256)
