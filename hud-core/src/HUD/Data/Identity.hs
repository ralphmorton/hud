{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Data.Identity (
    Token(..),
    JWT(..),
    Identity(..)
) where

import HUD.Bridge
import HUD.Data.Common

import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import Web.PathPieces (PathPiece(..))
import Web.HttpApiData (ToHttpApiData(..), FromHttpApiData(..))

--
--
--

newtype Token = Token {
    unToken :: Text
} deriving (Bridge, Eq, Show, Generic, FromJSON, ToJSON)

instance PathPiece Token where
    toPathPiece = unToken
    fromPathPiece = Just . Token

instance ToHttpApiData Token where
    toUrlPiece = unToken

instance FromHttpApiData Token where
    parseUrlPiece = Right . Token

--
--
--

data JWT a = JWT {
    jwtISS :: Text,
    jwtSUB :: Text,
    jwtAUD :: Text,
    jwtEXP :: UTCTime,
    jwtIAT :: UTCTime,
    jwtPayload :: a
} deriving (Eq, Show)

instance ToJSON a => ToJSON (JWT a) where
    toJSON j =
        object [
            "iss" .= jwtISS j,
            "sub" .= jwtSUB j,
            "aud" .= jwtAUD j,
            "exp" .= utcTimeToInt (jwtEXP j),
            "iat" .= utcTimeToInt (jwtIAT j),
            "payload" .= jwtPayload j
        ]

utcTimeToInt :: UTCTime -> Int
utcTimeToInt = floor . utcTimeToPOSIXSeconds

instance FromJSON a => FromJSON (JWT a) where
    parseJSON (Object v) = do
        iss <- v .: "iss"
        sub' <- v .: "sub"
        aud <- v .: "aud"
        exp' <- intToUTCTime <$> v .: "exp"
        iat <- intToUTCTime <$> v .: "iat"
        payload <- v .: "payload"
        pure JWT {
            jwtISS = iss,
            jwtSUB = sub',
            jwtAUD = aud,
            jwtEXP = exp',
            jwtIAT = iat,
            jwtPayload = payload
        }
    parseJSON _ = mzero

intToUTCTime :: Int -> UTCTime
intToUTCTime = posixSecondsToUTCTime . fromIntegral

--
--
--

data Identity
    = EmailIdentity EmailAddress
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
