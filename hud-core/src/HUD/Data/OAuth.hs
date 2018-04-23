{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Data.OAuth (
    OAuthCode(..),
    OAuthResult(..),
    OAuthToken(..)
) where

import HUD.Bridge (Bridge)

import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

--
--
--

newtype OAuthCode a = OAuthCode {
    unOAuthCode :: Text
} deriving (Bridge, Eq, Show, Generic, FromJSON, ToJSON)

--
--
--

data OAuthResult a
    = ORSuccess (OAuthToken a)
    | ORFailure
    deriving (Show, Generic, FromJSON, ToJSON)

--
--
--

newtype OAuthToken a = OAuthToken {
    unOAuthToken :: Text
} deriving (Eq, Show, Generic)

instance ToJSON (OAuthToken a) where
    toJSON (OAuthToken tok) = object ["access_token" .= tok]

instance FromJSON (OAuthToken a) where
    parseJSON (Object o) = OAuthToken <$> o .: "access_token"
    parseJSON _ = mzero
