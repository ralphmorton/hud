{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HUD.Data.Common (
    EmailAddress(..),
    PasswordHash(..)
) where

import HUD.Bridge (Bridge)

import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Database.Persist (PersistField(..), PersistValue(..))
import Database.Persist.Sql (PersistFieldSql(..), SqlType(..))
import GHC.Generics (Generic)
import Web.PathPieces (PathPiece(..))
import Web.HttpApiData (ToHttpApiData(..), FromHttpApiData(..))

--
--
--

-- TODO: custom validated instances?

-- | An email address.
newtype EmailAddress = EmailAddress {
    unEmailAddress :: Text
} deriving (Bridge, Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance PersistField EmailAddress where
    toPersistValue = PersistText . unEmailAddress
    fromPersistValue (PersistText t) = pure (EmailAddress t)
    fromPersistValue v = Left $ "Not a valid EmailAddress value: " <> pack (show v)

instance PersistFieldSql EmailAddress where
    sqlType _ = SqlString

instance PathPiece EmailAddress where
    toPathPiece = unEmailAddress
    fromPathPiece = Just . EmailAddress

instance ToHttpApiData EmailAddress where
    toUrlPiece = unEmailAddress

instance FromHttpApiData EmailAddress where
    parseUrlPiece = Right . EmailAddress

-- | A password hash
newtype PasswordHash = PasswordHash {
    unPasswordHash :: Text
} deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance PersistField PasswordHash where
    toPersistValue = PersistText . unPasswordHash
    fromPersistValue (PersistText t) = pure (PasswordHash t)
    fromPersistValue v = Left $ "Not a valid PasswordHash value: " <> pack (show v)

instance PersistFieldSql PasswordHash where
    sqlType _ = SqlString

instance PathPiece PasswordHash where
    toPathPiece = unPasswordHash
    fromPathPiece = Just . PasswordHash

instance ToHttpApiData PasswordHash where
    toUrlPiece = unPasswordHash

instance FromHttpApiData PasswordHash where
    parseUrlPiece = Right . PasswordHash
