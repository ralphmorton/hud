{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HUD.Dashboard.Data.Relational (
    Account(..),
    AccountKey(..),
    User(..),
    UserKey(..),
    UserLevel(..)
) where

import HUD.Bridge
import HUD.Data
import HUD.Names (Github)

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

-- | Account key.
newtype AccountKey = AccountKey {
    unAccountKey :: Text
} deriving (Bridge, Eq, Ord, Read, Show, Generic, FromJSON, ToJSON)

instance PersistField AccountKey where
    toPersistValue = PersistText . unAccountKey
    fromPersistValue (PersistText t) = pure (AccountKey t)
    fromPersistValue v = Left $ "Not a valid AccountKey value: " <> pack (show v)

instance PersistFieldSql AccountKey where
    sqlType _ = SqlString

instance PathPiece AccountKey where
    toPathPiece = unAccountKey
    fromPathPiece = Just . AccountKey

instance ToHttpApiData AccountKey where
    toUrlPiece = unAccountKey

instance FromHttpApiData AccountKey where
    parseUrlPiece = Right . AccountKey

-- | An account.
data Account = Account {
    accountName :: Text
} deriving (Bridge, Show, Generic, FromJSON, ToJSON)

--
--
--

-- | User key.
newtype UserKey = UserKey {
    unUserKey :: Text
} deriving (Bridge, Eq, Ord, Read, Show, Generic, FromJSON, ToJSON)

instance PersistField UserKey where
    toPersistValue = PersistText . unUserKey
    fromPersistValue (PersistText t) = pure (UserKey t)
    fromPersistValue v = Left $ "Not a valid UserKey value: " <> pack (show v)

instance PersistFieldSql UserKey where
    sqlType _ = SqlString

instance PathPiece UserKey where
    toPathPiece = unUserKey
    fromPathPiece = Just . UserKey

instance ToHttpApiData UserKey where
    toUrlPiece = unUserKey

instance FromHttpApiData UserKey where
    parseUrlPiece = Right . UserKey

-- | A user.
data User = User {
    userName :: Text,
    userEmail :: EmailAddress,
    userPassword :: Maybe PasswordHash,
    userGithubToken :: Maybe (OAuthToken Github)
} deriving (Show, Generic, FromJSON, ToJSON)

--
--
--

-- | User level.
data UserLevel
    = Owner
    | Unprivileged
    deriving (Bridge, Eq, Ord, Read, Show, Generic, FromJSON, ToJSON)

instance PersistField UserLevel where
    toPersistValue Owner = PersistText "owner"
    toPersistValue Unprivileged = PersistText "unprivileged"
    fromPersistValue (PersistText "owner") = pure Owner
    fromPersistValue (PersistText "unprivileged") = pure Unprivileged
    fromPersistValue v = Left $ "Not a valid UserLevel value: " <> pack (show v)

instance PersistFieldSql UserLevel where
    sqlType _ = SqlString
