{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD.Heroku.Organisation (
    OrganisationID(..),
    OrganisationType(..),
    OrganisationRole(..),
    Organisation(..)
) where

import HUD.Bridge (Bridge)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

--
--
--

newtype OrganisationID = OrganisationID {
    unOrganisationID :: Text
} deriving (Bridge, Eq, Show, Generic, FromJSON, ToJSON)

--
--
--

data OrganisationType
    = OTEnterprise
    | OTTeam
    | OTUnknown
    deriving (Bridge, Eq, Show, Generic, FromJSON, ToJSON)

--
--
--

data OrganisationRole
    = OROwner
    | ORAdmin
    | ORCollaborator
    | ORMember
    | ORNone
    deriving (Bridge, Eq, Show, Generic, FromJSON, ToJSON)

--
--
--

data Organisation = Organisation {
    oID :: OrganisationID,
    oCreatedAt :: UTCTime,
    oUpdatedAt :: UTCTime,
    oName :: Text,
    oType :: OrganisationType,
    oRole :: OrganisationRole
} deriving (Bridge, Show, Generic, FromJSON, ToJSON)
