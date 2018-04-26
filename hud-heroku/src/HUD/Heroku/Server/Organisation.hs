{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Heroku.Server.Organisation (
    organisations
) where

import HUD.Data
import HUD.Data.HUD.Heroku
import HUD.Operational
import HUD.Names (Heroku)
import HUD.Heroku.Types
import HUD.Heroku.Server.Request

import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Time (UTCTime)
import Network.HTTP.Types (methodGet)
import UnliftIO (MonadUnliftIO)

--
--
--

organisations :: (
    MonadUnliftIO m,
    MonadThrow m,
    ContextReader r m,
    HasContext r HttpManager,
    HasContext r HerokuClient) => OAuthToken Heroku -> m [Organisation]
organisations oauth = fmap toOrganisation <$> herokuReq oauth methodGet "https://api.heroku.com/organizations" noData

--

toOrganisation :: OrganisationData -> Organisation
toOrganisation o =Organisation {
    oID = odID o,
    oCreatedAt = odCreatedAt o,
    oUpdatedAt = odUpdatedAt o,
    oName = odName o,
    oType = odType o,
    oRole = odRole o
}

--

data OrganisationData = OrganisationData {
    odID :: OrganisationID,
    odCreatedAt :: UTCTime,
    odUpdatedAt :: UTCTime,
    odName :: Text,
    odType :: OrganisationType,
    odRole :: OrganisationRole
}

instance FromJSON OrganisationData where
    parseJSON (Object o) = do
        oid <- OrganisationID <$> o .: "id"
        created <- o .: "created_at"
        updated <- o .: "updated_at"
        name <- o .: "name"
        typ <- (o .: "type" :: Parser Text)
        role <- (o .: "role" :: Parser Text)
        pure OrganisationData {
            odID = oid,
            odCreatedAt = created,
            odUpdatedAt = updated,
            odName = name,
            odType = case typ of
                "enterprise" -> OTEnterprise
                "team" -> OTTeam
                _ -> OTUnknown,
            odRole = case role of
                "owner" -> OROwner
                "admin" -> ORAdmin
                "collaborator" -> ORCollaborator
                "member" -> ORMember
                _ -> ORNone
        }
        
    parseJSON _ = mzero
