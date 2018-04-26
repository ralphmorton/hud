{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Dashboard.Data (
    module HUD.Dashboard.Data.Relational,
    module HUD.Dashboard.Data.Persist,
    HandlerException(..)
) where

import HUD.Dashboard.Data.Relational
import HUD.Dashboard.Data.Persist

import Control.Exception
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

--
--
--

data HandlerException
    = MissingAuthToken
    | BadAuthToken
    | BadPassword
    | UnknownUser
    | IllegalPassword
    | NotFound
    | InternalFailure
    | MissingGithubToken
    | MissingTrelloToken
    | MissingHerokuToken
    deriving (Show, Generic, FromJSON, ToJSON)

instance Exception HandlerException
