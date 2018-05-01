{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Dashboard.Data (
    module HUD.Dashboard.Data.Relational,
    module HUD.Dashboard.Data.Persist,
    HandlerException(..),
    TokenState(..)
) where

import HUD.Bridge (Bridge)
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
    deriving (Bridge, Show, Generic, FromJSON, ToJSON)

instance Exception HandlerException

--
--
--

data TokenState = TokenState {
    tsGithub :: Bool,
    tsTrello :: Bool,
    tsHeroku :: Bool
} deriving (Bridge, Show, Generic, FromJSON, ToJSON)
