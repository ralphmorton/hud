
module HUD.Dashboard.Data (
    module HUD.Dashboard.Data.Relational,
    module HUD.Dashboard.Data.Persist,
    HandlerException(..)
) where

import HUD.Dashboard.Data.Relational
import HUD.Dashboard.Data.Persist

import Control.Exception

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
    deriving Show

instance Exception HandlerException

