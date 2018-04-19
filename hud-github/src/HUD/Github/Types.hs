module HUD.Github.Types (
    module HUD.Github.Types.Repo,
    GithubError(..)
) where

import HUD.Github.Types.Repo

import Control.Exception (Exception)
import GitHub.Data.Definitions (Error)

--
--
--

data GithubError
    = MissingAuthToken
    | BadAuthToken
    | GithubError Error
    deriving Show

instance Exception GithubError
