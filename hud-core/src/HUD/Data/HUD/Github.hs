{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD.Github (
    module HUD.Data.HUD.Github.Common,
    module HUD.Data.HUD.Github.User,
    module HUD.Data.HUD.Github.Repo,
    GithubReq(..),
    GithubRsp(..),
    PRDetails(..)
) where

import HUD.Bridge (Bridge)
import HUD.Data.HUD.Github.Common
import HUD.Data.HUD.Github.User
import HUD.Data.HUD.Github.Repo

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

--
--
--

data GithubReq
    = GHRQRepos
    | GHRQRepoPRs Account Repo
    | GHRQRepoPR Account Repo PRNum
    deriving (Bridge, Show, Generic, FromJSON, ToJSON)

--
--
--

data GithubRsp
    = GHRSFailure
    | GHRSRepos [(Account, Repo)]
    | GHRSRepoPRs [PR]
    | GHRSRepoPR PRDetails
    deriving (Bridge, Show, Generic, FromJSON, ToJSON)

--

data PRDetails = PRDetails {
    prdPR :: PR,
    prdCommits :: [Commit],
    prdComments :: [Comment],
    prdIssueComments :: [IssueComment]
} deriving (Bridge, Show, Generic, FromJSON, ToJSON)
