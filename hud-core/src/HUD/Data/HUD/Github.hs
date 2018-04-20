{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD.Github (
    module HUD.Data.HUD.Github.Common,
    module HUD.Data.HUD.Github.User,
    module HUD.Data.HUD.Github.Repo,
    HUDReq(..),
    HUDRsp(..),
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

data HUDReq
    = HRQRepoPRs Account Repo
    | HRQRepoPR Account Repo PRNum
    deriving (Bridge, Show, Generic, FromJSON, ToJSON)

--
--
--

data HUDRsp
    = HRSFailure
    | HRSRepoPRs [PR]
    | HRSRepoPR PRDetails
    deriving (Bridge, Show, Generic, FromJSON, ToJSON)

--

data PRDetails = PRDetails {
    prdPR :: PR,
    prdCommits :: [Commit],
    prdComments :: [Comment],
    prdIssueComments :: [IssueComment]
} deriving (Bridge, Show, Generic, FromJSON, ToJSON)
