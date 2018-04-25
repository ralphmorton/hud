{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD (
    HUDReq(..),
    HUDRsp(..)
) where

import HUD.Bridge (Bridge)
import HUD.Data.HUD.Github (GithubHUDReq, GithubHUDRsp)
import HUD.Data.HUD.Trello (TrelloHUDReq, TrelloHUDRsp)

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

--
--
--

data HUDReq
    = HRQGithub GithubHUDReq
    | HRQTrello TrelloHUDReq
    deriving (Bridge, Show, Generic, FromJSON, ToJSON)

--
--
--

data HUDRsp
    = HRSGithub GithubHUDRsp
    | HRSTrello TrelloHUDRsp
    deriving (Bridge, Show, Generic, FromJSON, ToJSON)
