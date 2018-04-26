{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD (
    Req(..),
    Rsp(..)
) where

import HUD.Bridge (Bridge)
import HUD.Data.HUD.Github (GithubReq, GithubRsp)
import HUD.Data.HUD.Trello (TrelloReq, TrelloRsp)
import HUD.Data.HUD.Heroku (HerokuReq, HerokuRsp)

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

--
--
--

data Req
    = RQGithub GithubReq
    | RQTrello TrelloReq
    | RQHeroku HerokuReq
    deriving (Bridge, Show, Generic, FromJSON, ToJSON)

--
--
--

data Rsp
    = RSGithub GithubRsp
    | RSTrello TrelloRsp
    | RSHeroku HerokuRsp
    deriving (Bridge, Show, Generic, FromJSON, ToJSON)
