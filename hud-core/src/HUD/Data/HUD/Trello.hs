{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD.Trello (
    module HUD.Data.HUD.Trello.Common,
    module HUD.Data.HUD.Trello.Board,
    TrelloHUDReq(..),
    TrelloHUDRsp(..),
    TrelloRequestException(..)
) where

import HUD.Bridge (Bridge)
import HUD.Data.HUD.Trello.Common
import HUD.Data.HUD.Trello.Board

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

--
--
--

data TrelloHUDReq
    = TRHRQBoardOverview Board
    deriving (Bridge, Show, Generic, FromJSON, ToJSON)

--
--
--

data TrelloHUDRsp
    = TRHRSFailure TrelloRequestException
    | TRHRSBoardOverview BoardOverview
    deriving (Bridge, Show, Generic, FromJSON, ToJSON)

--
--
--

data TrelloRequestException
    = TRREAuthFailure
    | TRREParseFailure
    | TRREUnknownFailure Int
    deriving (Bridge, Show, Generic, FromJSON, ToJSON)

instance Exception TrelloRequestException
