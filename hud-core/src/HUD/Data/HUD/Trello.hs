{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD.Trello (
    module HUD.Data.HUD.Trello.Member,
    module HUD.Data.HUD.Trello.Board,
    TrelloReq(..),
    TrelloRsp(..),
    TrelloRequestException(..)
) where

import HUD.Bridge (Bridge)
import HUD.Data.HUD.Trello.Member
import HUD.Data.HUD.Trello.Board

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

--
--
--

data TrelloReq
    = TRRQBoards
    | TRRQBoardOverview Board
    deriving (Bridge, Show, Generic, FromJSON, ToJSON)

--
--
--

data TrelloRsp
    = TRRSFailure TrelloRequestException
    | TRRSBoards [(Board, Text)]
    | TRRSBoardOverview BoardOverview
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
