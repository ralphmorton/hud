-- File auto generated by purescript-bridge! --
module HUD.Data.HUD.Trello


where
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Generic.Aeson as Aeson


import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import Data.Tuple (Tuple)
import HUD.Data.HUD.Trello.Board (Board, BoardOverview)
import Prim (Array, Int, String)

import Prelude
import Data.Generic (class Generic)

data TrelloRsp =
    TRRSFailure TrelloRequestException
  | TRRSBoards (Array (Tuple Board String))
  | TRRSBoardOverview BoardOverview

derive instance genericTrelloRsp :: Generic TrelloRsp


--------------------------------------------------------------------------------
_TRRSFailure :: Prism' TrelloRsp TrelloRequestException
_TRRSFailure = prism' TRRSFailure f
  where
    f (TRRSFailure a) = Just $ a
    f _ = Nothing

_TRRSBoards :: Prism' TrelloRsp (Array (Tuple Board String))
_TRRSBoards = prism' TRRSBoards f
  where
    f (TRRSBoards a) = Just $ a
    f _ = Nothing

_TRRSBoardOverview :: Prism' TrelloRsp BoardOverview
_TRRSBoardOverview = prism' TRRSBoardOverview f
  where
    f (TRRSBoardOverview a) = Just $ a
    f _ = Nothing

--------------------------------------------------------------------------------
data TrelloRequestException =
    TRREAuthFailure
  | TRREParseFailure
  | TRREUnknownFailure Int

derive instance genericTrelloRequestException :: Generic TrelloRequestException


--------------------------------------------------------------------------------
_TRREAuthFailure :: Prism' TrelloRequestException Unit
_TRREAuthFailure = prism' (\_ -> TRREAuthFailure) f
  where
    f TRREAuthFailure = Just unit
    f _ = Nothing

_TRREParseFailure :: Prism' TrelloRequestException Unit
_TRREParseFailure = prism' (\_ -> TRREParseFailure) f
  where
    f TRREParseFailure = Just unit
    f _ = Nothing

_TRREUnknownFailure :: Prism' TrelloRequestException Int
_TRREUnknownFailure = prism' TRREUnknownFailure f
  where
    f (TRREUnknownFailure a) = Just $ a
    f _ = Nothing

--------------------------------------------------------------------------------
data TrelloReq =
    TRRQBoards
  | TRRQBoardOverview Board

derive instance genericTrelloReq :: Generic TrelloReq


--------------------------------------------------------------------------------
_TRRQBoards :: Prism' TrelloReq Unit
_TRRQBoards = prism' (\_ -> TRRQBoards) f
  where
    f TRRQBoards = Just unit
    f _ = Nothing

_TRRQBoardOverview :: Prism' TrelloReq Board
_TRRQBoardOverview = prism' TRRQBoardOverview f
  where
    f (TRRQBoardOverview a) = Just $ a
    f _ = Nothing

--------------------------------------------------------------------------------
instance decodeTrelloRsp :: DecodeJson TrelloRsp where
    decodeJson = Aeson.decodeJson
instance encodeTrelloRsp :: EncodeJson TrelloRsp where
    encodeJson = Aeson.encodeJson
instance decodeTrelloRequestException :: DecodeJson TrelloRequestException where
    decodeJson = Aeson.decodeJson
instance encodeTrelloRequestException :: EncodeJson TrelloRequestException where
    encodeJson = Aeson.encodeJson
instance decodeTrelloReq :: DecodeJson TrelloReq where
    decodeJson = Aeson.decodeJson
instance encodeTrelloReq :: EncodeJson TrelloReq where
    encodeJson = Aeson.encodeJson
