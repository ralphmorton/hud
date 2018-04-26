-- File auto generated by purescript-bridge! --
module HUD.Data.HUD.Trello.Common


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
import Prim (String)

import Prelude
import Data.Generic (class Generic)

newtype Member =
    Member {
      unMember :: String
    }

derive instance genericMember :: Generic Member

derive instance newtypeMember :: Newtype Member _


--------------------------------------------------------------------------------
_Member :: Iso' Member { unMember :: String}
_Member = _Newtype

unMember :: Lens' Member String
unMember = _Newtype <<< prop (SProxy :: SProxy "unMember")

--------------------------------------------------------------------------------
newtype Board =
    Board {
      unBoard :: String
    }

derive instance genericBoard :: Generic Board

derive instance newtypeBoard :: Newtype Board _


--------------------------------------------------------------------------------
_Board :: Iso' Board { unBoard :: String}
_Board = _Newtype

unBoard :: Lens' Board String
unBoard = _Newtype <<< prop (SProxy :: SProxy "unBoard")

--------------------------------------------------------------------------------
derive instance eqMember :: Eq Member
instance decodeMember :: DecodeJson Member where
    decodeJson = Aeson.decodeJson
instance encodeMember :: EncodeJson Member where
    encodeJson = Aeson.encodeJson
derive instance eqBoard :: Eq Board
instance decodeBoard :: DecodeJson Board where
    decodeJson = Aeson.decodeJson
instance encodeBoard :: EncodeJson Board where
    encodeJson = Aeson.encodeJson
