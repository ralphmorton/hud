-- File auto generated by purescript-bridge! --
module HUD.Data.Identity


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

newtype Token =
    Token {
      unToken :: String
    }

derive instance genericToken :: Generic Token

derive instance newtypeToken :: Newtype Token _


--------------------------------------------------------------------------------
_Token :: Iso' Token { unToken :: String}
_Token = _Newtype

unToken :: Lens' Token String
unToken = _Newtype <<< prop (SProxy :: SProxy "unToken")

--------------------------------------------------------------------------------
derive instance eqToken :: Eq Token
instance decodeToken :: DecodeJson Token where
    decodeJson = Aeson.decodeJson
instance encodeToken :: EncodeJson Token where
    encodeJson = Aeson.encodeJson

