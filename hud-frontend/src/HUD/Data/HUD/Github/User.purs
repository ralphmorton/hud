-- File auto generated by purescript-bridge! --
module HUD.Data.HUD.Github.User


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

newtype AvatarURL =
    AvatarURL {
      unAvatarURL :: String
    }

derive instance genericAvatarURL :: Generic AvatarURL

derive instance newtypeAvatarURL :: Newtype AvatarURL _


--------------------------------------------------------------------------------
_AvatarURL :: Iso' AvatarURL { unAvatarURL :: String}
_AvatarURL = _Newtype

unAvatarURL :: Lens' AvatarURL String
unAvatarURL = _Newtype <<< prop (SProxy :: SProxy "unAvatarURL")

--------------------------------------------------------------------------------
derive instance eqAvatarURL :: Eq AvatarURL
instance decodeAvatarURL :: DecodeJson AvatarURL where
    decodeJson = Aeson.decodeJson
instance encodeAvatarURL :: EncodeJson AvatarURL where
    encodeJson = Aeson.encodeJson

