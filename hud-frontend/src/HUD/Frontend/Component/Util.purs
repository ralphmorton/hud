
module HUD.Frontend.Component.Util (
    class_
) where

import Prelude

import Halogen.HTML as H
import Halogen.HTML.Properties as HP

--
--
--

class_ :: forall r i. String -> HP.IProp ("class" :: String | r) i
class_ = HP.class_ <<< H.ClassName
