
module HUD.Frontend.Component.Home (
    Query,
    comp
) where

import Prelude

import Data.Maybe (Maybe(Nothing))
import Halogen (Component, ComponentDSL, ComponentHTML, component)
import Halogen.HTML as H
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

--
--
--

data Query a
    = Nop a

--
--
--

comp :: forall m. Component H.HTML Query Unit Void m
comp = component {
    initialState: id,
    render: render,
    eval: eval,
    receiver: const Nothing
}

--
--
--

render :: Unit -> ComponentHTML Query
render _ = H.h1_ [H.text "HOME"]

--
--
--

eval :: forall m. Query ~> ComponentDSL Unit Query Void m
eval (Nop next) = do
    pure next
