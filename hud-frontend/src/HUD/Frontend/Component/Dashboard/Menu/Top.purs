
module HUD.Frontend.Component.Dashboard.Menu.Top (
    Query,
    comp
) where

import Prelude

import HUD.Dashboard.Data.Relational (AccountKey)
import HUD.Frontend.Operational (IdentityInfo, OpM)
import HUD.Frontend.Router (AuthedRoute)

import Data.Maybe (Maybe(Nothing))
import Halogen (Component, ComponentDSL, ComponentHTML, component)
import Halogen.HTML as H
import Halogen.HTML.Properties as HP

--
--
--

type State = {
    route :: AuthedRoute
}

--
--
--

data Query a
    = Nop a

--
--
--

comp :: forall e. Component H.HTML Query AuthedRoute Void (OpM IdentityInfo AccountKey e)
comp = component {
    initialState: { route: _ },
    render,
    eval,
    receiver: const Nothing
}

--
--
--

render :: State -> ComponentHTML Query
render s = H.header props [brand]
    where
    props = [HP.class_ (H.ClassName "app-header navbar")]
    brand = H.a [HP.href "/", HP.class_ (H.ClassName "navbar-brand")] []

--
--
--

eval :: forall e. Query ~> ComponentDSL State Query Void (OpM IdentityInfo AccountKey e)
eval (Nop next) = do
    pure next
