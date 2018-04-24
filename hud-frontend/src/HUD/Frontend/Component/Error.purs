
module HUD.Frontend.Component.Error (
    Query,
    comp
) where

import Prelude

import HUD.Frontend.Web.Navigate (NAVIGATE, navigate)
import HUD.Frontend.Router (AuthedRoute(SelectAccount), Route(Authed))

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(Nothing))
import Halogen (Component, ComponentDSL, ComponentHTML, component)
import Halogen.HTML as H
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

--
--
--

type Effects e = (navigate :: NAVIGATE | e)

--
--
--

data Query a
    = GoHome a

--
--
--

comp :: forall e. Component H.HTML Query Unit Void (Aff (Effects e))
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
render _ = H.div_ [message, button]
    where
    message = H.text "Something went wrong"
    button =
        H.button
            [HP.title "Home", HE.onClick (HE.input_ GoHome)]
            [H.text "Home"]

--
--
--

eval :: forall e. Query ~> ComponentDSL Unit Query Void (Aff (Effects e))
eval (GoHome next) = do
    navigate (Authed SelectAccount)
    pure next
