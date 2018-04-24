
module HUD.Frontend.Component.Dashboard.Menu.Side (
    Query,
    comp
) where

import Prelude

import HUD.Dashboard.Data.Relational (AccountKey)
import HUD.Frontend.Operational (IdentityInfo, OpM)
import HUD.Frontend.Router (AuthedRoute(List), Route(Authed))
import HUD.Frontend.Web.Navigate (NAVIGATE, navigate)

import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans.Class (lift)
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

type State = {
    route :: AuthedRoute
}

--
--
--

data Query a
    = Nav (AccountKey -> Route) a

--
--
--

comp :: forall e. Component H.HTML Query AuthedRoute Void (OpM IdentityInfo AccountKey (Effects e))
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
render s = H.nav props [ul]
    where
    props = [HP.class_ (H.ClassName "sidebar-nav")]
    ul = H.ul [HP.class_ (H.ClassName "nav")] (renderMenuItems s)

renderMenuItems :: State -> Array (ComponentHTML Query)
renderMenuItems s = [home]
    where
    home = renderMenuItem "Home" (Authed <<< List)

renderMenuItem :: String -> (AccountKey -> Route) -> ComponentHTML Query
renderMenuItem title f = H.li props [link]
    where
    props = [HP.class_ (H.ClassName "nav-item")]
    link =
        H.a
            [
                HP.class_ (H.ClassName "nav-link"),
                HP.href "#",
                HE.onClick (HE.input_ (Nav f))
            ]
            [H.text title]

--
--
--

eval :: forall e. Query ~> ComponentDSL State Query Void (OpM IdentityInfo AccountKey (Effects e))
eval (Nav f next) = do
    acc <- _.acc <$> lift ask
    navigate (f acc)
    pure next
