
module HUD.Frontend.Component.Dashboard (
    Input,
    Query,
    comp
) where

import Prelude

import HUD.Dashboard.Data.Relational (AccountKey)
import HUD.Frontend.Operational (IdentityInfo, OpM)
import HUD.Frontend.Web.Navigate (NAVIGATE)
import HUD.Frontend.Router (AuthedRoute)
import HUD.Frontend.Component.Dashboard.Menu.Top as T
import HUD.Frontend.Component.Dashboard.Menu.Side as S

import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(Nothing))
import Halogen (Component, ParentDSL, ParentHTML, parentComponent)
import Halogen.Component.ChildPath as CP
import Halogen.HTML as H
import Halogen.HTML.Properties as HP

--
--
--

type Effects e = (navigate :: NAVIGATE | e)

type Child q i e = Component H.HTML q i Void (M e)

type M e = OpM IdentityInfo AccountKey (Effects e)

--

type Input i = {
    route :: AuthedRoute,
    childInput :: i
}

--

data Query a
    = Nop a

--

type Slot = Either3 Unit Unit Unit

type ChildQuery q = Coproduct3 T.Query S.Query q

--
--
--

comp :: forall e q i. Child q i e -> Component H.HTML Query (Input i) Void (M e)
comp child = parentComponent {
    initialState: id,
    render: render child,
    eval,
    receiver: const Nothing
}

--
--
--

render :: forall e q i. Child q i e -> Input i -> ParentHTML Query (ChildQuery q) Slot (M e)
render child i = H.div_ [top, page]
    where
    top = H.slot' CP.cp1 unit T.comp i.route absurd
    page = H.div [HP.class_ (H.ClassName "app-body")] [renderSidebar i, renderMain child i]

--

renderSidebar :: forall e q i. Input i -> ParentHTML Query (ChildQuery q) Slot (M e)
renderSidebar i = H.div props [side]
    where
    props = [HP.class_ (H.ClassName "sidebar")]
    side = H.slot' CP.cp2 unit S.comp i.route absurd

--

renderMain :: forall e q i. Child q i e -> Input i -> ParentHTML Query (ChildQuery q) Slot (M e)
renderMain child i =
    H.main
        [HP.class_ (H.ClassName "main")]
        [
            H.div
                [HP.class_ (H.ClassName "container-fluid")]
                [H.slot' CP.cp3 unit child i.childInput absurd]
        ]

--
--
--

eval :: forall e q i. Query ~> ParentDSL (Input i) Query (ChildQuery q) Slot Void (M e)
eval (Nop next) = pure next
