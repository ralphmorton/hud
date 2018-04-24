
module HUD.Frontend.Component.SelectAccount (
    Query,
    comp
) where

import Prelude

import HUD.Dashboard.Data.Relational (Account, AccountKey, UserLevel, accountName)
import HUD.Frontend.Operational (IdentityInfo, OpM)
import HUD.Frontend.Network.HTTP (AJAX)
import HUD.Frontend.Network.Dashboard (listAccounts)
import HUD.Frontend.Web.Navigate (NAVIGATE, navigate)
import HUD.Frontend.Router (AuthedRoute(List), Route(Authed))

import Control.Monad.Aff (never)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Trans.Class (lift)
import Data.Generic (gShow)
import Data.Lens ((^.))
import Data.Maybe (Maybe(Nothing))
import Data.Tuple (Tuple(..), fst, uncurry)
import Halogen (Component, ComponentDSL, ComponentHTML, action, lifecycleComponent, put)
import Halogen.HTML as H
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

--
--
--

type Effects e = (ajax :: AJAX, navigate :: NAVIGATE | e)

--
--
--

data State
    = Loading
    | View ViewState

--

type ViewState = {
    accounts :: Array (Tuple (Tuple AccountKey Account) UserLevel)
}

--
--
--

data Query a
    = Load a
    | Select AccountKey a

--
--
--

comp :: forall c e. Component H.HTML Query Unit Void (OpM IdentityInfo c (Effects e))
comp = lifecycleComponent {
    initialState: const Loading,
    render,
    eval,
    initializer: pure (action Load),
    finalizer: Nothing,
    receiver: const Nothing
}

--
--
--

render :: State -> ComponentHTML Query
render Loading = renderLoading
render (View vs) = renderView vs

--

renderLoading :: ComponentHTML Query
renderLoading = H.div_ [H.text "Loading"]

--

renderView :: ViewState -> ComponentHTML Query
renderView vs = H.div props (uncurry renderAccount <$> vs.accounts)
    where props = [HP.class_ (H.ClassName "row")]

renderAccount :: (Tuple AccountKey Account) -> UserLevel -> ComponentHTML Query
renderAccount (Tuple k a) l = H.div props [name, button]
    where
    props = [HP.class_ (H.ClassName "col-md-12")]
    name = H.h4_ [H.text (a ^. accountName <> " (" <> gShow l <> ")")]
    button =
        H.button
            [HP.title "Select", HE.onClick (HE.input_ (Select k))]
            [H.text "Select"]

--
--
--

eval :: forall c e. Query ~> ComponentDSL State Query Void (OpM IdentityInfo c (Effects e))
eval (Load next) = do
    (put <<< View) =<< lift loadViewState
    pure next
eval (Select k next) = do
    (lift <<< navigate) (Authed (List k))
    pure next

--
--
--

loadViewState :: forall c e. OpM IdentityInfo c (Effects e) ViewState
loadViewState = do
    accounts <- listAccounts
    case accounts of
        [acc] -> do
            (navigate <<< Authed <<< List <<< fst) (fst acc)
            liftAff never
        _ ->
            pure { accounts }
