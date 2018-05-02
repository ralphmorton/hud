
module HUD.Frontend.Component.Apps (
    Query,
    comp
) where

import Prelude

import HUD.Dashboard.Data (TokenState)
import HUD.Frontend.Operational (IdentityInfo, OpM)
import HUD.Frontend.Network.HTTP (AJAX)
import HUD.Frontend.Network.Dashboard (getTokenState)
import HUD.Frontend.Web.Navigate (NAVIGATE, redirect)

import Control.Monad (void)
import Control.Monad.Aff (never)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Reader.Class (ask)
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
    tokenState :: TokenState
}

--
--
--

data Query a
    = Load a
    | AuthToGithub a

--
--
--

comp :: forall a e. Component H.HTML Query Unit Void (OpM IdentityInfo a (Effects e))
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
renderView vs =
    H.div_
        [
            H.h1_ [H.text "Token State"],
            H.text (gShow vs.tokenState),
            H.h1_ [H.text "Auth"],
            H.button
                [
                    HE.onClick (HE.input_ AuthToGithub)
                ]
                [H.text "Authorize Github"]
        ]

--
--
--

eval :: forall a e. Query ~> ComponentDSL State Query Void (OpM IdentityInfo a (Effects e))
eval (Load next) = do
    (put <<< View) =<< lift loadViewState
    pure next
eval (AuthToGithub next) = do
    void (lift authToGithub)
    pure next

--
--
--

loadViewState :: forall a e. OpM IdentityInfo a (Effects e) ViewState
loadViewState = { tokenState: _ } <$> getTokenState

--
--
--

authToGithub :: forall a e. OpM IdentityInfo a (Effects e) Void
authToGithub = do
    { config: { github } } <- ask
    let url = "https://github.com/login/oauth/authorize?client_id=" <> github.appID <> "&scope=repo"
    redirect url
    liftAff never
