
module HUD.Frontend.Component.Apps (
    Service(..),
    Query,
    comp
) where

import Prelude

import HUD.Data.OAuth (OAuthCode)
import HUD.Dashboard.Data (TokenState)
import HUD.Frontend.Operational (IdentityInfo, OpM)
import HUD.Frontend.Network.HTTP (AJAX)
import HUD.Frontend.Network.Dashboard (authGithub, authHeroku, authTrello, getTokenState)
import HUD.Frontend.Web.Navigate (NAVIGATE, navigate, origin, redirect)
import HUD.Frontend.Web.Route (toPath)
import HUD.Frontend.Router (AuthedRoute(Apps), Route(Authed))

import Control.Monad (void)
import Control.Monad.Aff (never)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans.Class (lift)
import Data.Generic (gShow)
import Data.Lens ((^.))
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Tuple (Tuple(..), fst, uncurry)
import Global (encodeURIComponent)
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

data Service
    = Github
    | Trello
    | Heroku

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
    | StartAuth Service a
    | Authorise Service (OAuthCode Unit) a

--
--
--

comp :: forall a e. Maybe (Tuple Service (OAuthCode Unit)) -> Component H.HTML Query Unit Void (OpM IdentityInfo a (Effects e))
comp auth = lifecycleComponent {
    initialState: const Loading,
    render,
    eval,
    initializer: (pure <<< action) (maybe Load (uncurry Authorise) auth),
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
                    HE.onClick (HE.input_ (StartAuth Github))
                ]
                [H.text "Authorize Github"],
            H.button
                [
                    HE.onClick (HE.input_ (StartAuth Trello))
                ]
                [H.text "Authorize Trello"],
            H.button
                [
                    HE.onClick (HE.input_ (StartAuth Heroku))
                ]
                [H.text "Authorize Heroku"]
        ]

--
--
--

eval :: forall a e. Query ~> ComponentDSL State Query Void (OpM IdentityInfo a (Effects e))
eval (Load next) = do
    (put <<< View) =<< lift loadViewState
    pure next
eval (StartAuth svc next) = do
    (void <<< lift) (startAuth svc)
    pure next
eval (Authorise svc code next) = do
    (void <<< lift) (authorise svc code)
    pure next

--
--
--

loadViewState :: forall a e. OpM IdentityInfo a (Effects e) ViewState
loadViewState = { tokenState: _ } <$> getTokenState

--
--
--

startAuth :: forall a e. Service -> OpM IdentityInfo a (Effects e) Void
startAuth Github = do
    { config: { github } } <- ask
    let url = "https://github.com/login/oauth/authorize?client_id=" <> github.appID <> "&scope=repo"
    redirect url
    liftAff never
startAuth Trello = do
    { config: { trello } } <- ask
    base <- origin
    let redir = encodeURIComponent (base <> "/" <> toPath (Authed Apps) <> "/trello/auth")
    let url = "https://trello.com/1/authorize?expiration=never&name=" <> trello.appName <> "&scope=read&response_type=token&return_url=" <> redir <> "&key=" <> trello.appID
    redirect url
    liftAff never
startAuth Heroku = do
    { config: { heroku } } <- ask
    let url = "https://id.heroku.com/oauth/authorize?client_id=" <> heroku.appID <> "&response_type=code&scope=read&state=idsfgsdfuyufw"
    redirect url
    liftAff never

--
--
--

authorise :: forall a e. Service -> OAuthCode Unit -> OpM IdentityInfo a (Effects e) Void
authorise svc code = do
    case svc of
        Github -> authGithub code
        Trello -> authTrello code
        Heroku -> authHeroku code
    navigate (Authed Apps)
    liftAff never
