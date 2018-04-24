
module HUD.Frontend.Component.HUD (
    Query,
    comp
) where

import Prelude

import HUD.Data.HUD (HUDReq(HRQGithub), HUDRsp)
import HUD.Data.HUD.Github (GithubHUDReq(GHHRQRepoPR))
import HUD.Data.HUD.Github.Common (Account(..), Repo(..), PRNum(..))
import HUD.Frontend.Operational (IdentityInfo, OpM)
import HUD.Frontend.Network.HTTP (AJAX)
import HUD.Frontend.Network.Dashboard (hud)

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

type Effects e = (ajax :: AJAX| e)

--
--
--

data State
    = Loading
    | View ViewState

--

type ViewState = {
    rsp :: HUDRsp
}

--
--
--

data Query a
    = Load a

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
renderView vs = H.div_  [H.text (gShow vs.rsp)]

--
--
--

eval :: forall a e. Query ~> ComponentDSL State Query Void (OpM IdentityInfo a (Effects e))
eval (Load next) = do
    (put <<< View) =<< lift loadViewState
    pure next

--
--
--

loadViewState :: forall a e. OpM IdentityInfo a (Effects e) ViewState
loadViewState = do
    let account = Account { unAccount: "ralphmorton" }
    let repo = Repo { unRepo: "coeus" }
    let prnum = PRNum { unPRNum: 2 }
    let req = HRQGithub (GHHRQRepoPR account repo prnum)
    { rsp: _ } <$> hud req
