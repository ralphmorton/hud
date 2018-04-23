
module HUD.Frontend.Component.Login (
    Query,
    comp
) where

import Prelude

import HUD.Data.Common (EmailAddress(..), unEmailAddress)
import HUD.Frontend.Operational (Exception(HttpException), OpM)
import HUD.Frontend.Network.Dashboard (login)
import HUD.Frontend.Storage (storeToken)
import HUD.Frontend.Network.HTTP (AJAX, HttpException(StatusCodeException))
import HUD.Frontend.Web.Navigate (NAVIGATE, navigate)
import HUD.Frontend.Router (AuthedRoute(Home), PublicRoute(Onboard), Route(..))
import HUD.Frontend.Component.Util (class_)

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Trans.Class (lift)
import Data.DateTime (DateTime, adjust)
import Data.DateTime.Locale (LocalValue(..))
import Data.Lens ((^.))
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Time.Duration (Seconds)
import Data.Tuple (Tuple(..), fst, snd)
import DOM (DOM)
import DOM.WebStorage (STORAGE)
import Halogen (Component, ComponentDSL, ComponentHTML, component, put)
import Halogen.HTML as H
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

--
--
--

type Effects e = (ajax :: AJAX, dom :: DOM, navigate :: NAVIGATE, now :: NOW, storage :: STORAGE | e)

--
--
--

data State
    = EnteringLogin EmailAddress String
    | LoggingIn EmailAddress
    | BadAuth EmailAddress

--
--
--

data Query a
    = SetLogin EmailAddress String a
    | Login EmailAddress String a
    | ForgotPassword a

--
--
--

comp :: forall e i c. Component H.HTML Query Unit Void (OpM i c (Effects e))
comp = component {
    initialState: const initialState,
    render: render,
    eval: eval,
    receiver: const Nothing
}

--

initialState :: State
initialState = EnteringLogin (email "") ""

--
--
--

render :: State -> ComponentHTML Query
render (EnteringLogin addr pw) = renderLogin addr pw false false
render (LoggingIn addr) = renderLogin addr "" false true
render (BadAuth addr) = renderLogin addr "" true false

--

renderLogin :: EmailAddress -> String -> Boolean -> Boolean -> ComponentHTML Query
renderLogin addr pw err logging = renderWrapper (renderEnteringLogin' addr pw err logging)

--

renderWrapper :: ComponentHTML Query -> ComponentHTML Query
renderWrapper content =
    H.div
        [class_ "flex-row align-items-center"]
        [
            H.div
                [class_ "container login-container"]
                [
                    H.div
                        [class_ "row justify-content-center"]
                        [
                            H.div
                                [class_ "col-md-8"]
                                [
                                    H.div
                                        [class_ "card-group"]
                                        [
                                            H.div
                                                [class_ "card p-4"]
                                                [
                                                    H.div
                                                        [class_ "card-body"]
                                                        [content]
                                                ]
                                        ]
                                ]
                        ]
                ]
        ]

--

renderEnteringLogin' :: EmailAddress -> String -> Boolean -> Boolean -> ComponentHTML Query
renderEnteringLogin' addr pw err logging = H.div_ [title, instructions, email', password, controls]
    where
    title = H.h1_ [H.text "Login"]
    instructions = case err of
        false -> H.p [class_ "text-muted"] [H.text "Sign In to your account"]
        true -> H.p [class_ "login-error"] [H.text "Invalid email or password"]
    email' = renderEmailInput addr pw logging
    password = renderPasswordInput addr pw logging
    controls = renderControls addr pw logging

renderEmailInput :: EmailAddress -> String -> Boolean -> ComponentHTML Query
renderEmailInput addr pw logging =
    H.div
        [class_ "input-group mb-3"]
        [
            H.div
                [class_ "input-group-prepend"]
                [
                    H.span
                        [class_ "input-group-text"]
                        [
                            H.i [class_ "fa fa-user"] []
                        ]
                ],
            H.input [
                class_ "form-control",
                HP.placeholder "Email Address",
                HP.type_ HP.InputText,
                HP.value (addr ^. unEmailAddress),
                HP.disabled logging,
                HE.onValueInput (HE.input (flip SetLogin pw <<< email))
            ]
        ]

renderPasswordInput :: EmailAddress -> String -> Boolean -> ComponentHTML Query
renderPasswordInput addr pw logging =
    H.div
        [class_ "input-group mb-4"]
        [
            H.div
                [class_ "input-group-prepend"]
                [
                    H.span
                        [class_ "input-group-text"]
                        [
                            H.i [class_ "fa fa-lock"] []
                        ]
                ],
            H.input [
                class_ "form-control",
                HP.placeholder "Password",
                HP.type_ HP.InputPassword,
                HP.value pw,
                HP.disabled logging,
                HE.onValueInput (HE.input (SetLogin addr))
            ]
        ]

renderControls :: EmailAddress -> String -> Boolean -> ComponentHTML Query
renderControls addr pw logging =
    H.div
        [class_ "row"]
        [
            H.div
                [class_ "col-6"]
                [
                    H.button
                        [
                            class_ "btn btn-primary px-4",
                            HP.title "Login",
                            HP.disabled (pw == "" || addr == email "" || logging),
                            HE.onClick (HE.input_ (Login addr pw))
                        ]
                        [H.text (if logging then "Logging in..." else "Login")]
                ],
            H.div
                [class_ "col-6 text-right"]
                [
                    H.button
                        [
                            class_ "btn btn-link px-0",
                            HP.title "Forgot Password",
                            HP.disabled logging,
                            HE.onClick (HE.input_ ForgotPassword)
                        ]
                        [H.text "Forgot Password"]
                ]
        ]

--
--
--

eval :: forall e i c. Query ~> ComponentDSL State Query Void (OpM i c (Effects e))
eval (SetLogin addr pw next) = do
    put (EnteringLogin addr pw)
    pure next
eval (Login addr pw next) = do
    put (LoggingIn addr)
    res <- lift (login' addr pw)
    when (not res) $ put (BadAuth addr)
    pure next
eval (ForgotPassword next) = do
    (lift <<< navigate) (Public Onboard)
    pure next

--
--
--

login' :: forall i c e. EmailAddress -> String -> OpM i c (Effects e) Boolean
login' addr pw = handleLogin do
    res <- login addr pw
    now <- getExpiry (fst res)
    storeToken (Tuple now (snd res))
    navigate (Authed Home)
    pure true

handleLogin :: forall i c e. OpM i c (Effects e) Boolean -> OpM i c (Effects e) Boolean
handleLogin = flip catchError $ \e -> case e of
    HttpException (StatusCodeException 403) -> pure false
    _ -> throwError e

getExpiry :: forall i c e. Seconds -> OpM i c (Effects e) DateTime
getExpiry ttl = do
    res <- liftEff nowDateTime
    case res of
        LocalValue _ dt -> pure (maybe dt id $ adjust ttl dt)

--
--
--

email :: String -> EmailAddress
email = EmailAddress <<< { unEmailAddress: _ }
