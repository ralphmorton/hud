
module HUD.Frontend.Component.Onboard (
    Query,
    comp
) where

import Prelude

import HUD.Data.Common (EmailAddress(..), unEmailAddress)
import HUD.Data.Identity (Token)
import HUD.Frontend.Operational (OpM)
import HUD.Frontend.Network.Dashboard (setPassword)
import HUD.Frontend.Network.Identity (confirmEmailAddress, validateEmailAddress)
import HUD.Frontend.Network.HTTP (AJAX)

import Control.Monad.Trans.Class (lift)
import Data.Lens ((^.))
import Data.Maybe (Maybe(Nothing))
import Data.String as S
import Halogen (Component, ComponentDSL, ComponentHTML, component, put)
import Halogen.HTML as H
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Text.Email.Validate (isValid)

--
--
--

type Effects e = (ajax :: AJAX | e)

--
--
--

data State
    = EnteringEmail EmailAddress
    | RequestingCode EmailAddress
    | EnteringCode EmailAddress String
    | ConfirmingCode EmailAddress String
    | EnteringPassword EmailAddress Token String String
    | SettingPassword EmailAddress Token
    | Success EmailAddress
    | Failure EmailAddress Failure

data Failure
    = InvalidConfirmCode
    | NoSuchUser
    | IllegalPassword
    | UnhandledFailure

instance showFailure :: Show Failure where
    show InvalidConfirmCode = "InvalidConfirmCode"
    show NoSuchUser = "NoSuchUser"
    show IllegalPassword = "IllegalPassword"
    show UnhandledFailure = "UnhandledFailure"

--
--
--

data Query a
    = SetEmail EmailAddress a
    | ConfirmEmail EmailAddress a
    | SetCode EmailAddress String a
    | ConfirmCode EmailAddress String a
    | SetPasswords EmailAddress Token String String a
    | SavePassword EmailAddress Token String a
    | Reset EmailAddress a

--
--
--

comp :: forall e i c. Component H.HTML Query Unit Void (OpM i c (Effects e))
comp = component {
    initialState: (const <<< EnteringEmail) (email ""),
    render: render,
    eval: eval,
    receiver: const Nothing
}

--
--
--

render :: State -> ComponentHTML Query
render (EnteringEmail addr) = renderEnteringEmail addr
render (RequestingCode addr) = renderRequestingCode addr
render (EnteringCode addr code) = renderEnteringCode addr code
render (ConfirmingCode addr code) = renderConfirmingCode addr code
render (EnteringPassword addr tok pw1 pw2) = renderEnteringPassword addr tok pw1 pw2
render (SettingPassword addr tok) = renderSettingPassword addr tok
render (Success addr) = renderSuccess addr
render (Failure addr failure) = renderFailure addr failure

--

renderEnteringEmail :: EmailAddress -> ComponentHTML Query
renderEnteringEmail addr = H.div_ [input, next]
    where
    input =
        H.input [
            HP.type_ HP.InputText,
            HP.value (addr ^. unEmailAddress),
            HE.onValueInput (HE.input (SetEmail <<< email))
        ]
    next =
        H.button
            [
                HP.title "Next",
                HP.disabled (not <<< isValid $ addr ^. unEmailAddress),
                HE.onClick (HE.input_ (ConfirmEmail addr))
            ]
            [H.text "Next"]

--

renderRequestingCode :: EmailAddress -> ComponentHTML Query
renderRequestingCode addr = H.div_ [H.text "requesting code"]

--

renderEnteringCode :: EmailAddress -> String -> ComponentHTML Query
renderEnteringCode addr code = H.div_ [address, input, next]
    where
    address = H.div_ [H.text (addr ^. unEmailAddress)]
    input =
        H.input [
            HP.type_ HP.InputText,
            HP.value code,
            HE.onValueInput (HE.input (SetCode addr))
        ]
    next =
        H.button
            [
                HP.title "Next",
                HP.disabled (code == ""),
                HE.onClick (HE.input_ (ConfirmCode addr code))
            ]
            [H.text "Next"]

--

renderConfirmingCode :: EmailAddress -> String -> ComponentHTML Query
renderConfirmingCode addr code = H.div_ [H.text "confirming code"]

--

renderEnteringPassword :: EmailAddress -> Token -> String -> String -> ComponentHTML Query
renderEnteringPassword addr tok pw1 pw2 = H.div_ [address, input1, input2, next]
    where
    address = H.div_ [H.text (addr ^. unEmailAddress)]
    input1 =
        H.input [
            HP.type_ HP.InputPassword,
            HP.value pw1,
            HE.onValueInput (HE.input (flip (SetPasswords addr tok) pw2))
        ]
    input2 =
        H.input [
            HP.type_ HP.InputPassword,
            HP.value pw2,
            HE.onValueInput (HE.input (SetPasswords addr tok pw1))
        ]
    next =
        H.button
            [
                HP.title "Next",
                HP.disabled (S.length pw1 < 8 || pw1 /= pw2),
                HE.onClick (HE.input_ (SavePassword addr tok pw1))
            ]
            [H.text "Next"]

--

renderSettingPassword :: EmailAddress -> Token -> ComponentHTML Query
renderSettingPassword addr tok = H.div_ [H.text "setting password"]

--

renderSuccess :: EmailAddress -> ComponentHTML Query
renderSuccess addr = H.div_ [H.text "success"]

--

renderFailure :: EmailAddress -> Failure -> ComponentHTML Query
renderFailure addr failure = H.div_ [H.text (show failure)]

--
--
--

eval :: forall e i c. Query ~> ComponentDSL State Query Void (OpM i c (Effects e))
eval (SetEmail addr next) = do
    put (EnteringEmail addr)
    pure next
eval (ConfirmEmail addr next) = do
    put (RequestingCode addr)
    lift (validateEmailAddress addr)
    put (EnteringCode addr "")
    pure next
eval (SetCode addr code next) = do
    put (EnteringCode addr code)
    pure next
eval (ConfirmCode addr code next) = do
    put (ConfirmingCode addr code)
    tok <- lift (confirmEmailAddress addr code)
    put (EnteringPassword addr tok "" "")
    pure next
eval (SetPasswords addr token pw1 pw2 next) = do
    put (EnteringPassword addr token pw1 pw2)
    pure next
eval (SavePassword addr token pw next) = do
    put (SettingPassword addr token)
    lift (setPassword token pw)
    put (Success addr)
    pure next
eval (Reset addr next) = do
    put (EnteringEmail addr)
    pure next

--
--
--

email :: String -> EmailAddress
email = EmailAddress <<< { unEmailAddress: _ }
