
module Main where

import Prelude

import HUD.Data.Identity (Token)
import HUD.Dashboard.Data.Relational (AccountKey)
import HUD.Frontend.Operational (Config, Context, IdentityInfo(..), OpM, runOp)
import HUD.Frontend.Network.HTTP (AJAX)
import HUD.Frontend.Router
import HUD.Frontend.Storage
import HUD.Frontend.Web.Navigate (NAVIGATE, navigate)
import HUD.Frontend.Component.Login as L
import HUD.Frontend.Component.Onboard as O
import HUD.Frontend.Component.Error as E
import HUD.Frontend.Component.Home as H
import HUD.Frontend.Component.Dashboard as D

import Control.Monad.Aff (Aff, never)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import Control.Monad.Error.Class (throwError)
import Data.DateTime (DateTime)
import Data.DateTime.Locale (LocalValue(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import DOM.WebStorage (STORAGE)
import Halogen (Component)
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.Component (hoist)
import Halogen.HTML (HTML)
import Halogen.VDom.Driver (runUI)

--
--
--

type Effects = HalogenEffects (
        ajax :: AJAX,
        console :: CONSOLE,
        navigate :: NAVIGATE,
        now :: NOW,
        route :: ROUTE,
        storage :: STORAGE
    )

--
--
--

foreign import loadConfig_ :: Eff Effects Config

--
--
--

main :: Eff Effects Unit
main = boot =<< loadConfig_

--
--
--

boot :: Config -> Eff Effects Unit
boot cfg = runHalogenAff do
    route <- match
    case route of
        Nothing -> throwError (error "No route matched")
        Just (Public r) -> case r of
            Login -> public runOp cfg L.comp unit
            Onboard -> public runOp cfg O.comp unit
            Error -> public idNat cfg E.comp unit
        Just (Authed r) -> case r of
            Home -> authed cfg H.comp unit

--

idNat :: forall m a. a -> m ~> m
idNat _ = id

--
--
--

public :: forall q i m.
    (Context Unit Unit -> m ~> Aff Effects) ->
    Config ->
    Component HTML q i Void m ->
    i -> Aff Effects Unit
public nat cfg comp i = void do
    let ctx = { i: unit, acc: unit, config: cfg }
    body <- awaitBody
    let comp' = hoist (nat ctx) comp
    runUI comp' i body

--
--
--

authed :: forall q i.
    Config ->
    Component HTML q i Void (OpM IdentityInfo Unit Effects) ->
    i -> Aff Effects Unit
authed cfg comp i = void do
    ctx <- { i: _, acc: unit, config: cfg } <<< IIUser <$> currentToken
    body <- awaitBody
    let comp' = hoist (runOp ctx) comp
    runUI comp' i body

--
--
--

dashboard :: forall q i.
    Config ->
    Component HTML q i Void (OpM IdentityInfo AccountKey Effects) ->
    i ->
    AuthedRoute ->
    AccountKey -> Aff Effects Unit
dashboard cfg comp i r acc = void do
    ctx <- { i: _, acc: acc, config: cfg } <<< IIUser <$> currentToken
    body <- awaitBody
    let comp' = hoist (runOp ctx) (D.comp comp)
    let input = { route: r, childInput: i }
    runUI comp' input body

--
--
--

currentToken :: Aff Effects Token
currentToken = do
    now <- getCurrentTime
    mres <- retrieveToken
    case mres of
        Just res | fst res > now ->
            pure (snd res)
        _ -> do
            navigate (Public Login)
            never

getCurrentTime :: Aff Effects DateTime
getCurrentTime = do
    res <- liftEff nowDateTime
    case res of
        LocalValue _ dt -> pure dt
