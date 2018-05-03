
module HUD.Frontend.Web.Navigate (
    NAVIGATE,
    navigate,
    redirect,
    origin
) where

import Prelude

import HUD.Frontend.Web.Route (class IsRoute, toPath)

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Class (liftEff)

--
--
--

foreign import data NAVIGATE :: Effect

--

foreign import navigate_ :: forall e. String -> Eff (navigate :: NAVIGATE | e) Unit

foreign import redirect_ :: forall e. String -> Eff (navigate :: NAVIGATE | e) Unit

foreign import origin_ :: forall e. Eff (navigate :: NAVIGATE | e) String

--
--
--

navigate :: forall m e a. MonadAff (navigate :: NAVIGATE | e) m => IsRoute a => a -> m Unit
navigate = liftEff <<< navigate_ <<< toPath

--
--
--

redirect :: forall m e a. MonadAff (navigate :: NAVIGATE | e) m => String -> m Unit
redirect = liftEff <<< redirect_

--
--
--

origin :: forall m e a. MonadAff (navigate :: NAVIGATE | e) m => m String
origin = liftEff origin_
