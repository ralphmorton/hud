
module HUD.Frontend.Web.Navigate (
    NAVIGATE,
    navigate
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

--
--
--

navigate :: forall m e a. MonadAff (navigate :: NAVIGATE | e) m => IsRoute a => a -> m Unit
navigate = liftEff <<< navigate_ <<< toPath
