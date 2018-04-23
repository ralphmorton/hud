
module HUD.Frontend.Storage (
    storeToken,
    retrieveToken
) where

import Prelude

import HUD.Data.Identity (Token)

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Class (liftEff)
import Data.DateTime (DateTime)
import Data.Generic (class Generic)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import DOM (DOM)
import DOM.WebStorage (STORAGE, getItem, getLocalStorage, setItem)

--
--
--

data Key a
    = KToken

derive instance genericKey :: Generic (Key a)

--
--
--

storeToken :: forall m e. MonadAff (dom :: DOM, storage :: STORAGE | e) m => Tuple DateTime Token -> m Unit
storeToken tok = liftEff do
    storage <- getLocalStorage
    setItem storage KToken tok

--
--
--

retrieveToken :: forall m e. MonadAff (dom :: DOM, storage :: STORAGE | e) m => m (Maybe (Tuple DateTime Token))
retrieveToken = liftEff do
    storage <- getLocalStorage
    getItem storage KToken
