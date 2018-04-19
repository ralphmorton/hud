{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module HUD.IPCProvider (
    IPCProvider(..),
    RequeueBehaviour(..)
) where

import Control.Exception (SomeException)
import Data.Proxy

--
--
--

class IPCProvider a where
    type IPCRequest a = q | q -> a
    type IPCResponse a :: *
    onHandlerExc :: Proxy a -> SomeException -> RequeueBehaviour

--
--
--

data RequeueBehaviour
    = Reject Bool
    | Acknowledge
