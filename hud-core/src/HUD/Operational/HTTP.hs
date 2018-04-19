{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Operational.HTTP (
    HttpManager,
    createHttpManager,
    http
) where

import Control.Lens (view)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Context
import Data.Proxy
import Network.HTTP.Conduit hiding (Proxy, http)
import UnliftIO (MonadUnliftIO)

--
--
--

type HttpManager = Manager

--
--
--

createHttpManager :: MonadUnliftIO m => m HttpManager
createHttpManager = liftIO (newManager tlsManagerSettings)

--
--
--

http :: (
    MonadUnliftIO m,
    MonadReader (Context r) m,
    HasContextLens r Manager Manager) => Request -> m (Response ByteString)
http req = liftIO . httpLbs req =<< view (contextLens Proxy)
