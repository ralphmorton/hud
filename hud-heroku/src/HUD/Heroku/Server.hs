{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Heroku.Server (
    serve
) where

import Prelude hiding (log)

import HUD.Data (OAuthToken, Log'(..), LogLevel(..))
import HUD.Data.HUD.Heroku
import HUD.Operational
import HUD.Names (Heroku)
import HUD.Logging
import HUD.IPC.Server
import HUD.Heroku.Types
import HUD.Heroku.Server.Organisation

import Control.Monad.Catch (MonadThrow)
import qualified Data.Map as M
import Data.Text (pack)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (catch)

--
--
--

serve :: (
    MonadUnliftIO m,
    MonadThrow m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r HttpManager,
    HasContext r MinLogLevel,
    HasContext r HerokuClient) => m ()
serve = serveIPC (uncurry request)

--
--
--

request :: (
    MonadUnliftIO m,
    MonadThrow m,
    ContextReader r m,
    HasContext r HttpManager,
    HasContext r MinLogLevel,
    HasContext r HerokuClient) => OAuthToken Heroku -> HerokuReq -> m (Result HerokuRsp)
request tok req = catch (Success <$> runRequest tok req) $ \e -> do
    logRequestException req e
    (pure . Success) (HRRSFailure e)

--
--
--

runRequest :: (
    MonadUnliftIO m,
    MonadThrow m,
    ContextReader r m,
    HasContext r HttpManager,
    HasContext r HerokuClient) => OAuthToken Heroku -> HerokuReq -> m HerokuRsp
runRequest tok HRRQOrganisations =
    HRRSOrganisations <$> organisations tok

--
--
--

logRequestException :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r MinLogLevel) => HerokuReq -> HerokuRequestException -> m ()
logRequestException req e = log Log {
    logTime = (),
    logLevel = Error,
    logSource = "HUD.Heroku.Server.logRequestException",
    logTitle = "Heroku request failed",
    logData = M.fromList $ [
        ("request", (pack . show) req),
        ("exception", (pack . show) e)
    ]
}
