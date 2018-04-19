{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Logging (
    MinLogLevel(..),
    mkMinLogLevel,
    log
) where

import Prelude hiding (log)

import HUD.Context (viewC)
import HUD.Data
import HUD.Operational

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Data.Maybe (mapMaybe)
import Data.Monoid
import Data.List (intercalate)
import Data.Time (getCurrentTime)
import System.Environment (lookupEnv)
import UnliftIO (MonadUnliftIO)

--
--
--

newtype MinLogLevel = MinLogLevel {
    getMinLogLevel :: LogLevel
}

--
--
--

mkMinLogLevel :: IO MinLogLevel
mkMinLogLevel = maybe (MinLogLevel Debug) (MinLogLevel . read) <$> lookupEnv "MIN_LOG_LEVEL"

--
--
--

log :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r MinLogLevel) => LogReq -> m ()
log req = do
    MinLogLevel level <- viewC
    when (logLevel req >= level) $ do
        now <- liftIO getCurrentTime
        let l = req { logTime = now }
        logToConsole l

--
--
--

logToConsole :: MonadUnliftIO m => Log -> m ()
logToConsole l = liftIO (putStrLn line)
    where
    line = pref <> body
    pref = "<" <> show (logLevel l) <> "> "
    body = fields [
            ("time", pure . show $ logTime l),
            ("source", pure . show $ logSource l),
            ("title", pure . show $ logTitle l),
            ("data", pure . show $ logData l)
        ]

--

fields :: [(String, Maybe String)] -> String
fields = intercalate ", " . mapMaybe (uncurry field)
    where
    field n mv = do
        v <- mv
        pure (n <> "=" <> v)
