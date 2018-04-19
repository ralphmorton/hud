{-# LANGUAGE OverloadedStrings #-}

module HUD.Data.Logging (
    Log,
    LogReq,
    Log'(..),
    LogLevel(..),
    logToDoc,
    logFromDoc
) where

import Prelude hiding (lookup)

import Data.Bson
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time (UTCTime)

--
--
--

type Log = Log' UTCTime
type LogReq = Log' ()

-- | A log entry.
data Log' a = Log {
    logTime :: a,
    logLevel :: LogLevel,
    logSource :: Text,
    logTitle :: Text,
    logData :: M.Map Text Text
} deriving (Eq, Show)

--
--
--

-- | Log level.
data LogLevel
    = Debug
    | Info
    | Warning
    | Error
    | Critical
    deriving (Eq, Ord, Read, Show)

instance Val LogLevel where
    val Debug = String "debug"
    val Info = String "info"
    val Warning = String "warning"
    val Error = String "error"
    val Critical = String "critical"
    cast' (String "debug") = pure Debug
    cast' (String "info") = pure Info
    cast' (String "warning") = pure Warning
    cast' (String "error") = pure Error
    cast' (String "critical") = pure Critical
    cast' _ = Nothing

--
--
--

logToDoc :: Log -> Document
logToDoc l =
    [
        "time" := val (logTime l),
        "level" := val (logLevel l),
        "source" := val (logSource l),
        "title" := val (logTitle l),
        "data" := Doc (foldr (uncurry insert) [] (M.toList $ logData l))
    ]
    where insert k v = (:) (k := val v)

--
--
--

logFromDoc :: Document -> Maybe Log
logFromDoc doc = do
    time <- lookup "time" doc
    level <- lookup "level" doc
    source <- lookup "source" doc
    title <- lookup "title" doc
    dmap <- foldr insert M.empty <$> (lookup "data" doc :: Maybe Document)
    pure Log {
        logTime = time,
        logLevel = level,
        logSource = source,
        logTitle = title,
        logData = dmap
    }
    where
    insert (l := String v) = M.insert l v
    insert _ = id
