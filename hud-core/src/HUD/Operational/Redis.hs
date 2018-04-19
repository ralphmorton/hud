{-# LANGUAGE FlexibleContexts #-}

module HUD.Operational.Redis (
    RedisPool,
    createRedisPool,
    redis
) where

import Control.Lens (view)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.Context
import Data.Proxy
import Database.Redis (Connection, ConnectInfo(..), PortID(PortNumber), Redis, connect, defaultConnectInfo, runRedis)
import Text.Parsec
import UnliftIO (MonadUnliftIO)

--
--
--

type RedisPool = Connection

--
--
--

createRedisPool :: MonadUnliftIO m => String -> m RedisPool
createRedisPool connStr = case parseConnectionString connStr of
    Nothing -> error ("Malformed connection string: " ++ connStr)
    Just (host, port, pw) -> liftIO $ connect defaultConnectInfo {
        connectHost = host,
        connectPort = PortNumber (read port),
        connectAuth = Just $ B.pack pw
    }

parseConnectionString :: String -> Maybe (String, String, String)
parseConnectionString = either (const Nothing) Just . parse parser ""
    where
    parser = do
        _ <- string "redis://"
        _ <- many (noneOf ":")
        _ <- char ':'
        pw <- many (noneOf "@")
        _ <- char '@'
        host <- many (noneOf ":")
        _ <- char ':'
        port <- many anyChar
        return (host, port, pw)

--
--
--

redis :: (
    MonadUnliftIO m,
    MonadReader (Context r) m,
    HasContextLens r RedisPool RedisPool) => Redis a -> m a
redis f = liftIO . flip runRedis f =<< view (contextLens Proxy)
