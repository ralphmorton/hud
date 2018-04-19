{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Operational.Mongo (
    MongoPool,
    MongoAuthException(..),
    createMongoPool,
    mongo
) where

import Control.Exception (Exception)
import Control.Lens (view)
import Control.Monad (mzero, void)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (liftIO)
import Data.Context
import Data.Pool (Pool, createPool, destroyResource, putResource, takeResource)
import Data.Proxy
import Data.Text (pack)
import Data.Time (NominalDiffTime)
import Database.MongoDB
import Text.Parsec
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (onException, throwIO)

--
--
--

type MongoPool = (Pool Pipe, Database)

--
--
--

data MongoAuthException
    = AuthFailed
    deriving Show

instance Exception MongoAuthException

--
--
--

data Mongo = Mongo {
    mHost :: Host,
    mUsername :: Username,
    mPassword :: Password,
    mDB :: Database
} deriving Show

--
--
--

createMongoPool :: MonadUnliftIO m => Int -> Int -> NominalDiffTime -> String -> m MongoPool
createMongoPool size stripes ttl connStr = case parseConnectionString connStr of
    Nothing -> error ("Malformed connection string: " ++ connStr)
    Just m -> fmap (,mDB m) . liftIO $ createPool (connectWith m) close stripes ttl size

--

parseConnectionString :: String -> Maybe Mongo
parseConnectionString = either (const Nothing) Just . parse parser ""
    where
    parser = do
        void (string "mongodb://")
        username <- many (noneOf ":")
        void (char ':')
        password <- many (noneOf "@")
        void (char '@')
        hostStr <- many (noneOf ":/")
        portStr <- option "27017" $ do
            void (char ':')
            many (noneOf "/")
        void (char '/')
        db <- many anyChar
        case readsPrec 0 portStr of
            [(portv,"")] -> pure Mongo {
                mHost = Host hostStr (PortNumber portv),
                mUsername = pack username,
                mPassword = pack password,
                mDB = pack db
            }
            _ -> mzero

--

connectWith :: Mongo -> IO Pipe
connectWith m = do
    let db = mDB m
    pipe <- connect (mHost m)
    res <- access pipe master db . useDb db $ auth (mUsername m) (mPassword m)
    case res of
        False -> throwIO AuthFailed
        True -> pure pipe

--
--
--

mongo :: (
    MonadUnliftIO m,
    MonadReader (Context r) m,
    HasContextLens r MongoPool MongoPool) => AccessMode -> Action m a -> m a
mongo mode f = do
    (pool, db) <- view (contextLens Proxy)
    (r, local) <- liftIO (takeResource pool)
    res <- onException (access r mode db f) (liftIO $ destroyResource pool local r)
    liftIO (putResource local r)
    pure res
