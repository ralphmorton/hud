{-# LANGUAGE FlexibleContexts #-}

module HUD.Operational.SQL (
    SqlPool,
    createPostgresPool,
    sql
) where

import Control.Lens (view)
import Control.Monad.Logger (NoLoggingT(runNoLoggingT))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (liftIO)
import Data.ByteString (ByteString)
import Data.Context
import Data.Pool (Pool)
import Data.Proxy
import qualified Database.Persist.Postgresql as PG
import Database.Persist.Sql (SqlBackend, SqlPersistT, runSqlPool)
import UnliftIO (MonadUnliftIO)

--
--
--

type SqlPool = Pool SqlBackend

--
--
--

createPostgresPool :: MonadUnliftIO m => Int -> ByteString -> m SqlPool
createPostgresPool size connStr = liftIO (runNoLoggingT $ PG.createPostgresqlPool connStr size)

--
--
--

sql :: (
    MonadUnliftIO m,
    MonadReader (Context r) m,
    HasContextLens r SqlPool SqlPool) => SqlPersistT m a -> m a
sql m = runSqlPool m =<< view (contextLens Proxy)
