{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Operational.AMQP (
    AmqpPool,
    ConsumeResult(..),
    createAMQPPool,
    amqp,
    amqpQ,
    consume
) where

import Control.Exception (SomeException, fromException)
import Control.Lens (view)
import Control.Monad (void)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (liftIO)
import Data.Context
import Data.Foldable (traverse_)
import Data.Pool (Pool, createPool, destroyResource, putResource, takeResource)
import Data.Proxy
import Data.Text (Text, pack)
import Data.Time (NominalDiffTime)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Network.AMQP
import Text.Parsec
import UnliftIO (MonadUnliftIO(..))
import UnliftIO.Exception (handle, onException, throwIO)
import UnliftIO.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import UnliftIO.MVar (MVar, newEmptyMVar, putMVar, takeMVar)

-- NOTE: this could be made more efficient by
-- multiplexing channels, with the complexity
-- cost of needing to determine channel failure
-- root cause and potentially flush broken
-- connections.

--
--
--

newtype AmqpPool = AmqpPool (Pool (Connection, (Channel, TVar (Maybe Text))))

--
--
--

data ConsumeResult a
    = Continue
    | Done a

--
--
--

createAMQPPool :: MonadUnliftIO m => Int -> Int -> NominalDiffTime -> String -> m AmqpPool
createAMQPPool size stripes ttl connStr = liftIO $ do
    let mConnStr = parseAMQPConnectionString connStr
    (host, vhost, username, password) <- maybe (error "Malformed connection string") pure mConnStr
    AmqpPool <$> createPool
        (connect host vhost username password)
        (closeConnection . fst)
        stripes
        ttl
        size

--

parseAMQPConnectionString :: String -> Maybe (String, String, String, String)
parseAMQPConnectionString = either (const Nothing) Just . parse parser ""
    where
    parser = do
        void (string "amqp://")
        un <- many (noneOf ":")
        void (char ':')
        pw <- many (noneOf "@")
        void (char '@')
        host <- many (noneOf "/")
        vhost <- option "/" $ do
            void (char '/')
            many anyChar
        return (host, vhost, un, pw)

--

connect :: String -> String -> String -> String -> IO (Connection, (Channel, TVar (Maybe Text)))
connect host vhost username password = do
    qvar <- newTVarIO Nothing
    conn <- openConnection host (pack vhost) (pack username) (pack password)
    chan <- openChannel conn
    return (conn, (chan, qvar))

--
--
--

amqp :: (
    MonadUnliftIO m,
    MonadReader (Context r) m,
    HasContextLens r AmqpPool AmqpPool) => (Channel -> m a) -> m a
amqp f = do
    AmqpPool pool <- view (contextLens Proxy)
    (r, local) <- liftIO (takeResource pool)
    res <- onException (f . fst $ snd r) (liftIO $ destroyResource pool local r)
    liftIO (putResource local r)
    pure res

--
--
--

-- | Retrieve an AMQP channel and a channel-exclusive non-durable queue.
amqpQ :: (
    MonadUnliftIO m,
    MonadReader (Context r) m,
    HasContextLens r AmqpPool AmqpPool) => Text -> (Channel -> Text -> m a) -> m a
amqpQ exchange f = do
    AmqpPool pool <- view (contextLens Proxy)
    (r, local) <- liftIO (takeResource pool)
    let chan = fst (snd r)
    q <- resolveChanQueue exchange chan (snd $ snd r)
    res <- onException (f chan q) (liftIO $ destroyResource pool local r)
    liftIO (putResource local r)
    pure res

--

resolveChanQueue :: MonadUnliftIO m => Text -> Channel -> TVar (Maybe Text) -> m Text
resolveChanQueue exchange chan qvar = liftIO $ do
    current <- atomically (readTVar qvar)
    case current of
        Just q -> pure q
        Nothing -> do
            q <- toText <$> liftIO nextRandom
            void $ declareQueue chan newQueue {
                queueName = q,
                queueExclusive = True
            }
            bindQueue chan q exchange q
            atomically (writeTVar qvar $ pure q)
            pure q

--
--
--

-- | Synchronously consume from a channel.
consume :: MonadUnliftIO m => Channel -> Text -> Ack -> ((Message, Envelope) -> m (ConsumeResult a)) -> m a
consume chan q ack f = do
    evar <- newEmptyMVar
    ctag <- withRunInIO $ \run ->
        consumeMsgs chan q ack (run . performConsume evar f)
    liftIO $ addChannelExceptionHandler chan (putMVar evar . Left)
    c <- takeMVar evar
    liftIO (cancelConsumer chan ctag)
    either throwIO pure c

--

performConsume :: MonadUnliftIO m => MVar (Either SomeException a) -> ((Message, Envelope) -> m (ConsumeResult a)) -> (Message, Envelope) -> m ()
performConsume evar f m = handle (handleAMQP evar) $ do
    next <- f m
    case next of
        Done a -> putMVar evar (Right a)
        Continue -> pure ()

--

handleAMQP :: MonadUnliftIO m => MVar (Either SomeException a) -> SomeException -> m ()
handleAMQP evar e = do
    putMVar evar (Left e)
    traverse_ throwIO (toChanThreadKilled e)

--

toChanThreadKilled :: SomeException -> Maybe ChanThreadKilledException
toChanThreadKilled = fromException
