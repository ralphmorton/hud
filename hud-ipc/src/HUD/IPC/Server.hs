{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HUD.IPC.Server (
    Result(..),
    RequeueBehaviour(..),
    module HUD.IPC.Types,
    serveIPC
) where

import Prelude hiding (log)

import HUD.Data
import HUD.Logging
import HUD.Operational
import HUD.QueueEndpoint
import HUD.IPCProvider
import HUD.IPC.Types

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException)
import Control.Monad (forever, void)
import Control.Monad.Trans (liftIO)
import Data.Aeson (FromJSON, ToJSON, Value, decode, encode, parseJSON)
import Data.Aeson.Types (parseMaybe)
import Data.Foldable (traverse_)
import qualified Data.Map as M
import Data.Proxy
import Data.Text (Text, pack)
import Data.Void (Void)
import GHC.TypeLits
import Network.AMQP hiding (queueName)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (catchAny, handleAny)

--
--
--

type Handler m c = IPCRequest c -> m (Result (IPCResponse c))

--
--
--

data Result a
    = Failure RequeueBehaviour
    | Success a

instance Functor Result where
    fmap _ (Failure b) = Failure b
    fmap f (Success a) = Success (f a)

--
--
--

serveIPC :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r MinLogLevel,
    QueueEndpoint c,
    KnownSymbol (QueueName c),
    KnownSymbol (DeadLetterQueueName c),
    IPCProvider c,
    FromJSON (IPCRequest c),
    ToJSON (IPCResponse c)) => Handler m c -> m ()
serveIPC = serveIPC' (Proxy :: Proxy c)

--

serveIPC' :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r MinLogLevel,
    QueueEndpoint c,
    KnownSymbol (QueueName c),
    KnownSymbol (DeadLetterQueueName c),
    IPCProvider c,
    FromJSON (IPCRequest c),
    ToJSON (IPCResponse c)) => Proxy c -> Handler m c -> m ()
serveIPC' p handler = do
    declareQueues p
    forever $ do
        catchAny (void $ runIPC p handler) $ \e -> do
            logIPCException p e
            liftIO (threadDelay 10000000)

--

logIPCException :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r MinLogLevel,
    QueueEndpoint c,
    KnownSymbol (QueueName c)) => Proxy c -> SomeException -> m ()
logIPCException p e = log Log {
    logTime = (),
    logLevel = Error,
    logSource = "HUD.IPC.Server.logIPCException",
    logTitle = "An unhandled IPC server exception occurred",
    logData = M.fromList $ [
        ("queue", queueName p),
        ("exception", pack $ show e)
    ]
}

--
--
--

runIPC :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    QueueEndpoint c,
    KnownSymbol (QueueName c),
    IPCProvider c,
    FromJSON (IPCRequest c),
    ToJSON (IPCResponse c)) => Proxy c -> Handler m c -> m Void
runIPC p handler = amqp $ \chan -> consume chan q Ack (uncurry $ handle chan)
    where
    q = queueName p
    handle chan msg env = do
        hres <- runHandler p msg handler
        case hres of
            Failure (Reject rq) -> liftIO (rejectEnv env rq)
            Failure Acknowledge -> liftIO (ackEnv env)
            Success res -> do
                flip traverse_ (msgReplyTo msg) $ \rkey -> do
                    void . liftIO . forkIO . void . publishMsg chan "" rkey $ newMsg {
                        msgBody = encode res,
                        msgDeliveryMode = Just Persistent
                    }
                liftIO (ackEnv env)
        pure Continue

--
--
--

runHandler :: (
    MonadUnliftIO m,
    IPCProvider c,
    FromJSON (IPCRequest c),
    ToJSON (IPCResponse c)) => Proxy c -> Message -> Handler m c -> m (Result (Text, IPCResult (IPCResponse c)))
runHandler p msg handler = case decodeEnv msg of
    Nothing -> pure (Failure Acknowledge)
    Just (rid, v) -> case parseMaybe parseJSON v of
        Nothing -> (pure . Success) (rid, IPCRequestDecodeFailure)
        Just q -> handleAny (pure . handlerExc p) $ do
            r <- handler q
            pure $ ((rid,) . IPCResult) <$> r

--

handlerExc :: (
    IPCProvider c,
    FromJSON (IPCRequest c),
    ToJSON (IPCResponse c)) => Proxy c -> SomeException -> Result (Text, IPCResult a)
handlerExc p e = Failure (onHandlerExc p e)

--

decodeEnv :: Message -> Maybe (Text, Value)
decodeEnv = decode . msgBody
