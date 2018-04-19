{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.IPC.Client (
    module HUD.IPC.Types,
    ipc,
    ipc_,
    ipcExplicit_
) where

import HUD.Constants
import HUD.Operational
import HUD.QueueEndpoint
import HUD.IPCProvider
import HUD.IPC.Types

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Data.Aeson (FromJSON, ToJSON, Value, decode, encode, parseJSON)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Proxy
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (NominalDiffTime)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import GHC.TypeLits
import Network.AMQP hiding (queueName)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Timeout (timeout)

--
--
--

ipc :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    QueueEndpoint c,
    KnownSymbol (QueueName c),
    IPCProvider c,
    ToJSON (IPCRequest c),
    FromJSON (IPCResponse c)) => NominalDiffTime -> IPCRequest c -> m (Maybe (IPCResult (IPCResponse c)))
ipc = ipc' (Proxy :: Proxy c)

ipc' :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    QueueEndpoint c,
    KnownSymbol (QueueName c),
    IPCProvider c,
    ToJSON (IPCRequest c),
    FromJSON (IPCResponse c)) => Proxy c -> NominalDiffTime -> IPCRequest c -> m (Maybe (IPCResult (IPCResponse c)))
ipc' p ttl payload = amqpQ amqpExchange $ \chan replyTo -> do
    let q = queueName p
    let ttlNanos = floor ttl * 1000000
    rid <- toText <$> liftIO nextRandom
    void . liftIO . forkIO $ sendReq (Just replyTo) chan q (encode (rid, payload))
    timeout ttlNanos (consumeResult p chan replyTo rid)

--
--
--

ipc_ :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    QueueEndpoint c,
    KnownSymbol (QueueName c),
    IPCProvider c,
    ToJSON (IPCRequest c),
    FromJSON (IPCResponse c)) => IPCRequest c -> m ()
ipc_ = ipc_' (Proxy :: Proxy c)

--

ipc_' :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    QueueEndpoint c,
    KnownSymbol (QueueName c),
    IPCProvider c,
    ToJSON (IPCRequest c),
    FromJSON (IPCResponse c)) => Proxy c -> IPCRequest c -> m ()
ipc_' p payload = amqp $ \chan -> do
    let q = queueName p
    rid <- toText <$> liftIO nextRandom
    liftIO $ sendReq Nothing chan q (encode (rid,payload))

--
--
--

ipcExplicit_ :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    ToJSON a) => Text -> a -> m ()
ipcExplicit_ q payload = amqp $ \chan -> do
    rid <- toText <$> liftIO nextRandom
    liftIO $ sendReq Nothing chan q (encode (rid,payload))

--
--
--

sendReq :: Maybe Text -> Channel -> Text -> ByteString -> IO ()
sendReq replyTo chan q dta = do
    void $ publishMsg chan "" q newMsg {
        msgBody = dta,
        msgReplyTo = replyTo,
        msgDeliveryMode = Just Persistent
    }

--

consumeResult :: (
    MonadUnliftIO m,
    IPCProvider c,
    FromJSON (IPCResponse c)) => Proxy c -> Channel -> Text -> Text -> m (IPCResult (IPCResponse c))
consumeResult _ chan q rid = consume chan q NoAck (try . fst)
    where
    try msg = case decodeEnv msg of
        Nothing -> pure Continue
        Just (rid', v) -> case rid == rid' of
            False -> pure Continue
            True -> do
                case parseMaybe parseJSON v of
                    Nothing ->
                        pure . Done $ (IPCResultDecodeFailure . decodeUtf8) (toStrict $ msgBody msg)
                    Just r ->
                        pure (Done r)

--

decodeEnv :: Message -> Maybe (Text, Value)
decodeEnv = decode . msgBody
