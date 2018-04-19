{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module HUD.QueueEndpoint (
    QueueEndpoint(..),
    queueName,
    deadLetterQueueName,
    declareQueues
) where

import HUD.Constants
import HUD.Operational

import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Data.Int (Int64)
import qualified Data.Map as M
import Data.Proxy
import Data.Text (Text, pack)
import Data.Time (NominalDiffTime)
import GHC.TypeLits
import Network.AMQP hiding (queueName)
import qualified Network.AMQP as A
import Network.AMQP.Types (FieldTable(..), FieldValue(..))
import UnliftIO (MonadUnliftIO)

--
--
--

class QueueEndpoint c where
    type QueueName c = (q :: Symbol) | q -> c
    type DeadLetterQueueName c = (q :: Symbol) | q -> c
    queueTTL :: Proxy c -> NominalDiffTime
    deadLetterTTL :: Proxy c -> NominalDiffTime

--
--
--

queueName :: (QueueEndpoint c, KnownSymbol (QueueName c)) => Proxy c -> Text
queueName = pack . symbolVal . queueProxy

--

queueProxy :: QueueEndpoint c => Proxy c -> Proxy (QueueName c)
queueProxy _ = Proxy

--
--
--

deadLetterQueueName :: (QueueEndpoint c, KnownSymbol (DeadLetterQueueName c)) => Proxy c -> Text
deadLetterQueueName = pack . symbolVal . deadLetterQueueProxy

--

deadLetterQueueProxy :: QueueEndpoint c => Proxy c -> Proxy (DeadLetterQueueName c)
deadLetterQueueProxy _ = Proxy

--
--
--

declareQueues :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    QueueEndpoint c,
    KnownSymbol (QueueName c),
    KnownSymbol (DeadLetterQueueName c)) => Proxy c -> m ()
declareQueues p = amqp $ \chan -> liftIO $ do
    declareExchanges chan
    void $ declareQueue chan newQueue {
        A.queueName = queueName p,
        queueHeaders = sendHeaders $ floor (queueTTL p) * 1000
    }
    void $ declareQueue chan newQueue {
        A.queueName = deadLetterQueueName p,
        queueHeaders = retryHeaders $ floor (deadLetterTTL p) * 1000
    }
    bindQueue chan (queueName p) amqpExchange (queueName p)
    bindQueue chan (deadLetterQueueName p) amqpDeadLetterExchange (queueName p)

--

declareExchanges :: MonadUnliftIO m => Channel -> m ()
declareExchanges chan = liftIO $ do
    declareExchange chan newExchange {
        exchangeName = amqpExchange,
        exchangeType = "direct"
    }
    declareExchange chan newExchange {
        exchangeName = amqpDeadLetterExchange,
        exchangeType = "direct"
    }

--

sendHeaders :: Int64 -> FieldTable
sendHeaders ttlMillis = FieldTable . M.fromList $ [
        (pack "x-message-ttl", FVInt64 ttlMillis),
        (pack "x-dead-letter-exchange", FVString amqpDeadLetterExchange)
    ]

--

retryHeaders :: Int64 -> FieldTable
retryHeaders ttlMillis = FieldTable . M.fromList $ [
        (pack "x-message-ttl", FVInt64 ttlMillis),
        (pack "x-dead-letter-exchange", FVString amqpExchange)
    ]
