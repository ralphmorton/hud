{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HUD.Context (
    Context,
    viewC,
    emptyC,
    (>>+),
    (+<<),
    mkSqlPool,
    mkMongoPool,
    mkAmqpPool,
    mkRedisPool,
    mkHttp,
    mkSendGridV2
) where

import HUD.Operational

import Control.Lens (view)
import Control.Monad.Reader (MonadReader)
import qualified Data.ByteString.Char8 as B
import Data.Context
import Data.Proxy
import Data.Time (NominalDiffTime)
import System.Environment (getEnv)

--
--
--

viewC :: (MonadReader (Context r) m, HasContextLens r a a) => m a
viewC = view (contextLens Proxy)

--
--
--

emptyC :: IO (Context '[])
emptyC = pure EmptyContext

--
--
--

(>>+) :: (Applicative m, Contains c a ~ 'False) => m (Context c) -> m a -> m (Context (a ': c))
(>>+) mc ma = (:.) <$> ma <*> mc

infixl 5 >>+

(+<<) :: (Applicative m, Contains c a ~ 'False) => m a -> m (Context c) -> m (Context (a ': c))
(+<<) = flip (>>+)

infixr 5 +<<

--
--
--

mkSqlPool :: Int -> IO SqlPool
mkSqlPool n = createPostgresPool n . B.pack =<< getEnv "DATABASE_URL"

--

mkMongoPool :: Int -> Int -> NominalDiffTime -> IO MongoPool
mkMongoPool size stripes ttl = createMongoPool size stripes ttl =<< getEnv "MONGODB_URI"

--

mkAmqpPool :: Int -> Int -> NominalDiffTime -> IO AmqpPool
mkAmqpPool size stripes ttl = createAMQPPool size stripes ttl =<< getEnv "CLOUDAMQP_URL"

--

mkRedisPool :: IO RedisPool
mkRedisPool = createRedisPool =<< getEnv "REDIS_URL"

--

mkHttp :: IO HttpManager
mkHttp = createHttpManager

--

mkSendGridV2 :: IO SendGridCreds
mkSendGridV2 = do
    username <- B.pack <$> getEnv "SENDGRID_USERNAME"
    password <- B.pack <$> getEnv "SENDGRID_PASSWORD"
    pure . SendGridV2 $ SendGridV2Creds {
        sgv2User = username,
        sgv2Key = password
    }
