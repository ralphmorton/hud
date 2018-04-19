{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (log)

import HUD.Context
import HUD.Data (EmailAddress(..))
import HUD.Logging (mkMinLogLevel)
import HUD.Identity.Verifier
import HUD.Identity.Signer
import HUD.Identity.Server

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString.Char8 as B
import Data.Foldable (traverse_)
import Data.Proxy
import Data.Text (pack)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import System.Environment

--
--
--

main :: IO ()
main = do
    port <- read <$> getEnv "PORT"
    nvx <- (read <$> getEnv "NUM_VERIFIERS" :: IO Int)
    nsx <- (read <$> getEnv "NUM_SIGNERS" :: IO Int)
    c <- buildCtx
    let verifier' = runReaderT verifier c
    let signer' = runReaderT signer c
    traverse_ (const . void $ forkIO verifier') [1..nvx]
    traverse_ (const . void $ forkIO signer') [1..nsx]
    let server' = serve (Proxy :: Proxy API) (server c)
    run port (cors (const $ Just corsPolicy) server')
    where
    buildCtx =
        mkSenderAddr +<<
        mkHMACKey +<<
        mkRedisPool +<<
        mkAmqpPool 50 10 300 +<<
        mkHttp +<<
        mkMinLogLevel +<<
        emptyC

--

mkSenderAddr :: IO SenderAddr
mkSenderAddr = SenderAddr . EmailAddress . pack <$> getEnv "SENDER_ADDRESS"

--

mkHMACKey :: IO HMACKey
mkHMACKey = HMACKey . B.pack <$> getEnv "HMAC_KEY"

--
--
--

corsPolicy :: CorsResourcePolicy
corsPolicy = CorsResourcePolicy {
    corsOrigins         = Nothing,
    corsMethods         = simpleMethods ++ ["PUT", "DELETE", "PATCH"],
    corsRequestHeaders  = ["Content-Type", "Authorization", "Host", "User-Agent", "Origin", "Referer"],
    corsExposedHeaders  = Just ["*"],
    corsMaxAge          = Nothing,
    corsIgnoreFailures  = False,
    corsRequireOrigin   = False,
    corsVaryOrigin      = False
}
