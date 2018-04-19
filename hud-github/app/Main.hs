{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Prelude hiding (log)

import HUD.Context
import HUD.Logging (mkMinLogLevel)
import HUD.Github.Server

import Data.Proxy
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
    c <- buildCtx
    let server' = serve (Proxy :: Proxy API) (server c)
    run port (cors (const $ Just corsPolicy) server')
    where
    buildCtx =
        mkAmqpPool 50 10 300 +<<
        mkHttp +<<
        mkMinLogLevel +<<
        emptyC

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
