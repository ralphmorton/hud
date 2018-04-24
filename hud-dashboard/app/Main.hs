{-# LANGUAGE OverloadedStrings #-}

module Main where

import HUD.Context
import HUD.Operational
import HUD.Logging (mkMinLogLevel)
import HUD.Dashboard.API
import HUD.Dashboard.Data (migrate)
import HUD.Dashboard.Server

import Control.Monad.Reader (runReaderT)
import Data.Proxy (Proxy)
import Database.Persist.Sql (runMigration)
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
    flip runReaderT c $ do
        sql (runMigration migrate)
    let server' = serve (Proxy :: Proxy API) (server c)
    run port (cors (const $ Just corsPolicy) server')
    where
    buildCtx =
        mkAmqpPool 50 10 300 +<<
        mkSqlPool 50 +<<
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
