{-# LANGUAGE TupleSections #-}

module Main where

import HUD.Context
import HUD.Logging (mkMinLogLevel)
import HUD.Heroku.Types
import HUD.Heroku.Authoriser
import HUD.Heroku.Server

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.Reader (runReaderT)
import Data.Bifunctor (bimap)
import Data.Foldable (traverse_)
import Data.Text (pack)
import System.Environment (getEnv)

--
--
--

main :: IO ()
main = do
    ns <- (read <$> getEnv "NUM_SERVERS" :: IO Int)
    na <- (read <$> getEnv "NUM_AUTHORISERS" :: IO Int)
    c <- buildCtx
    let serve' = runReaderT serve c
    let authorise' = runReaderT authorise c
    traverse_ (const . void $ forkIO serve') [1..ns]
    traverse_ (const . void $ forkIO authorise') [1..na]
    forever (threadDelay 1000000)
    where
    buildCtx =
        mkAmqpPool 50 10 300 +<<
        mkHttp +<<
        mkMinLogLevel +<<
        mkHerokuClient +<<
        emptyC

--

mkHerokuClient :: IO HerokuClient
mkHerokuClient = do
    parts <- (,) <$> getEnv "HEROKU_CLIENT_ID" <*> getEnv "HEROKU_CLIENT_SECRET"
    (pure . uncurry HerokuClient) (bimap pack pack parts)
