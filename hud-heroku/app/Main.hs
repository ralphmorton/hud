{-# LANGUAGE TupleSections #-}

module Main where

import HUD.Context
import HUD.Logging (mkMinLogLevel)
import HUD.Heroku.Authoriser

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
    na <- (read <$> getEnv "NUM_AUTHORISERS" :: IO Int)
    c <- buildCtx
    let authorise' = runReaderT authorise c
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
