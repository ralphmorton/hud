{-# LANGUAGE TupleSections #-}

module Main where

import HUD.Context
import HUD.Logging (mkMinLogLevel)
import HUD.Trello.Server

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.Reader (runReaderT)
import Data.Foldable (traverse_)
import Data.Text (pack)
import System.Environment (getEnv)

--
--
--

main :: IO ()
main = do
    ns <- (read <$> getEnv "NUM_SERVERS" :: IO Int)
    c <- buildCtx
    let serve' = runReaderT serve c
    traverse_ (const . void $ forkIO serve') [1..ns]
    forever (threadDelay 1000000)
    where
    buildCtx =
        mkAmqpPool 50 10 300 +<<
        mkHttp +<<
        mkMinLogLevel +<<
        mkTrelloClientKey +<<
        emptyC

--

mkTrelloClientKey :: IO TrelloClientKey
mkTrelloClientKey = TrelloClientKey . pack <$> getEnv "TRELLO_CLIENT_KEY"
