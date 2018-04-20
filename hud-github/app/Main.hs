
module Main where

import HUD.Context
import HUD.Logging (mkMinLogLevel)
import HUD.Github.Server

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Data.Foldable (traverse_)
import System.Environment (getEnv)

--
--
--

main :: IO ()
main = do
    n <- (read <$> getEnv "NUM_SERVERS" :: IO Int)
    c <- buildCtx
    let f = runReaderT serveGithub c
    traverse_ (const . void $ forkIO f) [1..(n-1)]
    f
    where
    buildCtx =
        mkAmqpPool 50 10 300 +<<
        mkMinLogLevel +<<
        emptyC
