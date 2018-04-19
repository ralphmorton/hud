
module Main where

import HUD.Context
import HUD.Logging (mkMinLogLevel)
import HUD.Email.Send

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
    n <- (read <$> getEnv "NUM_SENDERS" :: IO Int)
    c <- buildCtx
    let f = runReaderT send c
    traverse_ (const . void $ forkIO f) [1..(n-1)]
    f
    where
    buildCtx =
        mkMongoPool 50 10 300 +<<
        mkAmqpPool 50 10 300 +<<
        mkHttp +<<
        mkSendGridV2 +<<
        mkMinLogLevel +<<
        emptyC
