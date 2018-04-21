{-# LANGUAGE TupleSections #-}

module Main where

import HUD.Context
import HUD.Logging (mkMinLogLevel)
import HUD.Github.Server
import HUD.Github.Authoriser

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
        mkGithubClient +<<
        emptyC

--

mkGithubClient :: IO GithubClient
mkGithubClient = do
    parts <- (,) <$> getEnv "GITHUB_CLIENT_ID" <*> getEnv "GITHUB_CLIENT_SECRET"
    (pure . uncurry GithubClient) (bimap pack pack parts)
