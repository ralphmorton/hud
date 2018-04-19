{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Github.Server.Repo (
    repoOverview
) where

import HUD.Data
import HUD.Github.Types

import Control.Monad.Trans (liftIO)
import Data.Text.Encoding (encodeUtf8)
import GitHub.Auth (Auth(OAuth))
import GitHub.Data.Name (Name(N))
import GitHub.Data.Repos (Repo(..))
import GitHub.Data.URL (getUrl)
import GitHub.Endpoints.Repos (repository')
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO)

--
--
--

repoOverview :: MonadUnliftIO m => GithubAccount -> GithubRepo -> GithubToken -> m RepoOverview
repoOverview (GithubAccount account) (GithubRepo name) (GithubOAuthToken tok) = do
    let oauth = OAuth (encodeUtf8 tok)
    res <- liftIO $ repository' (pure oauth) (N account) (N name)
    either (throwIO . GithubError) (pure . buildRepoOverview) res

--

buildRepoOverview :: Repo -> RepoOverview
buildRepoOverview r = RepoOverview {
    roDescription = repoDescription r,
    roCreatedAt = repoCreatedAt r,
    roHtmlUrl = getUrl (repoHtmlUrl r)
}
