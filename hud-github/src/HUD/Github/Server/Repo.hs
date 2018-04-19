{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Github.Server.Repo (
    repoPRs
) where

import HUD.Data
import HUD.Github.Types

import Control.Arrow ((&&&))
import Control.Monad.Trans (liftIO)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (toList)
import GitHub.Auth (Auth(OAuth))
import GitHub.Data.Definitions (SimpleUser(..))
import GitHub.Data.Id (untagId)
import GitHub.Data.Name (Name(N), untagName)
import GitHub.Data.Options (IssueState(..))
import GitHub.Data.PullRequests (SimplePullRequest(..))
import GitHub.Data.URL (getUrl)
import GitHub.Endpoints.PullRequests (pullRequestsFor')
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO)

--
--
--

repoPRs :: MonadUnliftIO m => GithubAccount -> GithubRepo -> GithubToken -> m [PR]
repoPRs (GithubAccount account) (GithubRepo name) (GithubOAuthToken tok) = do
    let oauth = OAuth (encodeUtf8 tok)
    res <- liftIO $ pullRequestsFor' (pure oauth) (N account) (N name)
    either (throwIO . GithubError) (pure . fmap toPR . toList) res

--

toPR :: SimplePullRequest -> PR
toPR r = PR {
    prID = (PRID . untagId) (simplePullRequestId r),
    prNumber = PRNum (simplePullRequestNumber r),
    prCreatedAt = simplePullRequestCreatedAt  r,
    prUpdatedAt = simplePullRequestUpdatedAt r,
    prClosedAt = simplePullRequestClosedAt r,
    prMergedAt = simplePullRequestMergedAt r,
    prUser = toGithubAccount (simplePullRequestUser r),
    prHtmlURL = getUrl (simplePullRequestHtmlUrl r),
    prTitle = simplePullRequestTitle r,
    prBody = simplePullRequestBody r,
    prState = toPRState (simplePullRequestState r),
    prAssignees = toGithubAccount <$> toList (simplePullRequestAssignees r),
    prReviewers = toGithubAccount <$> toList (simplePullRequestRequestedReviewers r)
}

toGithubAccount :: SimpleUser -> (GithubAccount, AvatarURL)
toGithubAccount = (GithubAccount . untagName . simpleUserLogin &&& AvatarURL . getUrl . simpleUserAvatarUrl)

toPRState :: IssueState -> PRState
toPRState StateOpen = PROpen
toPRState StateClosed = PRClosed
