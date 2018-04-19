{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Github.Server.Repo (
    repoPRs,
    repoPR
) where

import HUD.Data
import HUD.Github.Types

import Control.Arrow ((&&&))
import Control.Monad.Trans (liftIO)
import Data.Bifunctor (second)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (toList)
import GitHub.Auth (Auth(OAuth))
import GitHub.Data.Definitions (Error, SimpleUser(..))
import qualified GitHub.Data.GitData as GD
import GitHub.Data.Id (Id(..), untagId)
import GitHub.Data.Name (Name(N), untagName)
import GitHub.Data.Options (IssueState(..))
import GitHub.Data.PullRequests (PullRequest(..), SimplePullRequest(..))
import GitHub.Data.URL (getUrl)
import GitHub.Endpoints.PullRequests (pullRequest', pullRequestCommits', pullRequestsFor')
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (concurrently)
import UnliftIO.Exception (throwIO)

--
--
--

repoPRs :: MonadUnliftIO m => GithubAccount -> GithubRepo -> GithubToken -> m [PR]
repoPRs (GithubAccount account) (GithubRepo name) (GithubOAuthToken tok) = liftIO $ do
    let oauth = OAuth (encodeUtf8 tok)
    res <- pullRequestsFor' (pure oauth) (N account) (N name)
    fromGithubResponse (fmap toPR . toList) res

--
--
--

repoPR :: MonadUnliftIO m => (GithubAccount, GithubRepo, GithubPR) -> GithubToken -> m PRDetails
repoPR ((GithubAccount account), (GithubRepo name), (GithubPR prid)) (GithubOAuthToken tok) = liftIO $ do
    let oauth = OAuth (encodeUtf8 tok)
    let getPR = pullRequest' (pure oauth) (N account) (N name) (Id prid)
    let getCommits = pullRequestCommits' (pure oauth) (N account) (N name) (Id prid)
    (pr, commits) <- concurrently getPR getCommits
    let res = (,) <$> pr <*> commits
    fromGithubResponse (uncurry toPRDetails . second toList) res

--
--
--

fromGithubResponse :: MonadUnliftIO m => (a -> b) -> Either Error a -> m b
fromGithubResponse _ (Left e) = throwIO (GithubError e)
fromGithubResponse f (Right a) = pure (f a)

--
--
--

toPR :: SimplePullRequest -> PR
toPR r = PR {
    prID = (PRID . untagId) (simplePullRequestId r),
    prNumber = GithubPR (simplePullRequestNumber r),
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

--
--
--

toPRDetails :: PullRequest -> [GD.Commit] -> PRDetails
toPRDetails r cx = PRDetails {
    prdPR = PR {
        prID = (PRID . untagId) (pullRequestId r),
        prNumber = GithubPR (pullRequestNumber r),
        prCreatedAt = pullRequestCreatedAt  r,
        prUpdatedAt = pullRequestUpdatedAt r,
        prClosedAt = pullRequestClosedAt r,
        prMergedAt = pullRequestMergedAt r,
        prUser = toGithubAccount (pullRequestUser r),
        prHtmlURL = getUrl (pullRequestHtmlUrl r),
        prTitle = pullRequestTitle r,
        prBody = pullRequestBody r,
        prState = toPRState (pullRequestState r),
        prAssignees = toGithubAccount <$> toList (pullRequestAssignees r),
        prReviewers = toGithubAccount <$> toList (pullRequestRequestedReviewers r)
    },
    prdCommits = toCommit <$> cx
}

--

toCommit :: GD.Commit -> Commit
toCommit c = Commit {
    cmSHA = (CommitSHA . untagName) (GD.commitSha c),
    cmURL = getUrl (GD.commitUrl c),
    cmCommitter = toGithubAccount <$> GD.commitCommitter c,
    cmAuthor = toGithubAccount <$> GD.commitAuthor c,
    cmStats = toStats <$> GD.commitStats c,
    cmMessage = GD.gitCommitMessage (GD.commitGitCommit c)
}

toStats :: GD.Stats -> CommitStats
toStats s = CommitStats {
    cmsAdditions = GD.statsAdditions s,
    cmsDeletions = GD.statsDeletions s,
    cmsTotal = GD.statsTotal s
}

--
--
--

toPRState :: IssueState -> PRState
toPRState StateOpen = PROpen
toPRState StateClosed = PRClosed

toGithubAccount :: SimpleUser -> (GithubAccount, AvatarURL)
toGithubAccount = (GithubAccount . untagName . simpleUserLogin &&& AvatarURL . getUrl . simpleUserAvatarUrl)
