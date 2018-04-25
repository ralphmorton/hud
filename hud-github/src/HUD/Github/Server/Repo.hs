{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Github.Server.Repo (
    repos,
    repoPRs,
    repoPR
) where

import HUD.Data
import HUD.Data.HUD.Github
import HUD.Names (Github)

import Control.Arrow ((&&&))
import Control.Monad ((<=<))
import Control.Monad.Trans (liftIO)
import Data.Monoid
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (toList)
import GitHub.Auth (Auth(OAuth))
import qualified GitHub.Data.Comments as GD
import GitHub.Data.Definitions (SimpleOwner(..), SimpleUser(..))
import qualified GitHub.Data.GitData as GD
import GitHub.Data.Id (Id(..), untagId)
import qualified GitHub.Data.Issues as GD
import GitHub.Data.Name (Name(N), untagName)
import GitHub.Data.Options (IssueState(..), sortByUpdated, stateOpen)
import GitHub.Data.PullRequests (PullRequest(..), SimplePullRequest(..))
import qualified GitHub.Data.Repos as GD
import GitHub.Data.Request (FetchCount(..))
import GitHub.Data.URL (getUrl)
import GitHub.Endpoints.Issues.Comments (commentsR)
import GitHub.Endpoints.PullRequests (pullRequestR, pullRequestCommitsR, pullRequestsForR)
import GitHub.Endpoints.PullRequests.Comments (pullRequestCommentsR)
import GitHub.Endpoints.Repos (currentUserReposR)
import GitHub.Request (Request, executeRequest)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (concurrently)
import UnliftIO.Exception (throwIO)

--
--
--

repos :: MonadUnliftIO m => OAuthToken Github -> m [(Account, Repo)]
repos (OAuthToken tok) = do
    let auth = OAuth (encodeUtf8 tok)
    let req = request auth (currentUserReposR GD.RepoPublicityAll FetchAll)
    fmap (repoAccount &&& repoRepo) . toList <$> req

--

repoAccount :: GD.Repo -> Account
repoAccount = Account . untagName . simpleOwnerLogin . GD.repoOwner

repoRepo :: GD.Repo -> Repo
repoRepo = Repo . untagName . GD.repoName

--
--
--

repoPRs :: MonadUnliftIO m => OAuthToken Github -> Account -> Repo -> m [PR]
repoPRs (OAuthToken tok) (Account account) (Repo name) = do
    let auth = OAuth (encodeUtf8 tok)
    let opts = stateOpen <> sortByUpdated
    let req = pullRequestsForR (N account) (N name) opts (FetchAtLeast 50)
    fmap simplePR . toList <$> request auth req

--

simplePR :: SimplePullRequest -> PR
simplePR r = PR {
    prID = (PRID . untagId) (simplePullRequestId r),
    prNumber = PRNum (simplePullRequestNumber r),
    prCreatedAt = simplePullRequestCreatedAt  r,
    prUpdatedAt = simplePullRequestUpdatedAt r,
    prClosedAt = simplePullRequestClosedAt r,
    prMergedAt = simplePullRequestMergedAt r,
    prUser = simpleUser (simplePullRequestUser r),
    prHtmlURL = getUrl (simplePullRequestHtmlUrl r),
    prTitle = simplePullRequestTitle r,
    prBody = simplePullRequestBody r,
    prState = issueState (simplePullRequestState r),
    prAssignees = simpleUser <$> toList (simplePullRequestAssignees r),
    prReviewers = simpleUser <$> toList (simplePullRequestRequestedReviewers r)
}

--
--
--

repoPR :: MonadUnliftIO m => OAuthToken Github -> Account -> Repo -> PRNum -> m PRDetails
repoPR (OAuthToken tok) (Account account) (Repo name) (PRNum num) = do
    let auth = OAuth (encodeUtf8 tok)
    let prReq = pullRequestR (N account) (N name) (Id num)
    let commitsReq = pullRequestCommitsR (N account) (N name) (Id num) FetchAll
    let commentsReq = pullRequestCommentsR (N account) (N name) (Id num) FetchAll
    let issueCommentsReq = commentsR (N account) (N name) (Id num) FetchAll
    ((p, cx), (cmx, icmx)) <- concurrently
            (concurrently (request auth prReq) (request auth commitsReq))
            (concurrently (request auth commentsReq) (request auth issueCommentsReq))
    pure PRDetails {
        prdPR = pr p,
        prdCommits = commit <$> toList cx,
        prdComments = comment <$> toList cmx,
        prdIssueComments = issueComment <$> toList icmx
    }

--

pr :: PullRequest -> PR
pr r = PR {
    prID = (PRID . untagId) (pullRequestId r),
    prNumber = PRNum (pullRequestNumber r),
    prCreatedAt = pullRequestCreatedAt  r,
    prUpdatedAt = pullRequestUpdatedAt r,
    prClosedAt = pullRequestClosedAt r,
    prMergedAt = pullRequestMergedAt r,
    prUser = simpleUser (pullRequestUser r),
    prHtmlURL = getUrl (pullRequestHtmlUrl r),
    prTitle = pullRequestTitle r,
    prBody = pullRequestBody r,
    prState = issueState (pullRequestState r),
    prAssignees = simpleUser <$> toList (pullRequestAssignees r),
    prReviewers = simpleUser <$> toList (pullRequestRequestedReviewers r)
}

--

commit :: GD.Commit -> Commit
commit c = Commit {
    cmSHA = (CommitSHA . untagName) (GD.commitSha c),
    cmURL = getUrl (GD.commitUrl c),
    cmCommitter = simpleUser <$> GD.commitCommitter c,
    cmAuthor = simpleUser <$> GD.commitAuthor c,
    cmStats = stats <$> GD.commitStats c,
    cmMessage = GD.gitCommitMessage (GD.commitGitCommit c)
}

stats :: GD.Stats -> CommitStats
stats s = CommitStats {
    cmsAdditions = GD.statsAdditions s,
    cmsDeletions = GD.statsDeletions s,
    cmsTotal = GD.statsTotal s
}

--

issueComment :: GD.IssueComment -> IssueComment
issueComment c = IssueComment {
    icoID = IssueCommentID (GD.issueCommentId c),
    icoCreatedAt = GD.issueCommentCreatedAt c,
    icoUpdatedAt = GD.issueCommentUpdatedAt c,
    icoURL = getUrl (GD.issueCommentUrl c),
    icoHtmlURL = getUrl (GD.issueCommentHtmlUrl c),
    icoUser = simpleUser (GD.issueCommentUser c),
    icoBody = GD.issueCommentBody c
}

--

comment :: GD.Comment -> Comment
comment c = Comment {
    coID = (CommentID . untagId) (GD.commentId c),
    coPosition = GD.commentPosition c,
    coLine = GD.commentLine c,
    coCreatedAt = GD.commentCreatedAt c,
    coUpdatedAt = GD.commentUpdatedAt c,
    coURL = getUrl (GD.commentUrl c),
    coHtmlURL = getUrl <$> GD.commentHtmlUrl c,
    coUser = simpleUser (GD.commentUser c),
    coBody = GD.commentBody c
}

--
--
--

request :: MonadUnliftIO m => Auth -> Request k a -> m a
request auth = either throwIO pure <=< liftIO . executeRequest auth

--
--
--

issueState :: IssueState -> PRState
issueState StateOpen = PROpen
issueState StateClosed = PRClosed

simpleUser :: SimpleUser -> (Account, AvatarURL)
simpleUser = (Account . untagName . simpleUserLogin &&& AvatarURL . getUrl . simpleUserAvatarUrl)
