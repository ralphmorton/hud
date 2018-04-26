{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module HUD.Names (
    module HUD.QueueEndpoint,
    module HUD.IPCProvider,
    GithubAuth,
    Github,
    Trello,
    HerokuAuth,
    Heroku
) where

import HUD.QueueEndpoint
import HUD.IPCProvider
import HUD.Data
import HUD.Data.HUD.Github
import HUD.Data.HUD.Trello
import HUD.Data.HUD.Heroku
import HUD.Operational.Email (Email)

import Data.Time (NominalDiffTime)

--
--
--

-- | Identity verifier.
data IdentityVerifier

instance QueueEndpoint IdentityVerifier where
    type QueueName IdentityVerifier = "ipc.identityverifier"
    type DeadLetterQueueName IdentityVerifier = "ipc.identityverifier.dl"
    queueTTL _ = 300
    deadLetterTTL _ = 10

instance IPCProvider IdentityVerifier where
    type IPCRequest IdentityVerifier = Token
    type IPCResponse IdentityVerifier = Maybe Identity
    onHandlerExc _ _ = Acknowledge

-- | Identity signer.
data IdentitySigner

instance QueueEndpoint IdentitySigner where
    type QueueName IdentitySigner = "ipc.identitysigner"
    type DeadLetterQueueName IdentitySigner = "ipc.identitysigner.dl"
    queueTTL _ = 300
    deadLetterTTL _ = 10

instance IPCProvider IdentitySigner where
    type IPCRequest IdentitySigner = (Identity, NominalDiffTime)
    type IPCResponse IdentitySigner = Token
    onHandlerExc _ _ = Acknowledge

-- | Email sender
data EmailSender

instance QueueEndpoint EmailSender where
    type QueueName EmailSender = "ipc.emailsender"
    type DeadLetterQueueName EmailSender = "ipc.emailsender.dl"
    queueTTL _ = 86400
    deadLetterTTL _ = 60

instance IPCProvider EmailSender where
    type IPCRequest EmailSender = Email
    type IPCResponse EmailSender = ()
    onHandlerExc _ _ = Reject False

-- | Github authoriser
data GithubAuth

instance QueueEndpoint GithubAuth where
    type QueueName GithubAuth = "ipc.githubauth"
    type DeadLetterQueueName GithubAuth = "ipc.githubauth.dl"
    queueTTL _ = 86400
    deadLetterTTL _ = 60

instance IPCProvider GithubAuth where
    type IPCRequest GithubAuth = OAuthCode Github
    type IPCResponse GithubAuth = OAuthResult Github
    onHandlerExc _ _ = Reject False
    
-- | Github data provider
data Github

instance QueueEndpoint Github where
    type QueueName Github = "ipc.github"
    type DeadLetterQueueName Github = "ipc.github.dl"
    queueTTL _ = 86400
    deadLetterTTL _ = 60

instance IPCProvider Github where
    type IPCRequest Github = (OAuthToken Github, GithubReq)
    type IPCResponse Github = GithubRsp
    onHandlerExc _ _ = Reject False

-- | Trello data provider
data Trello

instance QueueEndpoint Trello where
    type QueueName Trello = "ipc.trello"
    type DeadLetterQueueName Trello = "ipc.trello.dl"
    queueTTL _ = 86400
    deadLetterTTL _ = 60

instance IPCProvider Trello where
    type IPCRequest Trello = (OAuthToken Trello, TrelloReq)
    type IPCResponse Trello = TrelloRsp
    onHandlerExc _ _ = Reject False

-- | Github authoriser
data HerokuAuth

instance QueueEndpoint HerokuAuth where
    type QueueName HerokuAuth = "ipc.herokuauth"
    type DeadLetterQueueName HerokuAuth = "ipc.herokuauth.dl"
    queueTTL _ = 86400
    deadLetterTTL _ = 60

instance IPCProvider HerokuAuth where
    type IPCRequest HerokuAuth = OAuthCode Heroku
    type IPCResponse HerokuAuth = OAuthResult Heroku
    onHandlerExc _ _ = Reject False

-- | Heroku data provider
data Heroku

instance QueueEndpoint Heroku where
    type QueueName Heroku = "ipc.heroku"
    type DeadLetterQueueName Heroku = "ipc.heroku.dl"
    queueTTL _ = 86400
    deadLetterTTL _ = 60

instance IPCProvider Heroku where
    type IPCRequest Heroku = (OAuthToken Heroku, HerokuReq)
    type IPCResponse Heroku = HerokuRsp
    onHandlerExc _ _ = Reject False