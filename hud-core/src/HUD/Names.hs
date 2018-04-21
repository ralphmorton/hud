{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module HUD.Names (
    module HUD.QueueEndpoint,
    module HUD.IPCProvider,
    GithubAuth,
    Github
) where

import HUD.QueueEndpoint
import HUD.IPCProvider
import HUD.Data
import qualified HUD.Data.HUD.Github as GH
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
    type IPCRequest Github = (OAuthToken Github, GH.HUDReq)
    type IPCResponse Github = GH.HUDRsp
    onHandlerExc _ _ = Reject False
