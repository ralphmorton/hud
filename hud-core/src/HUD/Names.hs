{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module HUD.Names (
    module HUD.QueueEndpoint,
    module HUD.IPCProvider
) where

import HUD.QueueEndpoint
import HUD.IPCProvider
import HUD.Data
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
