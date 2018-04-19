{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Identity.Server.Email (
    SenderAddr(..),
    identifyEmail,
    confirmEmailIdentity
) where

import HUD.Context (viewC)
import HUD.Data
import HUD.Operational
import HUD.Names ()
import HUD.IPC.Client
import HUD.Identity.Crypto
import HUD.Identity.Server.Common

import Data.List.NonEmpty(NonEmpty(..))
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO)

--
--
--

newtype SenderAddr = SenderAddr {
    unSenderAddr :: EmailAddress
}

--
--
--

identifyEmail :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r RedisPool,
    HasContext r HttpManager,
    HasContext r SenderAddr) => EmailAddress -> m ()
identifyEmail addr = do
    SenderAddr sender <- viewC
    code <- genConfirmCode (unEmailAddress addr)
    ipc_ Email {
        emailFrom = EmailUser Nothing sender,
        emailTo = EmailUser Nothing addr :| [],
        emailCC = [],
        emailBCC = [],
        emailSubject = "Confirm your email",
        emailContent = TextOnly (TL.fromStrict code)
    }

--
--
--

confirmEmailIdentity :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r RedisPool,
    HasContext r HttpManager,
    HasContext r HMACKey) => EmailAddress -> Text -> m Token
confirmEmailIdentity addr code = do
    dta <- getConfirmCodeData code
    case dta == Just (unEmailAddress addr) of
        True -> encodeToken (EmailIdentity addr) tokenTTL
        False -> throwIO BadEmailToken
