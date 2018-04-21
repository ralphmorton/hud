{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Dashboard.Server.Auth (
    login,
    setPassword
) where

import Prelude hiding (log)

import HUD.Data
import HUD.Operational
import HUD.Names ()
import HUD.IPC.Client (IPCResult(IPCResult), ipc)
import HUD.Logging
import HUD.Dashboard.Data
import HUD.Dashboard.Persistence

import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import qualified Crypto.KDF.BCrypt as H
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO)

--
--
--

login :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r SqlPool,
    HasContext r MinLogLevel) => EmailAddress -> Text -> m (NominalDiffTime, Token)
login addr password = do
    mUser <- (sql . runMaybeT) (getUserByEmail addr)
    case mUser of
        Nothing ->
            throwIO UnknownUser
        Just (_, user) -> case userPassword user of
            Nothing ->
                throwIO BadPassword
            Just hash -> case checkPassword password hash of
                False -> throwIO BadPassword
                True -> createToken addr

--

checkPassword :: Text -> PasswordHash -> Bool
checkPassword password hash = H.validatePassword pw h
    where
    pw = B.pack (unpack password)
    h = (B.pack . unpack) (unPasswordHash hash)

--

createToken :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r MinLogLevel) => EmailAddress -> m (NominalDiffTime, Token)
createToken addr = do
    res <- ipc 10 (EmailIdentity addr, (14400 :: NominalDiffTime))
    case res of
        Just (IPCResult tok) -> pure (14400, tok)
        _ -> logTokenCreationFailure addr res *> throwIO InternalFailure

logTokenCreationFailure :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r MinLogLevel) => EmailAddress -> Maybe (IPCResult Token) -> m ()
logTokenCreationFailure addr res = log Log {
    logTime = (),
    logLevel = Error,
    logSource = "HUD.Dashboard.Server.Auth.logTokenCreationFailure",
    logTitle = "User token generation failed",
    logData = M.fromList $ [
        ("email", pack $ show addr),
        ("result", pack $ show res)
    ]
}

--
--
--

setPassword :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r AmqpPool,
    HasContext r SqlPool) => UserKey -> Text -> m ()
setPassword uk password
    | T.length password < 8 = throwIO IllegalPassword
    | otherwise = do
        let pw = B.pack (unpack password)
        hashed <- pack . B.unpack <$> liftIO (H.hashPassword 8 pw)
        (void . sql) (updateUserPassword uk (Just $ PasswordHash hashed))
