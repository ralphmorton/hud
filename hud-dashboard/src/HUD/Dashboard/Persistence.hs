{-# LANGUAGE TupleSections #-}

module HUD.Dashboard.Persistence (
    listAccounts,
    getAccount,
    getAccountByName,
    createAccount,
    listAccountUsers,
    getUser,
    getUserByEmail,
    getUserAccounts,
    updateUserEmail,
    updateUserPassword,
    setUserGithubToken,
    createUser,
    linkUserToAccount,
    unlinkUserFromAccount,
    updateAccountUserLevel
) where

import HUD.Data
import HUD.Names (Github)
import HUD.Dashboard.Data

import Control.Arrow ((&&&))
import Control.Monad (mzero, (<=<))
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Persist
import Database.Persist.Sql
import UnliftIO (MonadUnliftIO)

--
--
--

-- | List all accounts.
listAccounts :: MonadUnliftIO m => SqlPersistT m [(AccountKey, Account)]
listAccounts = fmap toKeyedAccount <$> selectList [] []

-- | Get an account by ID.
getAccount :: MonadUnliftIO m => AccountKey -> MaybeT (SqlPersistT m) Account
getAccount = liftMaybe . (fmap . fmap) toAccount . get . AccountEntKey

-- | Get an account by name.
getAccountByName :: MonadUnliftIO m => Text -> MaybeT (SqlPersistT m) (AccountKey, Account)
getAccountByName = liftMaybe . (fmap . fmap) toKeyedAccount . getBy . UniqueAccount

-- | Create an account.
createAccount :: MonadUnliftIO m => Account -> SqlPersistT m AccountKey
createAccount c = do
    now <- liftIO getCurrentTime
    k <- makeKey AccountKey
    insertKey (AccountEntKey k) (fromAccount now c)
    pure k

--
--
--

-- | List all users for an account.
listAccountUsers :: MonadUnliftIO m => AccountKey -> SqlPersistT m [(UserKey, User)]
listAccountUsers k = do
    ukx <- fmap (accountUserEntUser . entityVal) <$> selectList [AccountUserEntAccount ==. AccountEntKey k] []
    fmap toKeyedUser <$> selectList [UserEntId <-. ukx] []

-- | Get a user by ID.
getUser :: MonadUnliftIO m => UserKey -> MaybeT (SqlPersistT m) User
getUser = liftMaybe . (fmap . fmap) toUser . get . UserEntKey

-- | Get users by email.
getUserByEmail :: MonadUnliftIO m => EmailAddress -> MaybeT (SqlPersistT m) (UserKey, User)
getUserByEmail = liftMaybe . (fmap . fmap) toKeyedUser . getBy . UniqueUser

-- | Get all accounts a user is party to.
getUserAccounts :: MonadUnliftIO m => UserKey -> SqlPersistT m [((AccountKey, Account), UserLevel)]
getUserAccounts k = do
    ckx <- fmap ((accountUserEntAccount &&& accountUserEntLevel) . entityVal) <$> selectList [AccountUserEntUser ==. UserEntKey k] []
    cx <- fmap toKeyedAccount <$> selectList [AccountEntId <-. (fst <$> ckx)] []
    pure $ zipWith (,) cx (snd <$> ckx)

-- | Update a user's email.
updateUserEmail :: MonadUnliftIO m => UserKey -> EmailAddress -> SqlPersistT m User
updateUserEmail k eml = toUser <$> updateGet (UserEntKey k) [UserEntEmail =. eml]

-- | Update a user's password.
updateUserPassword :: MonadUnliftIO m => UserKey -> Maybe PasswordHash -> SqlPersistT m User
updateUserPassword k pwd = toUser <$> updateGet (UserEntKey k) [UserEntPassword =. pwd]

-- | Set a user's github token.
setUserGithubToken :: MonadUnliftIO m => UserKey -> Maybe (OAuthToken Github) -> SqlPersistT m User
setUserGithubToken k tok = toUser <$> updateGet (UserEntKey k) [UserEntGithubToken =. fmap unOAuthToken tok]

-- | Create a user.
createUser :: MonadUnliftIO m => User -> SqlPersistT m UserKey
createUser u = do
    now <- liftIO getCurrentTime
    k <- makeKey UserKey
    insertKey (UserEntKey k) (fromUser now u)
    pure k

-- | Link a user to an account.
linkUserToAccount :: MonadUnliftIO m => AccountKey -> UserKey -> UserLevel -> SqlPersistT m ()
linkUserToAccount ck uk l =
    insert_ AccountUserEnt {
        accountUserEntAccount = AccountEntKey ck,
        accountUserEntUser = UserEntKey uk,
        accountUserEntLevel = l
    }

-- | Unlink a user from an account.
unlinkUserFromAccount :: MonadUnliftIO m => AccountKey -> UserKey -> SqlPersistT m ()
unlinkUserFromAccount ck uk = deleteWhere criterion
    where criterion = [AccountUserEntAccount ==. AccountEntKey ck, AccountUserEntUser ==. UserEntKey uk]

-- | Set the level of an account user.
updateAccountUserLevel :: MonadUnliftIO m => AccountKey -> UserKey -> UserLevel -> SqlPersistT m ()
updateAccountUserLevel ck uk l = updateWhere criterion [AccountUserEntLevel =. l]
    where criterion = [AccountUserEntAccount ==. AccountEntKey ck, AccountUserEntUser ==. UserEntKey uk]

--
--
--

toKeyedAccount :: Entity AccountEnt -> (AccountKey, Account)
toKeyedAccount = (unAccountEntKey . entityKey &&& toAccount . entityVal)

toAccount :: AccountEnt -> Account
toAccount c = Account {
    accountName = accountEntName c
}

fromAccount :: UTCTime -> Account -> AccountEnt
fromAccount created c = AccountEnt {
    accountEntCreatedAt = created,
    accountEntName = accountName c
}

--
--
--

toKeyedUser :: Entity UserEnt -> (UserKey, User)
toKeyedUser = (unUserEntKey . entityKey &&& toUser . entityVal)

toUser :: UserEnt -> User
toUser u = User {
    userName = userEntName u,
    userEmail = userEntEmail u,
    userPassword = userEntPassword u,
    userGithubToken = OAuthToken <$> userEntGithubToken u
}

fromUser :: UTCTime -> User -> UserEnt
fromUser created u = UserEnt {
    userEntCreatedAt = created,
    userEntName = userName u,
    userEntEmail = userEmail u,
    userEntPassword = userPassword u,
    userEntGithubToken = unOAuthToken <$> userGithubToken u
}

--
--
--

makeKey :: MonadUnliftIO m => (Text -> k) -> m k
makeKey f = f . toText <$> liftIO nextRandom

liftMaybe :: Monad m => m (Maybe a) -> MaybeT m a
liftMaybe = maybe mzero pure <=< lift
