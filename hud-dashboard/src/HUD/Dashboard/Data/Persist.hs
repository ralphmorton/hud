{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HUD.Dashboard.Data.Persist where

import HUD.Data
import HUD.Dashboard.Data.Relational

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

--
--
--

share [mkPersist sqlSettings, mkMigrate "migrate"] [persistLowerCase|
AccountEnt sql=accounts
    Id AccountKey
    createdAt UTCTime
    name Text
    UniqueAccount name
UserEnt sql=users
    Id UserKey
    createdAt UTCTime
    name Text
    email EmailAddress
    password PasswordHash Maybe
    githubToken Text Maybe
    trelloToken Text Maybe
    herokuToken Text Maybe
    UniqueUser email
AccountUserEnt sql=account_users
    account AccountEntId
    user UserEntId
    level UserLevel
    UniqueAccountUser account user
|]
