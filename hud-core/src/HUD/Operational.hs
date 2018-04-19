{-# LANGUAGE ConstraintKinds #-}

module HUD.Operational (
    module HUD.Operational.SQL,
    module HUD.Operational.Mongo,
    module HUD.Operational.AMQP,
    module HUD.Operational.Redis,
    module HUD.Operational.HTTP,
    module HUD.Operational.Email,
    ContextReader,
    HasContext
) where

import HUD.Operational.SQL
import HUD.Operational.Mongo
import HUD.Operational.AMQP
import HUD.Operational.Redis
import HUD.Operational.HTTP
import HUD.Operational.Email

import Control.Monad.Reader
import Data.Context

--
--
--

type ContextReader r = MonadReader (Context r)

--

type HasContext r a = HasContextLens r a a
