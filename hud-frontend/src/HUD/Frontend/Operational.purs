
module HUD.Frontend.Operational (
    OpM,
    Exception(..),
    Context(..),
    Config,
    Endpoint,
    IdentityInfo(..),
    runOp
) where

import Prelude

import HUD.Data.Identity (Token)
import HUD.Frontend.Network.HTTP (class AsHttpException, HttpException)
import HUD.Frontend.Web.Navigate (NAVIGATE, navigate)
import HUD.Frontend.Router (PublicRoute(Error), Route(Public))

import Control.Monad.Aff (Aff, never)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Either (either)
import Debug.Trace (traceAny)

--
--
--

type OpM i c e = ReaderT (Context i c) (ExceptT Exception (Aff e))

--
--
--

data Exception
    = HttpException HttpException

instance showException :: Show Exception where
    show (HttpException e) = "HttpException " <> show e

instance asHttpExceptionException :: AsHttpException Exception where
    asHttpException = HttpException

--
--
--

type Context i c = {
    i :: i,
    acc :: c,
    config :: Config
}

type Config = {
    identity :: Endpoint,
    dashboard :: Endpoint,
    github :: { appID :: String },
    trello :: { appID :: String, appName :: String },
    heroku :: { appID :: String }
}

type Endpoint = {
    url :: String
}

--

data IdentityInfo
    = IIUser Token

--
--
--

runOp :: forall i c e. Context i c -> OpM i c (navigate :: NAVIGATE | e) ~> Aff (navigate :: NAVIGATE | e)
runOp ctx = handle <<< runExceptT <<< flip runReaderT ctx
    where
    handle = (=<<) (either err pure)
    err e = do -- TODO: rollbar
        traceAny (show e) $ \_ -> pure unit
        navigate (Public Error)
        never
