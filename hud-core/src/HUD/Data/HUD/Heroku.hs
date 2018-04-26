{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HUD.Data.HUD.Heroku (
    module HUD.Data.HUD.Heroku.Organisation,
    HerokuReq(..),
    HerokuRsp(..),
    HerokuRequestException(..)
) where

import HUD.Bridge (Bridge)
import HUD.Data.HUD.Heroku.Organisation

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

--
--
--

data HerokuReq
    = HRRQOrganisations
    deriving (Bridge, Show, Generic, FromJSON, ToJSON)

--
--
--

data HerokuRsp
    = HRRSFailure HerokuRequestException
    | HRRSOrganisations [Organisation]
    deriving (Bridge, Show, Generic, FromJSON, ToJSON)

--
--
--

data HerokuRequestException
    = HRREAuthFailure
    | HRREParseFailure
    | HRREUnknownFailure Int
    deriving (Bridge, Show, Generic, FromJSON, ToJSON)

instance Exception HerokuRequestException
