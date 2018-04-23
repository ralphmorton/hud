{-# LANGUAGE TemplateHaskell #-}

module Bridge.Types where

import Bridge.Instructions
import Bridge.Types.TH

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Map as M
import Data.Proxy
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.PureScript.Bridge

import HUD.Bridge
import HUD.Data
import HUD.Data.HUD
import HUD.Dashboard.Data.Relational

type ModuleName = String
type PurescriptCode = String

bridgeTypes ::  [(SumType 'Haskell, Instructions 'PureScript)]
bridgeTypes = fmap withInstructions $(listE . getInstanceSumTypes =<< reify ''Bridge)
    where
    withInstructions (name, base, st, arity) = (st, makeInstructions name base arity)
    makeInstructions name base arity = let
        tps = take arity alphabet
        inst = [if name `elem` withEqInstances then eqInstructions base tps else emptyInstructions,
                if name `elem` withToJSONInstances && name `elem` withFromJSONInstances
                    then
                        case arity of
                            0 -> jsonInstructions base
                            n -> jsonWithParametersInstructions base tps
                    else emptyInstructions
               ]
        in mconcat inst
    alphabet = ["a", "b", "c", "d", "e", "f"]

withEqInstances :: [String]
withEqInstances = $(listE . getInstances =<< reify ''Eq)

withToJSONInstances :: [String]
withToJSONInstances = $(listE . getInstances =<< reify ''ToJSON)

withFromJSONInstances :: [String]
withFromJSONInstances = $(listE . getInstances =<< reify ''FromJSON)
