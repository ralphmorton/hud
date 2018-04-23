{-# LANGUAGE TemplateHaskell #-}

module Bridge.Types.TH where

import Bridge.Instructions

import Control.Arrow (second)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Proxy
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.PureScript.Bridge
import Language.PureScript.Bridge.TypeParameters

--
--
--

getInstances :: Info -> [ExpQ]
getInstances (ClassI _ instances) = catMaybes $ getName <$> instances
    where
    getName (InstanceD _ _ (AppT _ t) _) =
        case extractArity t of
            Just (n, _) -> Just . stringE $ show n
            _ -> Nothing

getInstanceSumTypes :: Info -> [ExpQ]
getInstanceSumTypes (ClassI _ instances) = catMaybes $ getInstanceTypeArg <$> instances
    where
    getInstanceTypeArg (InstanceD _ _ (AppT _ t) _) =
        case extractArity t of
            Just (n, arity) ->
                Just $ tupE [stringE $ show n, stringE $ nameBase n, sumTypeFor n arity, litE (integerL arity)]
            _ -> Nothing

sumTypeFor :: Name -> Integer -> ExpQ
sumTypeFor n arity = appE (varE 'mkSumType) proxySignature
    where
    proxySignature = sigE (conE 'Proxy) $ appT (conT ''Proxy) proxyType
    proxyType = foldl f (conT n) $ take (fromInteger arity) bridgeTypeArgs
    f tc arg = appT tc $ conT arg

bridgeTypeArgs :: [Name]
bridgeTypeArgs = [''A, ''B, ''C, ''D, ''E, ''F, ''G, ''H, ''I, ''J, ''K, ''L, ''M, ''N, ''O, ''P]

extractArity :: Type -> Maybe (Name, Integer)
extractArity (AppT x _) = second (+1) <$> extractArity x
extractArity (ConT n) = Just (n, 0)
extractArity _ = Nothing
