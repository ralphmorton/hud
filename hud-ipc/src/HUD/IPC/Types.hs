{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.IPC.Types (
    IPCResult(..)
) where

import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)

--
--
--

data IPCResult a
    = IPCRequestDecodeFailure
    | IPCResultDecodeFailure Text
    | IPCUnhandledException Text
    | IPCResult a

--

instance Show a => Show (IPCResult a) where
    show IPCRequestDecodeFailure = "IPCRequestDecodeFailure"
    show (IPCResultDecodeFailure raw) = "IPCResultDecodeFailure " ++ show raw
    show (IPCUnhandledException e) = "IPCUnhandledException " ++ show e
    show (IPCResult a) = "IPCResult " ++ show a

--

instance ToJSON a => ToJSON (IPCResult a) where
    toJSON IPCRequestDecodeFailure =
        object ["t" .= ("IPCRequestDecodeFailure" :: Text)]
    toJSON (IPCResultDecodeFailure raw) =
        object ["t" .= ("IPCResultDecodeFailure" :: Text), "raw" .= raw]
    toJSON (IPCUnhandledException e) =
        object ["t" .= ("IPCUnhandledException" :: Text), "error" .= e]
    toJSON (IPCResult a) =
        object ["t" .= ("IPCResult" :: Text), "result" .= a]

--

instance FromJSON a => FromJSON (IPCResult a) where
    parseJSON (Object v) = do
        t <- (v .: "t" :: Parser Text)
        case t of
            "IPCRequestDecodeFailure" -> pure IPCRequestDecodeFailure
            "IPCResultDecodeFailure" -> IPCResultDecodeFailure <$> v .: "raw"
            "IPCUnhandledException" -> IPCUnhandledException <$> v .: "error"
            "IPCResult" -> IPCResult <$> v .: "result"
            _ -> mzero
    parseJSON _ = mzero
