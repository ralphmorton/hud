{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Operational.Email (
    EmailException(..),
    SendGridCreds(..),
    SendGridV2Creds(..),
    Email(..),
    EmailUser(..),
    EmailContent(..),
    email
) where

import HUD.Data (EmailAddress(..))
import HUD.Operational.HTTP

import Control.Exception (Exception)
import Control.Lens (view)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Context
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Monoid
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Types (Status, methodPost, status200)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO)

--
--
--

data EmailException
    = EmailHttpException Status BL.ByteString
    deriving Show

instance Exception EmailException

--
--
--

data SendGridCreds
    = SendGridV2 SendGridV2Creds

--

data SendGridV2Creds = SendGridV2Creds {
    sgv2User :: B.ByteString,
    sgv2Key :: B.ByteString
}

--
--
--

data Email = Email {
    emailFrom :: EmailUser,
    emailTo :: NonEmpty EmailUser,
    emailCC :: [EmailUser],
    emailBCC :: [EmailUser],
    emailSubject :: T.Text,
    emailContent :: EmailContent
} deriving (Eq, Show, Generic, FromJSON, ToJSON)

--

data EmailUser = EmailUser {
    emailUserName :: Maybe T.Text,
    emailUserAddress :: EmailAddress
} deriving (Eq, Show, Generic, FromJSON, ToJSON)

--

data EmailContent
    = TextOnly TL.Text
    | HtmlOnly TL.Text
    | TextAndHtml TL.Text TL.Text
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

--
--
--

email :: (
    MonadThrow m,
    MonadUnliftIO m,
    MonadReader (Context r) m,
    HasContextLens r HttpManager HttpManager,
    HasContextLens r SendGridCreds SendGridCreds) => Email -> m ()
email eml = do
    creds <- view (contextLens Proxy)
    case creds of
        SendGridV2 v2 -> emailV2 v2 eml

--
--
--

emailV2 :: (
    MonadThrow m,
    MonadUnliftIO m,
    MonadReader (Context r) m,
    HasContextLens r HttpManager HttpManager) => SendGridV2Creds -> Email -> m ()
emailV2 creds eml = do
    let parts = buildV2RequestData creds eml
    ireq <- formDataBody parts =<< parseRequest "https://api.sendgrid.com/api/mail.send.json"
    rsp <- http ireq { method = methodPost }
    let stat = responseStatus rsp
    let body = responseBody rsp
    case stat == status200 of
        False -> throwIO (EmailHttpException stat body)
        True -> pure ()

--

buildV2RequestData :: SendGridV2Creds -> Email -> [Part]
buildV2RequestData creds eml = base <> to <> cc <> bcc <> content
    where
    addr = unEmailAddress . emailUserAddress
    name = maybe "" id . emailUserName
    base =
        [
            partBS "subject" (TE.encodeUtf8 $ emailSubject eml),
            partBS "api_user" (sgv2User creds),
            partBS "api_key" (sgv2Key creds),
            partBS "from" (TE.encodeUtf8 . addr $ emailFrom eml),
            partBS "fromname" (TE.encodeUtf8 . name $ emailFrom eml)
        ]
    to = concat $ v2Recip "to[]" "toname[]" <$> toList (emailTo eml)
    cc = concat $ v2Recip "cc[]" "ccname[]" <$> emailCC eml
    bcc = concat $ v2Recip "bcc[]" "bccname[]" <$> emailBCC eml
    content = case emailContent eml of
        TextOnly txt ->
            pure $ partLBS "text" (TLE.encodeUtf8 txt)
        HtmlOnly html ->
            pure $ partLBS "html" (TLE.encodeUtf8 html)
        TextAndHtml txt html ->
            [partLBS "text" (TLE.encodeUtf8 txt), partLBS "html" (TLE.encodeUtf8 html)]

--

v2Recip :: T.Text -> T.Text -> EmailUser -> [Part]
v2Recip at nt (EmailUser name addr) = addrp : maybe [] pure namep
    where
    addrp = partBS at (TE.encodeUtf8 $ unEmailAddress addr)
    namep = partBS nt . TE.encodeUtf8 <$> name
