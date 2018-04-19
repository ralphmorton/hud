{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HUD.Email.Data (
    SentEmail(..),
    insertEmail,
    emailToDoc,
    emailFromDoc
) where

import Prelude hiding (lookup)

import HUD.Data (EmailAddress(..))
import HUD.Operational

import Data.Bson
import Data.List.NonEmpty (nonEmpty, toList)
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime)
import Database.MongoDB (Collection, insert_, master)
import UnliftIO (MonadUnliftIO)

--
--
--

emails :: Collection
emails = "emails"

--
--
--

data SentEmail = SentEmail {
    sentAt :: UTCTime,
    sentEmail :: Email
} deriving Show

--
--
--

insertEmail :: (
    MonadUnliftIO m,
    ContextReader r m,
    HasContext r MongoPool) => SentEmail -> m ()
insertEmail = mongo master . insert_ emails . emailToDoc

--
--
--

emailToDoc :: SentEmail -> Document
emailToDoc (SentEmail at' eml) =
    [
        "at" := val at',
        "from" := (val . EmailUserT) (emailFrom eml),
        "to" := valList (EmailUserT <$> toList (emailTo eml)),
        "cc" := valList (EmailUserT <$> emailCC eml),
        "bcc" := valList (EmailUserT <$> emailCC eml),
        "subject" := val (emailSubject eml),
        "content" := val (EmailContentT $ emailContent eml)
    ]

--
--
--

emailFromDoc :: Document -> Maybe SentEmail
emailFromDoc doc = do
    at' <- lookup "at" doc
    from <- unEmailUserT <$> lookup "from" doc
    to <- fmap unEmailUserT <$> (nonEmpty =<< lookup "to" doc)
    cc <- fmap unEmailUserT <$> lookup "cc" doc
    bcc <- fmap unEmailUserT <$> lookup "bcc" doc
    subject <- lookup "subject" doc
    content <- unEmailContentT <$> lookup "content" doc
    pure . SentEmail at' $ Email {
        emailFrom = from,
        emailTo = to,
        emailCC = cc,
        emailBCC = bcc,
        emailSubject = subject,
        emailContent = content
    }

--
--
--

newtype EmailUserT = EmailUserT {
    unEmailUserT :: EmailUser
} deriving (Eq, Show)

instance Val EmailUserT where
    val (EmailUserT u) =
        Doc [
            "name" := val (emailUserName u),
            "address" := val (unEmailAddress $ emailUserAddress u)
        ]
    cast' (Doc doc) = do
        name <- lookup "name" doc
        addr <- EmailAddress <$> lookup "address" doc
        pure . EmailUserT $ EmailUser {
            emailUserName = name,
            emailUserAddress = addr
        }
    cast' _ = Nothing

--
--
--

newtype EmailContentT = EmailContentT {
    unEmailContentT :: EmailContent
} deriving (Eq, Show)

instance Val EmailContentT where
    val (EmailContentT (TextOnly txt)) =
        Doc ["text" := val (TL.toStrict txt)]
    val (EmailContentT (HtmlOnly html)) =
        Doc ["html" := val (TL.toStrict html)]
    val (EmailContentT (TextAndHtml txt html)) =
        Doc ["text" := val (TL.toStrict txt), "html" := val (TL.toStrict html)]
    cast' (Doc doc) = case (lookup "text" doc, lookup "html" doc) of
        (Just txt, Nothing) -> pure (EmailContentT $ TextOnly (TL.fromStrict txt))
        (Nothing, Just html) -> pure (EmailContentT $ HtmlOnly (TL.fromStrict html))
        (Just txt, Just html) -> pure (EmailContentT $ TextAndHtml (TL.fromStrict txt) (TL.fromStrict html))
        _ -> Nothing
    cast' _ = Nothing
