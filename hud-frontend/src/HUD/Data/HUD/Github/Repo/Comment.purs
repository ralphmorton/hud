-- File auto generated by purescript-bridge! --
module HUD.Data.HUD.Github.Repo.Comment


where
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Generic.Aeson as Aeson


import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import Data.Time.Clock.Internal.UTCTime (UTCTime)
import Data.Tuple (Tuple)
import HUD.Data.HUD.Github.Common (Account)
import HUD.Data.HUD.Github.User (AvatarURL)
import Prim (Int, String)

import Prelude
import Data.Generic (class Generic)

newtype IssueCommentID =
    IssueCommentID {
      unIssueCommentID :: Int
    }

derive instance genericIssueCommentID :: Generic IssueCommentID

derive instance newtypeIssueCommentID :: Newtype IssueCommentID _


--------------------------------------------------------------------------------
_IssueCommentID :: Iso' IssueCommentID { unIssueCommentID :: Int}
_IssueCommentID = _Newtype

unIssueCommentID :: Lens' IssueCommentID Int
unIssueCommentID = _Newtype <<< prop (SProxy :: SProxy "unIssueCommentID")

--------------------------------------------------------------------------------
newtype IssueComment =
    IssueComment {
      icoID :: IssueCommentID
    , icoCreatedAt :: UTCTime
    , icoUpdatedAt :: UTCTime
    , icoURL :: String
    , icoHtmlURL :: String
    , icoUser :: Tuple Account AvatarURL
    , icoBody :: String
    }

derive instance genericIssueComment :: Generic IssueComment

derive instance newtypeIssueComment :: Newtype IssueComment _


--------------------------------------------------------------------------------
_IssueComment :: Iso' IssueComment { icoID :: IssueCommentID, icoCreatedAt :: UTCTime, icoUpdatedAt :: UTCTime, icoURL :: String, icoHtmlURL :: String, icoUser :: Tuple Account AvatarURL, icoBody :: String}
_IssueComment = _Newtype

icoID :: Lens' IssueComment IssueCommentID
icoID = _Newtype <<< prop (SProxy :: SProxy "icoID")

icoCreatedAt :: Lens' IssueComment UTCTime
icoCreatedAt = _Newtype <<< prop (SProxy :: SProxy "icoCreatedAt")

icoUpdatedAt :: Lens' IssueComment UTCTime
icoUpdatedAt = _Newtype <<< prop (SProxy :: SProxy "icoUpdatedAt")

icoURL :: Lens' IssueComment String
icoURL = _Newtype <<< prop (SProxy :: SProxy "icoURL")

icoHtmlURL :: Lens' IssueComment String
icoHtmlURL = _Newtype <<< prop (SProxy :: SProxy "icoHtmlURL")

icoUser :: Lens' IssueComment (Tuple Account AvatarURL)
icoUser = _Newtype <<< prop (SProxy :: SProxy "icoUser")

icoBody :: Lens' IssueComment String
icoBody = _Newtype <<< prop (SProxy :: SProxy "icoBody")

--------------------------------------------------------------------------------
newtype CommentID =
    CommentID {
      unCommentID :: Int
    }

derive instance genericCommentID :: Generic CommentID

derive instance newtypeCommentID :: Newtype CommentID _


--------------------------------------------------------------------------------
_CommentID :: Iso' CommentID { unCommentID :: Int}
_CommentID = _Newtype

unCommentID :: Lens' CommentID Int
unCommentID = _Newtype <<< prop (SProxy :: SProxy "unCommentID")

--------------------------------------------------------------------------------
newtype Comment =
    Comment {
      coID :: CommentID
    , coPosition :: Maybe Int
    , coLine :: Maybe Int
    , coCreatedAt :: Maybe UTCTime
    , coUpdatedAt :: UTCTime
    , coURL :: String
    , coHtmlURL :: Maybe String
    , coUser :: Tuple Account AvatarURL
    , coBody :: String
    }

derive instance genericComment :: Generic Comment

derive instance newtypeComment :: Newtype Comment _


--------------------------------------------------------------------------------
_Comment :: Iso' Comment { coID :: CommentID, coPosition :: Maybe Int, coLine :: Maybe Int, coCreatedAt :: Maybe UTCTime, coUpdatedAt :: UTCTime, coURL :: String, coHtmlURL :: Maybe String, coUser :: Tuple Account AvatarURL, coBody :: String}
_Comment = _Newtype

coID :: Lens' Comment CommentID
coID = _Newtype <<< prop (SProxy :: SProxy "coID")

coPosition :: Lens' Comment (Maybe Int)
coPosition = _Newtype <<< prop (SProxy :: SProxy "coPosition")

coLine :: Lens' Comment (Maybe Int)
coLine = _Newtype <<< prop (SProxy :: SProxy "coLine")

coCreatedAt :: Lens' Comment (Maybe UTCTime)
coCreatedAt = _Newtype <<< prop (SProxy :: SProxy "coCreatedAt")

coUpdatedAt :: Lens' Comment UTCTime
coUpdatedAt = _Newtype <<< prop (SProxy :: SProxy "coUpdatedAt")

coURL :: Lens' Comment String
coURL = _Newtype <<< prop (SProxy :: SProxy "coURL")

coHtmlURL :: Lens' Comment (Maybe String)
coHtmlURL = _Newtype <<< prop (SProxy :: SProxy "coHtmlURL")

coUser :: Lens' Comment (Tuple Account AvatarURL)
coUser = _Newtype <<< prop (SProxy :: SProxy "coUser")

coBody :: Lens' Comment String
coBody = _Newtype <<< prop (SProxy :: SProxy "coBody")

--------------------------------------------------------------------------------
derive instance eqIssueCommentID :: Eq IssueCommentID
instance decodeIssueCommentID :: DecodeJson IssueCommentID where
    decodeJson = Aeson.decodeJson
instance encodeIssueCommentID :: EncodeJson IssueCommentID where
    encodeJson = Aeson.encodeJson
instance decodeIssueComment :: DecodeJson IssueComment where
    decodeJson = Aeson.decodeJson
instance encodeIssueComment :: EncodeJson IssueComment where
    encodeJson = Aeson.encodeJson
derive instance eqCommentID :: Eq CommentID
instance decodeCommentID :: DecodeJson CommentID where
    decodeJson = Aeson.decodeJson
instance encodeCommentID :: EncodeJson CommentID where
    encodeJson = Aeson.encodeJson
instance decodeComment :: DecodeJson Comment where
    decodeJson = Aeson.decodeJson
instance encodeComment :: EncodeJson Comment where
    encodeJson = Aeson.encodeJson
