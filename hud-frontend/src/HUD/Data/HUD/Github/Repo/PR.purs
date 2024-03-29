-- File auto generated by purescript-bridge! --
module HUD.Data.HUD.Github.Repo.PR


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
import HUD.Data.HUD.Github.Common (Account, PRID, PRNum)
import HUD.Data.HUD.Github.User (AvatarURL)
import Prim (Array, String)

import Prelude
import Data.Generic (class Generic)

data PRState =
    PROpen
  | PRClosed

derive instance genericPRState :: Generic PRState


--------------------------------------------------------------------------------
_PROpen :: Prism' PRState Unit
_PROpen = prism' (\_ -> PROpen) f
  where
    f PROpen = Just unit
    f _ = Nothing

_PRClosed :: Prism' PRState Unit
_PRClosed = prism' (\_ -> PRClosed) f
  where
    f PRClosed = Just unit
    f _ = Nothing

--------------------------------------------------------------------------------
newtype PR =
    PR {
      prID :: PRID
    , prNumber :: PRNum
    , prCreatedAt :: UTCTime
    , prUpdatedAt :: UTCTime
    , prClosedAt :: Maybe UTCTime
    , prMergedAt :: Maybe UTCTime
    , prUser :: Tuple Account AvatarURL
    , prHtmlURL :: String
    , prTitle :: String
    , prBody :: Maybe String
    , prState :: PRState
    , prAssignees :: Array (Tuple Account AvatarURL)
    , prReviewers :: Array (Tuple Account AvatarURL)
    }

derive instance genericPR :: Generic PR

derive instance newtypePR :: Newtype PR _


--------------------------------------------------------------------------------
_PR :: Iso' PR { prID :: PRID, prNumber :: PRNum, prCreatedAt :: UTCTime, prUpdatedAt :: UTCTime, prClosedAt :: Maybe UTCTime, prMergedAt :: Maybe UTCTime, prUser :: Tuple Account AvatarURL, prHtmlURL :: String, prTitle :: String, prBody :: Maybe String, prState :: PRState, prAssignees :: Array (Tuple Account AvatarURL), prReviewers :: Array (Tuple Account AvatarURL)}
_PR = _Newtype

prID :: Lens' PR PRID
prID = _Newtype <<< prop (SProxy :: SProxy "prID")

prNumber :: Lens' PR PRNum
prNumber = _Newtype <<< prop (SProxy :: SProxy "prNumber")

prCreatedAt :: Lens' PR UTCTime
prCreatedAt = _Newtype <<< prop (SProxy :: SProxy "prCreatedAt")

prUpdatedAt :: Lens' PR UTCTime
prUpdatedAt = _Newtype <<< prop (SProxy :: SProxy "prUpdatedAt")

prClosedAt :: Lens' PR (Maybe UTCTime)
prClosedAt = _Newtype <<< prop (SProxy :: SProxy "prClosedAt")

prMergedAt :: Lens' PR (Maybe UTCTime)
prMergedAt = _Newtype <<< prop (SProxy :: SProxy "prMergedAt")

prUser :: Lens' PR (Tuple Account AvatarURL)
prUser = _Newtype <<< prop (SProxy :: SProxy "prUser")

prHtmlURL :: Lens' PR String
prHtmlURL = _Newtype <<< prop (SProxy :: SProxy "prHtmlURL")

prTitle :: Lens' PR String
prTitle = _Newtype <<< prop (SProxy :: SProxy "prTitle")

prBody :: Lens' PR (Maybe String)
prBody = _Newtype <<< prop (SProxy :: SProxy "prBody")

prState :: Lens' PR PRState
prState = _Newtype <<< prop (SProxy :: SProxy "prState")

prAssignees :: Lens' PR (Array (Tuple Account AvatarURL))
prAssignees = _Newtype <<< prop (SProxy :: SProxy "prAssignees")

prReviewers :: Lens' PR (Array (Tuple Account AvatarURL))
prReviewers = _Newtype <<< prop (SProxy :: SProxy "prReviewers")

--------------------------------------------------------------------------------
derive instance eqPRState :: Eq PRState
instance decodePRState :: DecodeJson PRState where
    decodeJson = Aeson.decodeJson
instance encodePRState :: EncodeJson PRState where
    encodeJson = Aeson.encodeJson
instance decodePR :: DecodeJson PR where
    decodeJson = Aeson.decodeJson
instance encodePR :: EncodeJson PR where
    encodeJson = Aeson.encodeJson

