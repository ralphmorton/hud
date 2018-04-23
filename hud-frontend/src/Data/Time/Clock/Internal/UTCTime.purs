
module Data.Time.Clock.Internal.UTCTime (
    UTCTime,
    fromUTCTime,
    toUTCTime
) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Generic.Aeson as Aeson
import Data.Array (some)
import Data.Date (canonicalDate, day, month, year)
import Data.DateTime (DateTime(..))
import Data.Either (either)
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Generic (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.String (fromCharArray)
import Data.Time (Time(..), hour, millisecond, minute, second)
import Data.Unfoldable (replicateA)
import Text.Parsing.Parser (Parser, fail, runParser)
import Text.Parsing.Parser.String (char)
import Text.Parsing.Parser.Token (digit)

--
--
--

newtype UTCTime = UTCTime String

derive instance genericUTCTime :: Generic UTCTime

instance encodeUTCTime :: EncodeJson UTCTime where
    encodeJson = Aeson.encodeJson

instance decodeUTCTime :: DecodeJson UTCTime where
    decodeJson = Aeson.decodeJson

--
--
--

fromUTCTime :: UTCTime -> Maybe DateTime
fromUTCTime (UTCTime s) = either (const Nothing) (pure) res
    where res = runParser s iso8601

--

-- 2018-03-27T08:47:56.7672091Z
iso8601 :: Parser String DateTime
iso8601 = do
    year' <- number =<< replicateA 4 digit
    void (char '-')
    month' <- number =<< replicateA 2 digit
    void (char '-')
    day' <- number =<< replicateA 2 digit
    void (char 'T')
    hour' <- number =<< replicateA 2 digit
    void (char ':')
    minute' <- number =<< replicateA 2 digit
    void (char ':')
    second' <- number =<< replicateA 2 digit
    void (char '.')
    milli' <- number =<< some digit
    void (char 'Z')
    let date = canonicalDate year' month' day'
    let time = Time hour' minute' second' milli'
    pure (DateTime date time)

--

number :: forall a. BoundedEnum a => Array Char -> Parser String a
number cx = maybe (fail "Could not parse number") pure do 
    i <- fromString (fromCharArray cx)
    toEnum i

--
--
--

toUTCTime :: DateTime -> UTCTime
toUTCTime = UTCTime <<< formatISO8601

--

formatISO8601 :: DateTime -> String
formatISO8601 (DateTime d t) =
    (show <<< fromEnum $ year d) <> "-" <>
    (showLeadingZero <<< fromEnum $ month d) <> "-" <>
    (showLeadingZero <<< fromEnum $ day d) <> "T" <>
    (showLeadingZero <<< fromEnum $ hour t) <> ":" <>
    (showLeadingZero <<< fromEnum $ minute t) <> ":" <>
    (showLeadingZero <<< fromEnum $ second t) <> "." <>
    (show <<< fromEnum $ millisecond t) <> "Z"

--

showLeadingZero :: Int -> String
showLeadingZero n = case (n < 10) of
    true -> "0" <> show n
    false -> show n
