{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Json.Util
    ( append
    , toJSONFieldName
    , (#)
    , UTCTimeMillis (..)
    , ToJSONObject  (..)
    , Base64ByteString (..)
    ) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Base64.Lazy as EL
import Data.Char (isUpper)
import Data.String
import Data.Time.Clock
import Data.Time.Format (formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Text (pack)
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import GHC.Generics

append :: Pair -> [Pair] -> [Pair]
append (_, Null) pp = pp
append p         pp = p:pp
{-# INLINE append #-}

infixr 5 #

(#) :: Pair -> [Pair] -> [Pair]
(#) = append
{-# INLINE (#) #-}

-----------------------------------------------------------------------------
-- UTCTimeMillis

-- | A newtype wrapper for 'UTCTime' that formats timestamps in JSON with
-- millisecond precision instead of the default picosecond precision.
newtype UTCTimeMillis = UTCTimeMillis UTCTime
  deriving (Eq)

instance Show UTCTimeMillis where
    showsPrec d = showParen (d > 10) . showString . showUTCTimeMillis

{-# INLINE showUTCTimeMillis #-}
showUTCTimeMillis :: UTCTimeMillis -> String
showUTCTimeMillis (UTCTimeMillis t) = formatTime defaultTimeLocale format t
  where
    format = "%FT%T." ++ formatMillis t ++ "Z"
    formatMillis = take 3 . formatTime defaultTimeLocale "%q"

instance ToJSON UTCTimeMillis where
    toJSON = String . pack . showUTCTimeMillis

instance FromJSON UTCTimeMillis where
    parseJSON = fmap UTCTimeMillis . parseJSON

-----------------------------------------------------------------------------
-- ToJSONObject

class ToJSONObject a where
    toJSONObject :: a -> Object

instance ToJSONObject Object where
    toJSONObject = id

-----------------------------------------------------------------------------
-- toJSONFieldName

-- | Convenient helper to convert field names to use as JSON fields.
-- it removes the prefix (assumed to be anything before an uppercase
-- character) and converts the rest to underscore
--
-- Example:
-- newtype TeamName = TeamName { tnTeamName :: Text }
-- deriveJSON toJSONFieldName ''tnTeamName
--
-- would generate {To/From}JSON instances where
-- the field name is "team_name"
toJSONFieldName :: Options
toJSONFieldName = defaultOptions{ fieldLabelModifier = camelTo2 '_' . dropPrefix }
  where
    dropPrefix :: String -> String
    dropPrefix = dropWhile (not . isUpper)

--------------------------------------------------------------------------------
-- base64-encoded lazy bytestrings

-- | Lazy 'ByteString' with base64 json encoding.  Relevant discussion:
-- <https://github.com/bos/aeson/issues/126>.  See test suite for more details.
newtype Base64ByteString = Base64ByteString { fromBase64ByteString :: L.ByteString }
  deriving (Eq, Show, Generic)

instance FromJSON Base64ByteString where
  parseJSON (String st) = handleError . EL.decode . stToLbs $ st
    where
      stToLbs = L.fromChunks . pure . Data.Text.Encoding.encodeUtf8
      handleError = either (fail "parse Base64ByteString: invalid base64 encoding")
                           (pure . Base64ByteString)
  parseJSON _ = fail "parse Base64ByteString: not a string"

instance ToJSON Base64ByteString where
  toJSON (Base64ByteString lbs) = String . lbsToSt . EL.encode $ lbs
    where
      lbsToSt = Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode
              . mconcat
              . L.toChunks

instance IsString Base64ByteString where
  fromString = Base64ByteString . L8.pack
