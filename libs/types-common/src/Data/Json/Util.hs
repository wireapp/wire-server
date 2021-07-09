{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TypeApplications #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Data.Json.Util
  ( -- * JSON object utilities
    append,
    toJSONFieldName,
    (#),
    ToJSONObject (..),

    -- * UTCTimeMillis
    UTCTimeMillis,
    toUTCTimeMillis,
    fromUTCTimeMillis,
    showUTCTimeMillis,
    readUTCTimeMillis,

    -- * Base64
    Base64ByteString (..),
    fromBase64TextLenient,
    toBase64Text,
  )
where

import qualified Cassandra as CQL
import Control.Lens (coerced, (%~), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.Lazy as B64L
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Conversion as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Fixed
import Data.Schema
import Data.String.Conversions (cs)
import qualified Data.Swagger as S
import Data.Text (pack)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Data.Time.Clock
import Data.Time.Format (formatTime, parseTimeM)
import qualified Data.Time.Lens as TL
import Data.Time.Locale.Compat (defaultTimeLocale)
import Imports
import Test.QuickCheck (Arbitrary (arbitrary))
-- for UTCTime
import Test.QuickCheck.Instances ()

append :: A.Pair -> [A.Pair] -> [A.Pair]
append (_, A.Null) pp = pp
append p pp = p : pp
{-# INLINE append #-}

infixr 5 #

-- | An operator for building JSON in cases where you want @null@ fields to
-- disappear from the result instead of being present as @null@s.
(#) :: A.Pair -> [A.Pair] -> [A.Pair]
(#) = append
{-# INLINE (#) #-}

-----------------------------------------------------------------------------
-- UTCTimeMillis

-- | A newtype wrapper for 'UTCTime' that formats timestamps in JSON with
-- millisecond precision instead of the default picosecond precision.
-- Construct values using 'toUTCTimeMillis'; deconstruct with 'fromUTCTimeMillis'.
-- Unlike with 'UTCTime', 'Show' renders ISO string.
newtype UTCTimeMillis = UTCTimeMillis {fromUTCTimeMillis :: UTCTime}
  deriving (Eq, Ord, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema UTCTimeMillis

instance ToSchema UTCTimeMillis where
  schema =
    UTCTimeMillis <$> showUTCTimeMillis
      .= mkSchema swagger parseJSON (pure . A.String . pack)
    where
      swagger =
        S.NamedSchema (Just "UTCTimeMillis") <$> mempty
          & S.schema . S.type_ ?~ S.SwaggerString
          & S.schema . S.format ?~ "yyyy-mm-ddThh:MM:ss.qqq"
          & S.schema . S.example ?~ "2021-05-12T10:52:02.671Z"

{-# INLINE toUTCTimeMillis #-}
toUTCTimeMillis :: HasCallStack => UTCTime -> UTCTimeMillis
toUTCTimeMillis = UTCTimeMillis . (TL.seconds . coerced @Pico @_ @Integer %~ (* 1e9) . (`div` 1e9))

{-# INLINE showUTCTimeMillis #-}
showUTCTimeMillis :: UTCTimeMillis -> String
showUTCTimeMillis (UTCTimeMillis t) = formatTime defaultTimeLocale "%FT%T.%03qZ" t

readUTCTimeMillis :: String -> Maybe UTCTimeMillis
readUTCTimeMillis = fmap toUTCTimeMillis . parseTimeM True defaultTimeLocale formatUTCTimeMillis

formatUTCTimeMillis :: String
formatUTCTimeMillis = "%FT%T%QZ"

instance Show UTCTimeMillis where
  showsPrec d = showParen (d > 10) . showString . showUTCTimeMillis

instance BS.ToByteString UTCTimeMillis where
  builder = BB.byteString . cs . show

instance BS.FromByteString UTCTimeMillis where
  parser = maybe (fail "UTCTimeMillis") pure . readUTCTimeMillis =<< BS.parser

instance CQL.Cql UTCTimeMillis where
  ctype = CQL.Tagged CQL.TimestampColumn
  toCql = CQL.toCql . fromUTCTimeMillis
  fromCql = fmap toUTCTimeMillis . CQL.fromCql

instance Arbitrary UTCTimeMillis where
  arbitrary = toUTCTimeMillis <$> arbitrary

-----------------------------------------------------------------------------
-- ToJSONObject

class ToJSONObject a where
  toJSONObject :: a -> A.Object

instance ToJSONObject A.Object where
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
toJSONFieldName :: A.Options
toJSONFieldName = A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . dropPrefix}
  where
    dropPrefix :: String -> String
    dropPrefix = dropWhile (not . isUpper)

--------------------------------------------------------------------------------
-- base64-encoded lazy bytestrings

-- | Lazy 'ByteString' with base64 json encoding.  Relevant discussion:
-- <https://github.com/bos/aeson/issues/126>.  See test suite for more details.
newtype Base64ByteString = Base64ByteString {fromBase64ByteString :: L.ByteString}
  deriving (Eq, Show, Generic)

instance FromJSON Base64ByteString where
  parseJSON (A.String st) = handleError . B64L.decode . stToLbs $ st
    where
      stToLbs = L.fromChunks . pure . Text.encodeUtf8
      handleError =
        either
          (const $ fail "parse Base64ByteString: invalid base64 encoding")
          (pure . Base64ByteString)
  parseJSON _ = fail "parse Base64ByteString: not a string"

instance ToJSON Base64ByteString where
  toJSON (Base64ByteString lbs) = A.String . lbsToSt . B64L.encode $ lbs
    where
      lbsToSt =
        Text.decodeUtf8With Text.lenientDecode
          . mconcat
          . L.toChunks

instance IsString Base64ByteString where
  fromString = Base64ByteString . L8.pack

instance Arbitrary Base64ByteString where
  arbitrary = Base64ByteString <$> arbitrary

--------------------------------------------------------------------------------
-- Utilities

fromBase64TextLenient :: Text -> ByteString
fromBase64TextLenient = B64.decodeLenient . Text.encodeUtf8

toBase64Text :: ByteString -> Text
toBase64Text = Text.decodeUtf8 . B64.encode
