{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TypeApplications #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
    utcTimeTextSchema,
    utcTimeSchema,
    toUTCTimeMillis,
    fromUTCTimeMillis,
    showUTCTimeMillis,
    readUTCTimeMillis,

    -- * Base64
    Base64ByteString (..),
    base64Schema,
    Base64ByteStringL (..),
    base64SchemaL,
    fromBase64TextLenient,
    fromBase64Text,
    toBase64Text,
  )
where

import qualified Cassandra as CQL
import Control.Lens hiding ((#), (.=))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Attoparsec.Time as Atto
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Conversion as BS
import qualified Data.ByteString.Lazy as L
import Data.Fixed
import Data.Schema
import Data.String.Conversions (cs)
import qualified Data.Swagger as S
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Data.Time.Clock
import Data.Time.Format (formatTime, parseTimeM)
import qualified Data.Time.Lens as TL
import Data.Time.Locale.Compat (defaultTimeLocale)
import Imports
import Servant
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
  schema = UTCTimeMillis <$> showUTCTimeMillis .= utcTimeTextSchema

utcTimeTextSchema :: ValueSchemaP NamedSwaggerDoc Text UTCTime
utcTimeTextSchema =
  parsedText "UTCTime" (Atto.parseOnly (Atto.utcTime <* Atto.endOfInput))
    & doc . S.schema
      %~ (S.format ?~ "yyyy-mm-ddThh:MM:ss.qqq")
        . (S.example ?~ "2021-05-12T10:52:02.671Z")

utcTimeSchema :: ValueSchema NamedSwaggerDoc UTCTime
utcTimeSchema = showUTCTime .= utcTimeTextSchema

{-# INLINE toUTCTimeMillis #-}
toUTCTimeMillis :: HasCallStack => UTCTime -> UTCTimeMillis
toUTCTimeMillis = UTCTimeMillis . (TL.seconds . coerced @Pico @_ @Integer %~ (* 1e9) . (`div` 1e9))

{-# INLINE showUTCTimeMillis #-}
showUTCTimeMillis :: UTCTimeMillis -> Text
showUTCTimeMillis = Text.pack . formatTime defaultTimeLocale "%FT%T.%03qZ" . fromUTCTimeMillis

showUTCTime :: UTCTime -> Text
showUTCTime = Text.pack . formatTime defaultTimeLocale "%FT%T%QZ"

readUTCTimeMillis :: String -> Maybe UTCTimeMillis
readUTCTimeMillis = fmap toUTCTimeMillis . parseTimeM True defaultTimeLocale formatUTCTimeMillis

formatUTCTimeMillis :: String
formatUTCTimeMillis = "%FT%T%QZ"

instance Show UTCTimeMillis where
  showsPrec d = showParen (d > 10) . showString . Text.unpack . showUTCTimeMillis

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

-- | Base64-encoded strict 'ByteString'.
--
-- For proper Swagger generation, avoid using this type directly in APIs. Instead,
-- use a plain 'ByteString' (or a more specific newtype wrapper), and construct
-- instances using @deriving via@.
--
-- For URLs or HTTP headers, the base64url encoding is used.
--
-- Some related discussion: <https://github.com/bos/aeson/issues/126>.
newtype Base64ByteString = Base64ByteString {fromBase64ByteString :: ByteString}
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, ToJSON) via Schema Base64ByteString
  deriving newtype (Arbitrary, IsString)

instance ToSchema Base64ByteString where
  schema = fromBase64ByteString .= fmap Base64ByteString base64SchemaN

instance FromHttpApiData Base64ByteString where
  parseUrlPiece = bimap Text.pack Base64ByteString . B64U.decodeUnpadded . Text.encodeUtf8

instance ToHttpApiData Base64ByteString where
  toUrlPiece = Text.decodeUtf8With Text.lenientDecode . B64U.encodeUnpadded . fromBase64ByteString

instance S.ToParamSchema Base64ByteString where
  toParamSchema _ = mempty & S.type_ ?~ S.SwaggerString

-- base64("example") ~> "ZXhhbXBsZQo="
base64SchemaN :: ValueSchema NamedSwaggerDoc ByteString
base64SchemaN =
  (toBase64Text .= parsedText "Base64ByteString" fromBase64Text)
    & doc %~ fmap (S.schema . S.example ?~ A.String "ZXhhbXBsZQo=")

base64Schema :: ValueSchema SwaggerDoc ByteString
base64Schema = unnamed base64SchemaN

--------------------------------------------------------------------------------

-- | Base64-encoded lazy 'ByteString'.
-- Similar to 'Base64ByteString', but based on 'LByteString'.
newtype Base64ByteStringL = Base64ByteStringL {fromBase64ByteStringL :: LByteString}
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Schema Base64ByteStringL
  deriving newtype (Arbitrary, IsString)

base64FromStrict :: Base64ByteString -> Base64ByteStringL
base64FromStrict = Base64ByteStringL . L.fromStrict . fromBase64ByteString

base64ToStrict :: Base64ByteStringL -> Base64ByteString
base64ToStrict = Base64ByteString . L.toStrict . fromBase64ByteStringL

instance ToSchema Base64ByteStringL where
  schema = fromBase64ByteStringL .= fmap Base64ByteStringL base64SchemaLN

instance FromHttpApiData Base64ByteStringL where
  parseUrlPiece = fmap base64FromStrict . parseUrlPiece

instance ToHttpApiData Base64ByteStringL where
  toUrlPiece = toUrlPiece . base64ToStrict

instance S.ToParamSchema Base64ByteStringL where
  toParamSchema _ = mempty & S.type_ ?~ S.SwaggerString

base64SchemaLN :: ValueSchema NamedSwaggerDoc LByteString
base64SchemaLN = L.toStrict .= fmap L.fromStrict base64SchemaN

base64SchemaL :: ValueSchema SwaggerDoc LByteString
base64SchemaL = unnamed base64SchemaLN

--------------------------------------------------------------------------------
-- Utilities

fromBase64TextLenient :: Text -> ByteString
fromBase64TextLenient = B64.decodeLenient . Text.encodeUtf8

fromBase64Text :: Text -> Either String ByteString
fromBase64Text = B64.decode . Text.encodeUtf8

toBase64Text :: ByteString -> Text
toBase64Text = Text.decodeUtf8 . B64.encode
