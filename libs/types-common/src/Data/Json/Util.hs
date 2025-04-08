{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
    base64URLSchema,
    Base64ByteStringL (..),
    fromBase64TextLenient,
    fromBase64Text,
    toBase64Text,
  )
where

import Cassandra qualified as CQL
import Control.Lens hiding ((#), (.=))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Attoparsec.Text qualified as Atto
import Data.Attoparsec.Time qualified as Atto
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Conversion qualified as BS
import Data.ByteString.Lazy qualified as L
import Data.ByteString.UTF8 qualified as UTF8
import Data.Fixed
import Data.OpenApi qualified as S
import Data.Schema
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Data.Time.Clock
import Data.Time.Format (formatTime, parseTimeM)
import Data.Time.Lens qualified as TL
import Data.Time.Locale.Compat (defaultTimeLocale)
import Imports
import Servant
import Test.QuickCheck (Arbitrary (arbitrary))
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
    UTCTimeMillis
      <$> showUTCTimeMillis
        .= ( utcTimeTextSchema "UTCTimeMillis"
               & doc . S.schema
                 %~ (S.format ?~ "yyyy-mm-ddThh:MM:ss.qqqZ")
                   . (S.example ?~ "2021-05-12T10:52:02.671Z")
           )

utcTimeTextSchema :: Text -> ValueSchemaP NamedSwaggerDoc Text UTCTime
utcTimeTextSchema name =
  parsedText name (Atto.parseOnly (Atto.utcTime <* Atto.endOfInput))
    & doc . S.schema
      %~ (S.format ?~ "yyyy-mm-ddThh:MM:ssZ")
        . (S.example ?~ "2021-05-12T10:52:02Z")

utcTimeSchema :: ValueSchema NamedSwaggerDoc UTCTime
utcTimeSchema = showUTCTime .= utcTimeTextSchema "UTCTime"

{-# INLINE toUTCTimeMillis #-}
toUTCTimeMillis :: UTCTime -> UTCTimeMillis
toUTCTimeMillis = UTCTimeMillis . (TL.seconds . coerced @Pico @_ @Integer %~ (* 1e9) . (`div` 1e9))

{-# INLINE showUTCTimeMillis #-}
showUTCTimeMillis :: UTCTimeMillis -> Text
showUTCTimeMillis = Text.pack . formatTime defaultTimeLocale "%FT%T.%03qZ" . fromUTCTimeMillis

showUTCTime :: UTCTime -> Text
showUTCTime = Text.pack . formatTime defaultTimeLocale "%FT%TZ"

readUTCTimeMillis :: String -> Maybe UTCTimeMillis
readUTCTimeMillis = fmap toUTCTimeMillis . parseTimeM True defaultTimeLocale formatUTCTimeMillis

formatUTCTimeMillis :: String
formatUTCTimeMillis = "%FT%T%QZ"

instance Show UTCTimeMillis where
  showsPrec d = showParen (d > 10) . showString . Text.unpack . showUTCTimeMillis

instance BS.ToByteString UTCTimeMillis where
  builder = BB.byteString . UTF8.fromString . show

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
-- Aeson Object

instance S.ToParamSchema A.Object where
  toParamSchema _ =
    mempty & S.type_ ?~ S.OpenApiString

instance ToSchema A.Object where
  schema =
    named "Object" $
      id .= jsonObject

-----------------------------------------------------------------------------
-- toJSONFieldName

-- | Convenient helper to convert field names to use as JSON fields.
-- it converts the field names to snake_case.
--
-- Example:
-- newtype TeamName = TeamName { teamName :: Text }
-- deriveJSON toJSONFieldName ''teamName
--
-- would generate {To/From}JSON instances where
-- the field name is "team_name"
toJSONFieldName :: A.Options
toJSONFieldName = A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_'}

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
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Base64ByteString
  deriving newtype (Arbitrary, IsString)

instance ToSchema Base64ByteString where
  schema = fromBase64ByteString .= fmap Base64ByteString base64SchemaN

instance FromHttpApiData Base64ByteString where
  parseUrlPiece = bimap Text.pack Base64ByteString . B64U.decodeUnpadded . Text.encodeUtf8

instance ToHttpApiData Base64ByteString where
  toUrlPiece = Text.decodeUtf8With Text.lenientDecode . B64U.encodeUnpadded . fromBase64ByteString

instance S.ToParamSchema Base64ByteString where
  toParamSchema _ = mempty & S.type_ ?~ S.OpenApiString

-- base64("example") ~> "ZXhhbXBsZQo="
base64SchemaN :: ValueSchema NamedSwaggerDoc ByteString
base64SchemaN =
  (toBase64Text .= parsedText "Base64ByteString" fromBase64Text)
    & doc %~ fmap (S.schema . S.example ?~ A.String "ZXhhbXBsZQo=")

base64Schema :: ValueSchema SwaggerDoc ByteString
base64Schema = unnamed base64SchemaN

base64URLSchemaN :: ValueSchema NamedSwaggerDoc ByteString
base64URLSchemaN =
  ( (Text.decodeUtf8 . B64U.encodeUnpadded)
      .= parsedText "Base64URLByteString" (B64U.decodeUnpadded . Text.encodeUtf8)
  )
    & doc %~ fmap (S.schema . S.example ?~ A.String "ZXhhbXBsZQo=")

base64URLSchema :: ValueSchema SwaggerDoc ByteString
base64URLSchema = unnamed base64URLSchemaN

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
  toParamSchema _ = mempty & S.type_ ?~ S.OpenApiString

base64SchemaLN :: ValueSchema NamedSwaggerDoc LByteString
base64SchemaLN = L.toStrict .= fmap L.fromStrict base64SchemaN

--------------------------------------------------------------------------------
-- Utilities

fromBase64TextLenient :: Text -> ByteString
fromBase64TextLenient = B64.decodeLenient . Text.encodeUtf8

fromBase64Text :: Text -> Either String ByteString
fromBase64Text = B64.decode . Text.encodeUtf8

toBase64Text :: ByteString -> Text
toBase64Text = Text.decodeUtf8 . B64.encode
