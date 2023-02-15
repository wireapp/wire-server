{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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

module Data.Misc
  ( -- * IpAddr / Port
    IpAddr (..),
    Port (..),

    -- * Location
    Location,
    location,
    latitude,
    longitude,
    Latitude (..),
    Longitude (..),

    -- * Time
    Milliseconds (..),

    -- * HttpsUrl
    HttpsUrl (..),
    mkHttpsUrl,
    ensureHttpsUrl,

    -- * Fingerprint
    Fingerprint (..),
    Rsa,

    -- * PlainTextPassword
    PlainTextPassword (..),

    -- * Typesafe FUTUREWORKS
    FutureWork (..),
  )
where

import Cassandra
import Control.Lens (makeLenses, (.~), (?~), (^.))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import qualified Data.Attoparsec.ByteString.Char8 as Chars
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Builder
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (toStrict)
import Data.IP (IP (IPv4, IPv6), toIPv4, toIPv6b)
import Data.Range
import Data.Schema
import Data.String.Conversions (cs)
import qualified Data.Swagger as S
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Imports
import Servant (FromHttpApiData (..))
import Test.QuickCheck (Arbitrary (arbitrary), chooseInteger)
import qualified Test.QuickCheck as QC
import Text.Read (Read (..))
import URI.ByteString hiding (Port, portNumber)
import qualified URI.ByteString.QQ as URI.QQ

--------------------------------------------------------------------------------
-- IpAddr / Port

newtype IpAddr = IpAddr {ipAddr :: IP}
  deriving stock (Eq, Ord, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema IpAddr)

instance S.ToParamSchema IpAddr where
  toParamSchema _ = mempty & S.type_ ?~ S.SwaggerString

instance FromHttpApiData IpAddr where
  parseQueryParam p = first Text.pack (runParser parser (encodeUtf8 p))

instance FromByteString IpAddr where
  parser = do
    s <- Chars.takeWhile1 (not . isSpace)
    case readMaybe (unpack s) of
      Nothing -> fail "Failed parsing bytestring as IpAddr."
      Just ip -> pure (IpAddr ip)

instance ToByteString IpAddr where
  builder = string8 . show . ipAddr

instance Read IpAddr where
  readPrec = IpAddr <$> readPrec

instance NFData IpAddr where rnf (IpAddr a) = seq a ()

-- TODO: Add an arbitrary instance for IPv6
instance Arbitrary IpAddr where
  arbitrary = IpAddr <$> QC.oneof [IPv4 <$> genIPv4, IPv6 <$> genIPv6]
    where
      genIPv4 = toIPv4 <$> replicateM 4 genByte
      genIPv6 = toIPv6b <$> replicateM 16 genByte
      genByte = QC.chooseInt (0, 255)

newtype Port = Port
  {portNumber :: Word16}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Real, Enum, Num, Integral, NFData, Arbitrary)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema Port)

instance Read Port where
  readsPrec n = map (first Port) . readsPrec n

instance ToSchema IpAddr where
  schema = toText .= parsedText "IpAddr" fromText
    where
      toText :: IpAddr -> Text
      toText = cs . toByteString

      fromText :: Text -> Either String IpAddr
      fromText = maybe (Left "Failed parsing IP address.") Right . fromByteString . cs

instance ToSchema Port where
  schema = Port <$> portNumber .= schema

--------------------------------------------------------------------------------
-- Location

data Location = Location
  { _latitude :: !Double,
    _longitude :: !Double
  }
  deriving stock (Eq, Ord, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Location

instance ToSchema Location where
  schema =
    object "Location" $
      Location
        <$> _latitude
          .= field "lat" genericToSchema
        <*> _longitude
          .= field "lon" genericToSchema

instance Show Location where
  show p =
    showString "{latitude="
      . shows (_latitude p)
      . showString ", longitude="
      . shows (_longitude p)
      $ "}"

instance NFData Location

makeLenses ''Location

-- FUTUREWORK: why not use these in 'Location'?
newtype Latitude = Latitude Double deriving (NFData, Generic)

newtype Longitude = Longitude Double deriving (NFData, Generic)

location :: Latitude -> Longitude -> Location
location (Latitude lat) (Longitude lon) =
  Location {_latitude = lat, _longitude = lon}

instance Arbitrary Location where
  arbitrary = Location <$> arbitrary <*> arbitrary

instance Cql Latitude where
  ctype = Tagged DoubleColumn

  toCql (Latitude x) = CqlDouble x

  fromCql (CqlDouble x) = pure (Latitude x)
  fromCql _ = Left "Latitude: Expected CqlDouble."

instance Cql Longitude where
  ctype = Tagged DoubleColumn

  toCql (Longitude x) = CqlDouble x

  fromCql (CqlDouble x) = pure (Longitude x)
  fromCql _ = Left "Longitude: Expected CqlDouble."

--------------------------------------------------------------------------------
-- Time

newtype Milliseconds = Ms
  { ms :: Word64
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Milliseconds
  deriving newtype (Num)

-- only generate values which can be represented exactly by double
-- precision floating points
instance Arbitrary Milliseconds where
  arbitrary = Ms . fromIntegral <$> chooseInteger (0 :: Integer, 2 ^ (53 :: Int))

-- | Convert milliseconds to 'Int64', with clipping if it doesn't fit.
msToInt64 :: Milliseconds -> Int64
msToInt64 = fromIntegral . min (fromIntegral (maxBound @Int64)) . ms

-- | Convert 'Int64' to milliseconds, rounding negative values to 0.
int64ToMs :: Int64 -> Milliseconds
int64ToMs = Ms . fromIntegral . max 0

instance ToSchema Milliseconds where
  schema = int64ToMs <$> msToInt64 .= schema

instance Cql Milliseconds where
  ctype = Tagged BigIntColumn
  toCql = CqlBigInt . msToInt64
  fromCql = \case
    CqlBigInt i -> pure $ int64ToMs i
    _ -> Left "Milliseconds: expected CqlBigInt"

--------------------------------------------------------------------------------
-- HttpsUrl

newtype HttpsUrl = HttpsUrl
  { httpsUrl :: URIRef Absolute
  }
  deriving stock (Eq, Ord, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema HttpsUrl

mkHttpsUrl :: URIRef Absolute -> Either String HttpsUrl
mkHttpsUrl uri =
  if uri ^. uriSchemeL . schemeBSL == "https"
    then Right $ HttpsUrl uri
    else Left $ "Non-HTTPS URL: " ++ show uri

ensureHttpsUrl :: URIRef Absolute -> HttpsUrl
ensureHttpsUrl = HttpsUrl . (uriSchemeL . schemeBSL .~ "https")

instance Show HttpsUrl where
  showsPrec i = showsPrec i . httpsUrl

instance ToByteString HttpsUrl where
  builder = serializeURIRef . httpsUrl

instance FromByteString HttpsUrl where
  parser = either fail pure . mkHttpsUrl =<< uriParser strictURIParserOptions

instance ToSchema HttpsUrl where
  schema =
    (decodeUtf8 . toByteString')
      .= parsedText "HttpsUrl" (runParser parser . encodeUtf8)
      & doc'
        . S.schema
        . S.example
        ?~ toJSON ("https://example.com" :: Text)

instance Cql HttpsUrl where
  ctype = Tagged BlobColumn
  toCql = CqlBlob . toByteString

  fromCql (CqlBlob t) = runParser parser (toStrict t)
  fromCql _ = Left "HttpsUrl: Expected CqlBlob"

instance Arbitrary HttpsUrl where
  arbitrary = pure $ HttpsUrl [URI.QQ.uri|https://example.com|]

--------------------------------------------------------------------------------
-- Fingerprint

-- Tag for Rsa encoded fingerprints
data Rsa

newtype Fingerprint a = Fingerprint
  { fingerprintBytes :: ByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromByteString, ToByteString, NFData)

deriving via
  (Schema (Fingerprint a))
  instance
    (ToSchema (Fingerprint a)) =>
    ToJSON (Fingerprint a)

deriving via
  (Schema (Fingerprint a))
  instance
    (ToSchema (Fingerprint a)) =>
    FromJSON (Fingerprint a)

deriving via
  (Schema (Fingerprint a))
  instance
    (ToSchema (Fingerprint a)) =>
    S.ToSchema (Fingerprint a)

instance ToSchema (Fingerprint Rsa) where
  schema =
    (decodeUtf8 . B64.encode . fingerprintBytes)
      .= parsedText "Fingerprint" (runParser p . encodeUtf8)
      & doc'
        . S.schema
        . S.example
        ?~ toJSON ("ioy3GeIjgQRsobf2EKGO3O8mq/FofFxHRqy0T4ERIZ8=" :: Text)
    where
      p :: Chars.Parser (Fingerprint Rsa)
      p = do
        bs <- parser
        either fail pure (Fingerprint <$> B64.decode bs)

instance Cql (Fingerprint a) where
  ctype = Tagged BlobColumn
  toCql = CqlBlob . toByteString

  fromCql (CqlBlob b) = pure (Fingerprint (toStrict b))
  fromCql _ = Left "Fingerprint: Expected CqlBlob"

instance Arbitrary (Fingerprint Rsa) where
  arbitrary =
    pure $
      Fingerprint
        "\138\140\183\EM\226#\129\EOTl\161\183\246\DLE\161\142\220\239&\171\241h|\\GF\172\180O\129\DC1!\159"

--------------------------------------------------------------------------------
-- Password

newtype PlainTextPassword = PlainTextPassword
  {fromPlainTextPassword :: Text}
  deriving stock (Eq, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema PlainTextPassword

instance Show PlainTextPassword where
  show _ = "PlainTextPassword <hidden>"

instance ToSchema PlainTextPassword where
  schema =
    PlainTextPassword
      <$> fromPlainTextPassword
        .= untypedRangedSchema 6 1024 schema

instance Arbitrary PlainTextPassword where
  -- TODO: why 6..1024? For tests we might want invalid passwords as well, e.g. 3 chars
  arbitrary = PlainTextPassword . fromRange <$> genRangeText @6 @1024 arbitrary

-- | Usage:
-- 1. Use this type in patterns to mark FUTUREWORKS.
-- 2. Remove the label constructor -> all futureworks become compiler errors
--
-- Example:
-- >>> let (FutureWork @'LegalholdPlusFederationNotImplemented -> _remoteUsers, localUsers)
-- >>>      = partitionQualified domain qualifiedUids
newtype FutureWork label payload = FutureWork payload
