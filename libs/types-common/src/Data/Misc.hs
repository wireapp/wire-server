{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
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

    -- * PlainTextPassword6
    PlainTextPassword' (..),
    PlainTextPassword6,
    PlainTextPassword8,
    plainTextPassword6,
    plainTextPassword8,
    fromPlainTextPassword,
    plainTextPassword8Unsafe,
    plainTextPassword6Unsafe,
    plainTextPassword8To6,

    -- * Typesafe FUTUREWORKS
    FutureWork (..),
    from64,
    readT,
    showT,
  )
where

import Cassandra
import Control.Lens ((.~), (?~), (^.))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.Attoparsec.ByteString.Char8 qualified as Chars
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Builder
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (toStrict)
import Data.IP (IP (IPv4, IPv6), toIPv4, toIPv6b)
import Data.OpenApi qualified as S
import Data.Range
import Data.Schema
import Data.Text qualified as Text
import Data.Text.Encoding
import Data.Text.Encoding.Error
import GHC.TypeLits (Nat)
import GHC.TypeNats (KnownNat)
import Imports
import Servant (FromHttpApiData (..))
import Test.QuickCheck (Arbitrary (arbitrary), chooseInteger)
import Test.QuickCheck qualified as QC
import Text.Read (Read (..))
import URI.ByteString hiding (Port, portNumber)
import URI.ByteString.QQ qualified as URI.QQ

--------------------------------------------------------------------------------
-- IpAddr / Port

newtype IpAddr = IpAddr {ipAddr :: IP}
  deriving stock (Eq, Ord, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema IpAddr)

instance S.ToParamSchema IpAddr where
  toParamSchema _ = mempty & S.type_ ?~ S.OpenApiString

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
      toText = decodeUtf8With lenientDecode . toStrict . toByteString

      fromText :: Text -> Either String IpAddr
      fromText =
        maybe (Left "Failed parsing IP address.") Right
          . fromByteString
          . encodeUtf8

instance ToSchema Port where
  schema = Port <$> portNumber .= schema

--------------------------------------------------------------------------------
-- Location

-- FUTUREWORK: why not use these in 'Location'?
newtype Latitude = Latitude Double deriving (NFData, Generic)

newtype Longitude = Longitude Double deriving (NFData, Generic)

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
  deriving stock (Eq, Show, Generic, Typeable)
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
    (Typeable (Fingerprint a), ToSchema (Fingerprint a)) =>
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
        either fail (pure . Fingerprint) (B64.decode bs)

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

type PlainTextPassword6 = PlainTextPassword' (6 :: Nat)

type PlainTextPassword8 = PlainTextPassword' (8 :: Nat)

plainTextPassword6 :: Text -> Maybe PlainTextPassword6
plainTextPassword6 = fmap PlainTextPassword' . checked

plainTextPassword6Unsafe :: Text -> PlainTextPassword6
plainTextPassword6Unsafe = PlainTextPassword' . unsafeRange

plainTextPassword8 :: Text -> Maybe PlainTextPassword8
plainTextPassword8 = fmap PlainTextPassword' . checked

plainTextPassword8Unsafe :: Text -> PlainTextPassword8
plainTextPassword8Unsafe = PlainTextPassword' . unsafeRange

fromPlainTextPassword :: PlainTextPassword' t -> Text
fromPlainTextPassword = fromRange . fromPlainTextPassword'

-- | Convert a 'PlainTextPassword8' to a legacy 'PlainTextPassword'.
plainTextPassword8To6 :: PlainTextPassword8 -> PlainTextPassword6
plainTextPassword8To6 = PlainTextPassword' . unsafeRange . fromPlainTextPassword

newtype PlainTextPassword' (minLen :: Nat) = PlainTextPassword'
  {fromPlainTextPassword' :: Range minLen (1024 :: Nat) Text}
  deriving stock (Eq, Generic)

deriving via (Schema (PlainTextPassword' tag)) instance (ToSchema (PlainTextPassword' tag)) => FromJSON (PlainTextPassword' tag)

deriving via (Schema (PlainTextPassword' tag)) instance (ToSchema (PlainTextPassword' tag)) => ToJSON (PlainTextPassword' tag)

deriving via (Schema (PlainTextPassword' tag)) instance (KnownNat tag, ToSchema (PlainTextPassword' tag)) => S.ToSchema (PlainTextPassword' tag)

instance Show (PlainTextPassword' minLen) where
  show _ = "PlainTextPassword' <hidden>"

instance (KnownNat (n :: Nat), Within Text n 1024) => ToSchema (PlainTextPassword' n) where
  schema = PlainTextPassword' <$> fromPlainTextPassword' .= schema

instance (KnownNat (n :: Nat), Within Text n 1024) => Arbitrary (PlainTextPassword' n) where
  arbitrary = PlainTextPassword' <$> arbitrary

-- | Usage:
-- 1. Use this type in patterns to mark FUTUREWORKS.
-- 2. Remove the label constructor -> all futureworks become compiler errors
--
-- Example:
-- >>> let (FutureWork @'LegalholdPlusFederationNotImplemented -> _remoteUsers, localUsers)
-- >>>      = partitionQualified domain qualifiedUids
newtype FutureWork label payload = FutureWork payload

-------------------------------------------------------------------------------

-- | Same as 'read' but works on 'Text'
readT :: (Read a) => Text -> Maybe a
readT = readMaybe . Text.unpack
{-# INLINE readT #-}

-- | Same as 'show' but works on 'Text'
showT :: (Show a) => a -> Text
showT = Text.pack . show
{-# INLINE showT #-}

-- | Decodes a base64 'Text' to a regular 'ByteString' (if possible)
from64 :: Text -> Either String ByteString
from64 = B64.decode . encodeUtf8
{-# INLINE from64 #-}
