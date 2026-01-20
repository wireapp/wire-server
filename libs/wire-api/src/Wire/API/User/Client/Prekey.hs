{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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

module Wire.API.User.Client.Prekey
  ( PrekeyId (..),
    UncheckedPrekeyBundle (..),
    clientIdFromPrekey,
    parsePrekeyBundlePrekeyId,
    PrekeyBundlePrekeyPayload (..),
    PrekeyParseError (..),
    LastPrekey,
    lastPrekey,
    unpackLastPrekey,
    fakeLastPrekey,
    lastPrekeyId,
    PrekeyBundle (..),
    ClientPrekey (..),
  )
where

import Cassandra (ColumnType (IntColumn), Cql (ctype, fromCql, toCql), Tagged (..), Value (CqlInt))
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Crypto.Hash (SHA256, hash)
import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.Aeson qualified as A
import Data.Bifunctor (first)
import Data.Bits
import Data.ByteArray (convert)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Conversion qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.Id
import Data.Json.Util (base64Schema)
import Data.OpenApi qualified as S
import Data.Schema (Schema (..), ToSchema (..), array, field, named, object, withParser, (.=))
import Data.Text.Encoding (encodeUtf8)
import Imports
import Wire.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

-- | We define PrekeyId as Word16, but it dismisses to 32-bits unsigned standard IDs.
newtype PrekeyId = PrekeyId {keyId :: Word16}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, Arbitrary, S.ToSchema, ToSchema)

instance Cql PrekeyId where
  ctype = Tagged IntColumn
  toCql = CqlInt . fromIntegral . keyId
  fromCql (CqlInt i) = pure $ PrekeyId (fromIntegral i)
  fromCql _ = Left "PrekeyId: Int expected"

--------------------------------------------------------------------------------
-- PrekeyBundle Specific Types
newtype PrekeyBundlePublicKey = PrekeyBundlePublicKey {unPrekeyBundlePublicKey :: ByteString}
  deriving stock (Eq, Show, Generic)

instance ToJSON PrekeyBundlePublicKey where
  toJSON = A.toJSON . B.fromByteString @Text . B64.encode . unPrekeyBundlePublicKey

instance FromJSON PrekeyBundlePublicKey where
  parseJSON = withText "PrekeyBundlePublicKey" $ \t ->
    either (const $ fail "Not base 64-encoded") (pure . PrekeyBundlePublicKey) $
      B64.decode (B.toByteString' t)

instance ToSchema PrekeyBundlePublicKey where
  schema = named "PrekeyBundlePublicKey" $ PrekeyBundlePublicKey <$> unPrekeyBundlePublicKey .= base64Schema

newtype PrekeyBundleIdentityKey = PrekeyBundleIdentityKey {unPrekeyBundleIdentityKey :: PrekeyBundlePublicKey}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToSchema)

newtype PrekeyBundleSignature = PrekeyBundleSignature {unPrekeyBundleSignature :: ByteString}
  deriving stock (Eq, Show, Generic)

instance ToJSON PrekeyBundleSignature where
  toJSON = A.toJSON . B.fromByteString @Text . B64.encode . unPrekeyBundleSignature

instance FromJSON PrekeyBundleSignature where
  parseJSON = withText "PrekeyBundleSignature" $ \t ->
    either (const $ fail "Not base 64-encoded") (pure . PrekeyBundleSignature) $
      B64.decode (B.toByteString' t)

instance ToSchema PrekeyBundleSignature where
  schema = named "PrekeyBundleSignature" $ PrekeyBundleSignature <$> unPrekeyBundleSignature .= base64Schema

-- Decoders for new types

decodePrekeyBundlePublicKey :: CBOR.Decoder s PrekeyBundlePublicKey
decodePrekeyBundlePublicKey = do
  n <- CBOR.decodeMapLen
  unless (n == 1) $ fail $ "Schema Mismatch: Expected Map of 1 element, found " <> show n
  k <- CBOR.decodeInt
  unless (k == 0) $ fail $ "Unknown Key: Expected 0, found " <> show k
  PrekeyBundlePublicKey <$> CBOR.decodeBytes

decodePrekeyBundleIdentityKey :: CBOR.Decoder s PrekeyBundleIdentityKey
decodePrekeyBundleIdentityKey = do
  n <- CBOR.decodeMapLen
  unless (n == 1) $ fail $ "Schema Mismatch: Expected Map of 1 element, found " <> show n
  k <- CBOR.decodeInt
  unless (k == 0) $ fail $ "Unknown Key: Expected 0, found " <> show k
  PrekeyBundleIdentityKey <$> decodePrekeyBundlePublicKey

--------------------------------------------------------------------------------
-- UncheckedPrekeyBundle

data UncheckedPrekeyBundle = UncheckedPrekeyBundle
  { prekeyId :: PrekeyId,
    -- | Prekey bundle
    prekeyKey :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UncheckedPrekeyBundle)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema UncheckedPrekeyBundle

instance ToSchema UncheckedPrekeyBundle where
  schema =
    object "UncheckedPrekeyBundle" $
      UncheckedPrekeyBundle
        <$> prekeyId .= field "id" schema
        <*> prekeyKey .= field "key" schema

-- | Construct a new client ID from a prekey.
--
-- This works by taking the SHA256 hash of the prekey, truncating it to its
-- first 8 bytes, and interpreting the resulting bytestring as a big endian
-- Word64.
clientIdFromPrekey :: UncheckedPrekeyBundle -> ClientId
clientIdFromPrekey =
  ClientId
    . foldl' (\w d -> (w `shiftL` 8) .|. fromIntegral d) 0
    . BS.unpack
    . BS.take 8
    . convert
    . hash @ByteString @SHA256
    . encodeUtf8
    . prekeyKey

data PrekeyParseError
  = PrekeyParseBase64Error String
  | -- | Byte offset and error message
    PrekeyParseCborError Int64 String
  | PrekeyParseTrailingBytes
  deriving stock (Eq, Show, Generic)

-- | Represents the Prekey Bundle payload.
--
-- Structure based on `PrekyBundle` from proteus <https://github.com/wireapp/proteus/blob/b92dbc2d0c77105cae3911a7388acba05450a06d/src/internal/keys.rs#L246-L253>
data PrekeyBundlePrekeyPayload = PrekeyBundlePrekeyPayload
  { -- | Key 0
    prekeyBundleProtocolVersion :: Word,
    -- | Key 1
    prekeyBundlePrekeyId :: PrekeyId,
    -- | Key 2
    prekeyBundleIdentityKey :: PrekeyBundleIdentityKey,
    -- | Key 3
    prekeyBundleSignedPrekey :: PrekeyBundlePublicKey,
    -- | Key 4
    prekeyBundleOneTimePrekey :: Maybe PrekeyBundleSignature
  }
  deriving stock (Eq, Show, Generic)

-- | Parses a Base64 CBOR-encoded payload to extract the 'PrekeyId'.
parsePrekeyBundlePrekeyId :: UncheckedPrekeyBundle -> Either PrekeyParseError PrekeyId
parsePrekeyBundlePrekeyId pk = do
  bs <- first (PrekeyParseBase64Error . ("Base64 decoding error: " <>)) $ B64.decode $ B.toByteString' $ prekeyKey pk
  case CBOR.deserialiseFromBytes decodePrekeyBundlePrekeyPayload (LBS.fromStrict bs) of
    Left (CBOR.DeserialiseFailure off msg) -> Left $ PrekeyParseCborError off msg
    Right (rest, payload)
      | LBS.null rest -> Right (prekeyBundlePrekeyId payload)
      | otherwise -> Left PrekeyParseTrailingBytes

decodePrekeyBundlePrekeyPayload :: CBOR.Decoder s PrekeyBundlePrekeyPayload
decodePrekeyBundlePrekeyPayload = do
  n <- CBOR.decodeMapLen
  (m0, m1, m2, m3, m4) <- go n (Nothing, Nothing, Nothing, Nothing, Nothing)
  PrekeyBundlePrekeyPayload
    <$> maybe (fail "Missing Key 0") pure m0
    <*> maybe (fail "Missing Key 1") pure m1
    <*> maybe (fail "Missing Key 2") pure m2
    <*> maybe (fail "Missing Key 3") pure m3
    <*> pure m4 -- Key 4 is optional
  where
    go 0 acc = pure acc
    go i (m0, m1, m2, m3, m4) = do
      k <- CBOR.decodeInt
      case k of
        0 -> do
          v <- CBOR.decodeWord
          go (i - 1) (Just v, m1, m2, m3, m4)
        1 -> do
          v <- CBOR.decodeInt
          when (v < 0) $ fail "Value Error: Prekey ID cannot be negative"
          go (i - 1) (m0, Just (PrekeyId (fromIntegral v)), m2, m3, m4)
        2 -> do
          v <- decodePrekeyBundleIdentityKey
          go (i - 1) (m0, m1, Just v, m3, m4)
        3 -> do
          v <- decodePrekeyBundlePublicKey
          go (i - 1) (m0, m1, m2, Just v, m4)
        4 ->
          go (i - 1) (m0, m1, m2, m3, Nothing) -- If null, Nothing
        other -> fail $ "Unknown Key: " <> show other

--------------------------------------------------------------------------------
-- LastPrekey

newtype LastPrekey = LastPrekey
  {unpackLastPrekey :: UncheckedPrekeyBundle}
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema LastPrekey

instance ToSchema LastPrekey where
  schema = LastPrekey <$> unpackLastPrekey .= schema `withParser` check
    where
      check x =
        x
          <$ guard (prekeyId x == lastPrekeyId)
            <|> fail "Invalid last prekey ID"

instance Arbitrary LastPrekey where
  arbitrary = lastPrekey <$> arbitrary

lastPrekeyId :: PrekeyId
lastPrekeyId = PrekeyId maxBound

lastPrekey :: Text -> LastPrekey
lastPrekey = LastPrekey . UncheckedPrekeyBundle lastPrekeyId

-- for tests only
-- This fake last prekey has the wrong prekeyId
fakeLastPrekey :: LastPrekey
fakeLastPrekey = LastPrekey $ UncheckedPrekeyBundle (PrekeyId 7) "pQABAQcCoQBYIDXdN8VlKb5lbgPmoDPLPyqNIEyShG4oT/DlW0peRRZUA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="

--------------------------------------------------------------------------------
-- PrekeyBundle

data PrekeyBundle = PrekeyBundle
  { prekeyUser :: UserId,
    prekeyClients :: [ClientPrekey]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform PrekeyBundle)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema PrekeyBundle

instance ToSchema PrekeyBundle where
  schema =
    object "PrekeyBundle" $
      PrekeyBundle
        <$> prekeyUser .= field "user" schema
        <*> prekeyClients .= field "clients" (array schema)

--------------------------------------------------------------------------------
-- ClientPrekey

data ClientPrekey = ClientPrekey
  { prekeyClient :: ClientId,
    prekeyData :: UncheckedPrekeyBundle
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientPrekey)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ClientPrekey

instance ToSchema ClientPrekey where
  schema =
    object "ClientPrekey" $
      ClientPrekey
        <$> prekeyClient .= field "client" schema
        <*> prekeyData .= field "prekey" schema
