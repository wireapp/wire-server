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
    Prekey (..),
    clientIdFromPrekey,
    parseEDHOCPrekeyId,
    EdhocPrekeyPayload (..),
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
-- EDHOC Specific Types
newtype EdhocPublicKey = EdhocPublicKey {unEdhocPublicKey :: ByteString}
  deriving stock (Eq, Show, Generic)

instance ToJSON EdhocPublicKey where
  toJSON = A.toJSON . B.fromByteString @Text . B64.encode . unEdhocPublicKey

instance FromJSON EdhocPublicKey where
  parseJSON = withText "EdhocPublicKey" $ \t ->
    either (const $ fail "Not base 64-encoded") (pure . EdhocPublicKey) $
      B64.decode (B.toByteString' t)

instance ToSchema EdhocPublicKey where
  schema = named "EdhocPublicKey" $ EdhocPublicKey <$> unEdhocPublicKey .= base64Schema

newtype EdhocIdentityKey = EdhocIdentityKey {unEdhocIdentityKey :: EdhocPublicKey}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToSchema)

newtype EdhocSignature = EdhocSignature {unEdhocSignature :: ByteString}
  deriving stock (Eq, Show, Generic)

instance ToJSON EdhocSignature where
  toJSON = A.toJSON . B.fromByteString @Text . B64.encode . unEdhocSignature

instance FromJSON EdhocSignature where
  parseJSON = withText "EdhocSignature" $ \t ->
    either (const $ fail "Not base 64-encoded") (pure . EdhocSignature) $
      B64.decode (B.toByteString' t)

instance ToSchema EdhocSignature where
  schema = named "EdhocSignature" $ EdhocSignature <$> unEdhocSignature .= base64Schema

-- Decoders for new types

decodeEdhocPublicKey :: CBOR.Decoder s EdhocPublicKey
decodeEdhocPublicKey = do
  n <- CBOR.decodeMapLen
  unless (n == 1) $ fail $ "Schema Mismatch: Expected Map of 1 element, found " <> show n
  k <- CBOR.decodeInt
  unless (k == 0) $ fail $ "Unknown Key: Expected 0, found " <> show k
  EdhocPublicKey <$> CBOR.decodeBytes

decodeEdhocIdentityKey :: CBOR.Decoder s EdhocIdentityKey
decodeEdhocIdentityKey = do
  n <- CBOR.decodeMapLen
  unless (n == 1) $ fail $ "Schema Mismatch: Expected Map of 1 element, found " <> show n
  k <- CBOR.decodeInt
  unless (k == 0) $ fail $ "Unknown Key: Expected 0, found " <> show k
  EdhocIdentityKey <$> decodeEdhocPublicKey

--------------------------------------------------------------------------------
-- Prekey

data Prekey = Prekey
  { prekeyId :: PrekeyId,
    -- | Prekey bundle
    prekeyKey :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Prekey)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Prekey

instance ToSchema Prekey where
  schema =
    object "Prekey" $
      Prekey
        <$> prekeyId .= field "id" schema
        <*> prekeyKey .= field "key" schema

-- | Construct a new client ID from a prekey.
--
-- This works by taking the SHA256 hash of the prekey, truncating it to its
-- first 8 bytes, and interpreting the resulting bytestring as a big endian
-- Word64.
clientIdFromPrekey :: Prekey -> ClientId
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

-- | Represents the EDHOC Prekey Bundle payload.
--
-- Structure based on `PrekyBundle` from proteus <https://github.com/wireapp/proteus/blob/b92dbc2d0c77105cae3911a7388acba05450a06d/src/internal/keys.rs#L246-L253>
data EdhocPrekeyPayload = EdhocPrekeyPayload
  { -- | Key 0
    edhocProtocolVersion :: Word,
    -- | Key 1
    edhocPrekeyId :: PrekeyId,
    -- | Key 2
    edhocIdentityKey :: EdhocIdentityKey,
    -- | Key 3
    edhocSignedPrekey :: EdhocPublicKey,
    -- | Key 4
    edhocOneTimePrekey :: Maybe EdhocSignature
  }
  deriving stock (Eq, Show, Generic)

-- | Parses a Base64 CBOR-encoded payload to extract the 'PrekeyId'.
parseEDHOCPrekeyId :: Prekey -> Either PrekeyParseError PrekeyId
parseEDHOCPrekeyId pk = do
  bs <- first (PrekeyParseBase64Error . ("Base64 decoding error: " <>)) $ B64.decode $ B.toByteString' $ prekeyKey pk
  case CBOR.deserialiseFromBytes decodeEdhocPrekeyPayload (LBS.fromStrict bs) of
    Left (CBOR.DeserialiseFailure off msg) -> Left $ PrekeyParseCborError off msg
    Right (rest, payload)
      | LBS.null rest -> Right (edhocPrekeyId payload)
      | otherwise -> Left PrekeyParseTrailingBytes

decodeEdhocPrekeyPayload :: CBOR.Decoder s EdhocPrekeyPayload
decodeEdhocPrekeyPayload = do
  n <- CBOR.decodeMapLen
  (m0, m1, m2, m3, m4) <- go n (Nothing, Nothing, Nothing, Nothing, Nothing)
  EdhocPrekeyPayload
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
          v <- decodeEdhocIdentityKey
          go (i - 1) (m0, m1, Just v, m3, m4)
        3 -> do
          v <- decodeEdhocPublicKey
          go (i - 1) (m0, m1, m2, Just v, m4)
        4 ->
          go (i - 1) (m0, m1, m2, m3, Nothing) -- If null, Nothing
        other -> fail $ "Unknown Key: " <> show other

--------------------------------------------------------------------------------
-- LastPrekey

newtype LastPrekey = LastPrekey
  {unpackLastPrekey :: Prekey}
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
lastPrekey = LastPrekey . Prekey lastPrekeyId

-- for tests only
-- This fake last prekey has the wrong prekeyId
fakeLastPrekey :: LastPrekey
fakeLastPrekey = LastPrekey $ Prekey (PrekeyId 7) "pQABAQcCoQBYIDXdN8VlKb5lbgPmoDPLPyqNIEyShG4oT/DlW0peRRZUA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="

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
    prekeyData :: Prekey
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
