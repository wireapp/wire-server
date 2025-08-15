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
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term qualified as CBOR
import Crypto.Hash (SHA256, hash)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Bifunctor (first)
import Data.Bits
import Data.ByteArray (convert)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Conversion (toByteString')
import Data.ByteString.Lazy qualified as LBS
import Data.Id
import Data.OpenApi qualified as S
import Data.Schema
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
-- Prekey

data Prekey = Prekey
  { prekeyId :: PrekeyId,
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

-- | Parses a Base64 CBOR-encoded to extract the 'PrekeyId'.
--
-- The function expects the input to be a valid CBOR Map where the key @1@
-- maps to an integer value representing the ID.
--
-- Validate and extract metadata from
-- binary prekey bundles. The specific encoding handled here is **CBOR
-- (Concise Binary Object Representation)**.
--
-- = Format Specification
--
-- The input is expected to be a serialized CBOR Map adhering to the rules defined in __RFC 8949__.
--
-- The structure of the map is expected to be as follows:
--
-- * **Root Object**: CBOR Map (Major Type 5).
-- * **Key 0** (@Unsigned Int@): Protocol Version or Algorithm Identifier.
-- * **Key 1** (@Unsigned Int@): **The Prekey ID** (Target of extraction).
-- * **Key 2** (@Map@): The raw Public Key material (often a nested COSE-like structure).
--
-- = References
--
-- * <https://www.rfc-editor.org/rfc/rfc8949.html RFC 8949>: Concise Binary Object Representation (CBOR)
-- * <https://www.rfc-editor.org/rfc/rfc9052.html RFC 9052>: CBOR Object Signing and Encryption (COSE) - /Contextual/
--
-- == Errors
--
-- This function returns a 'Left String' in the following cases:
--
-- 1. The input is not valid base64 data.
-- 2. The input is not valid CBOR data.
-- 3. The root object is not a Map.
-- 4. The key @1@ is missing from the Map.
-- 5. The value associated with key @1@ is not an integer.
parseEDHOCPrekeyId :: Prekey -> Either String PrekeyId
parseEDHOCPrekeyId pk = do
  -- 1. Deserialise the Base64 ByteString.
  bs <- first ("Base64 decoding error: " <>) $ B64.decode $ toByteString' $ prekeyKey pk

  -- 2. Deserialise the ByteString into a generic CBOR Term.
  -- 'deserialiseFromBytes' returns (rest, result). We only care about the result.
  -- We convert strict ByteString to Lazy because cborg consumes Lazy.
  decodedTerm <- case CBOR.deserialiseFromBytes CBOR.decodeTerm (LBS.fromStrict bs) of
    Left err -> Left $ "CBOR decoding failed: " <> show err
    Right (rest, term)
      | LBS.null rest -> Right term -- Ensure no trailing garbage bytes
      | otherwise -> Left "Invalid: Trailing bytes detected after CBOR data"

  pairs <- validateMap decodedTerm
  lookupId pairs
  where
    validateMap :: CBOR.Term -> Either String [(CBOR.Term, CBOR.Term)]
    validateMap =
      \case
        CBOR.TMap pairs
          | length pairs == 5 -> pure pairs
          | otherwise -> Left $ "Schema Mismatch: Expected Map of 5 elements, found " <> show (length pairs)
        _ -> Left "Invalid Format: Root object is not a CBOR Map"

    -- Helper function to search the list of pairs for Key 1.
    lookupId :: [(CBOR.Term, CBOR.Term)] -> Either String PrekeyId
    lookupId =
      \case
        [] -> Left "ID not found: Key 1 is missing from the map"
        -- Iterate through the pairs...
        ((key, value) : rest) ->
          case key of
            -- Check if the key is the Integer 1 (TInt 1)
            CBOR.TInt 1 -> extractPrekeyId value
            _ -> lookupId rest

    -- Helper to ensure the value found is actually an Integer
    extractPrekeyId :: CBOR.Term -> Either String PrekeyId
    extractPrekeyId =
      \case
        CBOR.TInt i -> do
          when (i < 0) $
            Left "Value Error: Prekey ID cannot be negative"

          Right $ PrekeyId $ fromIntegral i
        CBOR.TInteger i -> do
          when (i < 0) $
            Left "Value Error: Prekey ID cannot be negative"

          Right $ PrekeyId $ fromIntegral i -- Handles larger integers
        _ -> Left "Type Mismatch: Found Key 1, but value was not an integer"

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
