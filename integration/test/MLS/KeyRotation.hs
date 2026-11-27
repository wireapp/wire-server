{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

-- | Client side of the MLS signature key rotation: the proof of possession
-- expected by @PUT /clients/{client}/mls-public-keys@.
--
-- This is deliberately implemented from the documented wire format rather
-- than with wire-api, so that the tests check the format itself.
module MLS.KeyRotation
  ( X509Rotation (..),
    prepareX509Rotation,
    signKeyRotation,
    signaturePrivateKey,
  )
where

import Crypto.ECC (Curve_P256R1, Curve_P384R1, Curve_P521R1)
import Crypto.Error (CryptoFailable (..))
import Crypto.Hash.Algorithms (HashAlgorithm, SHA256 (..), SHA384 (..), SHA512 (..))
import qualified Crypto.PubKey.ECDSA as ECDSA
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.ASN1.BinaryEncoding (DER (..))
import Data.ASN1.Encoding (encodeASN1')
import Data.ASN1.Types (ASN1 (..), ASN1ConstructionType (..))
import qualified Data.Aeson as Aeson
import Data.Bits (shiftR, (.|.))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import MLS.Util
import Testlib.Prelude

-- | A local rotation of an x509 credential, with everything the backend needs
-- to accept the new key.
data X509Rotation = X509Rotation
  { oldKey :: ByteString,
    newKey :: ByteString,
    -- | key package signed with the new key
    keyPackage :: ByteString,
    -- | signature with the old key over the rotation
    signature :: ByteString
  }

-- | Rotate the x509 credential of a client locally (see
-- 'rotateX509Credential'), and build the proof of possession of both keys.
prepareX509Rotation :: (HasCallStack) => Ciphersuite -> ClientIdentity -> App X509Rotation
prepareX509Rotation suite cid = do
  oldKey <- mlscli Nothing suite cid ["public-key"] Nothing
  oldPrivateKey <- signaturePrivateKey suite cid
  newKey <- rotateX509Credential suite cid
  (keyPackage, _) <- generateKeyPackage cid suite
  signature <- signKeyRotation suite oldPrivateKey cid oldKey newKey
  pure X509Rotation {..}

-- | The private signature key of a client, read from its mls-test-cli store.
-- The store is JSON, mapping base64 "self" to the signature key pair and the
-- credential.
signaturePrivateKey :: (HasCallStack) => Ciphersuite -> ClientIdentity -> App ByteString
signaturePrivateKey suite cid = do
  gs <- getClientGroupState cid
  store <-
    maybe (assertFailure "no key store for this signature scheme") pure
      $ Map.lookup (csSignatureScheme suite) gs.keystore
  json :: Value <- either assertFailure pure (Aeson.eitherDecodeStrict store)
  keyPair <-
    json %. "c2VsZg=="
      & asList >>= \case
        (x : _) -> pure x
        [] -> assertFailure "empty key store entry"
  bytes <- keyPair %. "private" & asList >>= traverse asInt
  pure (BS.pack (map fromIntegral bytes))

-- | Sign, with the given (old) private key, the rotation of a client from the
-- old public key to the new one:
--
-- > SignWithLabel(old, "WireMLSKeyRotation", struct {
-- >   opaque client_identity<V>;   // "{user_id}:{client_id}@{domain}"
-- >   opaque old_signature_key<V>;
-- >   opaque new_signature_key<V>;
-- > })
signKeyRotation :: (HasCallStack) => Ciphersuite -> ByteString -> ClientIdentity -> ByteString -> ByteString -> App ByteString
signKeyRotation suite privateKey cid old new =
  case suite.code of
    "0x0001" -> do
      sk <- crypto (Ed25519.secretKey privateKey)
      pure (BA.convert (Ed25519.sign sk (Ed25519.toPublic sk) content))
    "0x0002" -> ecdsa (Proxy @Curve_P256R1) SHA256
    "0x0007" -> ecdsa (Proxy @Curve_P384R1) SHA384
    "0x0005" -> ecdsa (Proxy @Curve_P521R1) SHA512
    code -> assertFailure ("signKeyRotation: unsupported ciphersuite " <> code)
  where
    tbs = opaque (B8.pack (cid2Str cid)) <> opaque old <> opaque new
    content = opaque "MLS 1.0 WireMLSKeyRotation" <> opaque tbs

    crypto :: CryptoFailable a -> App a
    crypto = \case
      CryptoPassed x -> pure x
      CryptoFailed e -> assertFailure ("signKeyRotation: " <> show e)

    -- MLS encodes ECDSA signatures in DER
    ecdsa :: forall curve hash. (ECDSA.EllipticCurveECDSA curve, HashAlgorithm hash) => Proxy curve -> hash -> App ByteString
    ecdsa curve hash = do
      sk <- crypto (ECDSA.decodePrivate curve privateKey)
      sig <- liftIO (ECDSA.sign curve sk hash content)
      let (r, s) = ECDSA.signatureToIntegers curve sig
      pure (encodeASN1' DER [Start Sequence, IntVal r, IntVal s, End Sequence])

-- | A variable-length vector, as defined in RFC 9420, section 2.1.2.
opaque :: ByteString -> ByteString
opaque bs = varint (BS.length bs) <> bs
  where
    varint n
      | n < 0x40 = BS.pack [fromIntegral n]
      | n < 0x4000 = BS.pack [0x40 .|. fromIntegral (n `shiftR` 8), fromIntegral n]
      | otherwise =
          BS.pack
            [ 0x80 .|. fromIntegral (n `shiftR` 24),
              fromIntegral (n `shiftR` 16),
              fromIntegral (n `shiftR` 8),
              fromIntegral n
            ]
