{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

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

module Wire.API.MLS.CipherSuite where

import Crypto.Error
import Crypto.Hash.Algorithms
import qualified Crypto.KDF.HKDF as HKDF
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.Schema
import qualified Data.Swagger as S
import Data.Word
import Imports
import Wire.API.Arbitrary
import Wire.API.MLS.Credential
import Wire.API.MLS.Serialisation

newtype CipherSuite = CipherSuite {cipherSuiteNumber :: Word16}
  deriving stock (Eq, Show)
  deriving newtype (ParseMLS, Arbitrary)
  deriving (S.ToSchema) via Schema CipherSuite

instance ToSchema CipherSuite where
  schema = CipherSuite <$> cipherSuiteNumber .= schema

data CipherSuiteTag = MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
  deriving stock (Bounded, Enum, Eq, Show)

-- | See https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol.html#table-5.
cipherSuiteTag :: CipherSuite -> Maybe CipherSuiteTag
cipherSuiteTag (CipherSuite n) = case n of
  1 -> pure MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
  _ -> Nothing

csHash :: CipherSuiteTag -> ByteString -> ByteString -> ByteString
csHash MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 ctx value =
  HKDF.expand (HKDF.extract @SHA256 (mempty :: ByteString) value) ctx 16

csVerifySignature :: CipherSuiteTag -> ByteString -> ByteString -> ByteString -> Bool
csVerifySignature MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 pub x sig = fromMaybe False . maybeCryptoError $ do
  pub' <- Ed25519.publicKey pub
  sig' <- Ed25519.signature sig
  pure $ Ed25519.verify pub' x sig'

csSignatureScheme :: CipherSuiteTag -> SignatureSchemeTag
csSignatureScheme MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 = Ed25519
