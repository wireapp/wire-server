{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

import Control.Lens ((?~))
import Crypto.Error
import Crypto.Hash.Algorithms
import qualified Crypto.KDF.HKDF as HKDF
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.Aeson (parseJSON, toJSON)
import Data.Proxy
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Swagger.Internal.Schema as S
import Data.Word
import Imports
import Wire.API.Arbitrary
import Wire.API.MLS.Credential
import Wire.API.MLS.Serialisation

newtype CipherSuite = CipherSuite {cipherSuiteNumber :: Word16}
  deriving stock (Eq, Show)
  deriving newtype (ParseMLS, Arbitrary)

instance ToSchema CipherSuite where
  schema =
    named "CipherSuite" $
      cipherSuiteNumber .= fmap CipherSuite (unnamed schema)

data CipherSuiteTag = MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
  deriving stock (Bounded, Enum, Eq, Show, Generic, Ord)
  deriving (Arbitrary) via (GenericUniform CipherSuiteTag)

instance S.ToSchema CipherSuiteTag where
  declareNamedSchema _ =
    pure . S.named "CipherSuiteTag" $
      ( S.paramSchemaToSchema (Proxy @Word16)
          & S.description ?~ "Index number of ciphersuite. See https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol.html#table-5"
      )

instance ToSchema CipherSuiteTag where
  schema =
    mkSchema
      (swaggerDoc @CipherSuiteTag)
      tagParser
      (Just . toJSON . cipherSuiteNumber . tagCipherSuite)
    where
      tagParser v = do
        index <- parseJSON v
        maybe
          (fail "Not a valid index number of a ciphersuite. See https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol.html#table-5.")
          pure
          (cipherSuiteTag (CipherSuite index))

-- | See https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol.html#table-5.
cipherSuiteTag :: CipherSuite -> Maybe CipherSuiteTag
cipherSuiteTag (CipherSuite n) = case n of
  1 -> pure MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
  _ -> Nothing

-- | Inverse of 'cipherSuiteTag'
tagCipherSuite :: CipherSuiteTag -> CipherSuite
tagCipherSuite MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 = CipherSuite 1

csHash :: CipherSuiteTag -> ByteString -> ByteString -> ByteString
csHash MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 ctx value =
  HKDF.expand (HKDF.extract @SHA256 (mempty :: ByteString) value) ctx 16

csVerifySignature :: CipherSuiteTag -> ByteString -> ByteString -> ByteString -> Bool
csVerifySignature MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 pub x sig =
  fromMaybe False . maybeCryptoError $ do
    pub' <- Ed25519.publicKey pub
    sig' <- Ed25519.signature sig
    pure $ Ed25519.verify pub' x sig'

csSignatureScheme :: CipherSuiteTag -> SignatureSchemeTag
csSignatureScheme MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 = Ed25519
