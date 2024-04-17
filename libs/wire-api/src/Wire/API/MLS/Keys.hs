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

module Wire.API.MLS.Keys where

import Crypto.ECC (Curve_P256R1, Curve_P384R1, Curve_P521R1)
import Crypto.PubKey.ECDSA qualified as ECDSA
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.ByteArray
import Data.Json.Util
import Data.Map qualified as Map
import Data.Monoid
import Data.OpenApi qualified as S
import Data.Proxy
import Data.Schema hiding (HasField)
import Imports hiding (First, getFirst)
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential

data MLSKeysGeneric a = MLSKeysGeneric
  { ed25519 :: a,
    ecdsa_secp256r1_sha256 :: a,
    ecdsa_secp384r1_sha384 :: a,
    ecdsa_secp521r1_sha512 :: a
  }
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema (MLSKeysGeneric a)

instance ToSchema a => ToSchema (MLSKeysGeneric a) where
  schema =
    object "MLSKeysGeneric" $
      MLSKeysGeneric
        <$> ed25519 .= field "ed25519" schema
        <*> ecdsa_secp256r1_sha256 .= field "ecdsa_secp256r1_sha256" schema
        <*> ecdsa_secp384r1_sha384 .= field "ecdsa_secp384r1_sha384" schema
        <*> ecdsa_secp521r1_sha512 .= field "ecdsa_secp521r1_sha512" schema

data MLSKeys = MLSKeys
  { mlsKeyPair_ed25519 :: KeyPair Ed25519,
    mlsKeyPair_ecdsa_secp256r1_sha256 :: KeyPair Ecdsa_secp256r1_sha256,
    mlsKeyPair_ecdsa_secp384r1_sha384 :: KeyPair Ecdsa_secp384r1_sha384,
    mlsKeyPair_ecdsa_secp521r1_sha512 :: KeyPair Ecdsa_secp521r1_sha512
  }

newtype MLSPublicKeysByPurpose = MLSPublicKeysByPurpose
  { unMLSPublicKeysByPurpose :: Map SignaturePurpose MLSPublicKeys
  }
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema MLSPublicKeysByPurpose
  deriving newtype (Semigroup, Monoid)

instance ToSchema MLSPublicKeysByPurpose where
  schema =
    named "MLSKeys" $
      MLSPublicKeysByPurpose
        <$> unMLSPublicKeysByPurpose
          .= map_ schema

type MLSPublicKeys = MLSKeysGeneric MLSPublicKey

newtype MLSPublicKey = MLSPublicKey {unwrapMLSPublicKey :: ByteString}

instance ToSchema MLSPublicKey where
  schema = named "MLSPublicKey" $ MLSPublicKey <$> unwrapMLSPublicKey .= base64Schema

mlsKeysToRemovalPublic :: MLSKeys -> MLSPublicKeys
mlsKeysToRemovalPublic (MLSKeys (_, ed) (_, ec256) (_, ec384) (_, ec521)) =
  MLSKeysGeneric
    { ed25519 = MLSPublicKey $ convert ed,
      ecdsa_secp256r1_sha256 = MLSPublicKey $ ECDSA.encodePublic (Proxy @Curve_P256R1) ec256,
      ecdsa_secp384r1_sha384 = MLSPublicKey $ ECDSA.encodePublic (Proxy @Curve_P384R1) ec384,
      ecdsa_secp521r1_sha512 = MLSPublicKey $ ECDSA.encodePublic (Proxy @Curve_P521R1) ec521
    }

mlsKeysToPublic :: (SignaturePurpose -> MLSKeys) -> MLSPublicKeysByPurpose
mlsKeysToPublic f =
  MLSPublicKeysByPurpose (Map.singleton RemovalPurpose (mlsKeysToRemovalPublic (f RemovalPurpose)))
