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

module Wire.API.MLS.Keys
  ( KeyPair,
    MLSKeys (..),
    mkMLSKeys,
    MLSPublicKeys (..),
    mlsKeysToPublic,
  )
where

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
import GHC.Generics
import Imports hiding (First, getFirst)
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential

data MLSKeys = MLSKeys
  { mlsKeyPair_ed25519 :: First (KeyPair Ed25519),
    mlsKeyPair_ecdsa_secp256r1_sha256 :: First (KeyPair Ecdsa_secp256r1_sha256),
    mlsKeyPair_ecdsa_secp384r1_sha384 :: First (KeyPair Ecdsa_secp384r1_sha384),
    mlsKeyPair_ecdsa_secp521r1_sha512 :: First (KeyPair Ecdsa_secp521r1_sha512)
  }
  deriving (Generic)
  deriving (Semigroup, Monoid) via Generically MLSKeys

mkMLSKeys ::
  Maybe (KeyPair Ed25519) ->
  Maybe (KeyPair Ecdsa_secp256r1_sha256) ->
  Maybe (KeyPair Ecdsa_secp384r1_sha384) ->
  Maybe (KeyPair Ecdsa_secp521r1_sha512) ->
  MLSKeys
mkMLSKeys ed ec256 ec384 ec521 =
  MLSKeys (First ed) (First ec256) (First ec384) (First ec521)

newtype MLSPublicKeys = MLSPublicKeys
  { unMLSPublicKeys :: Map SignaturePurpose (Map SignatureSchemeTag ByteString)
  }
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema MLSPublicKeys
  deriving newtype (Semigroup, Monoid)

instance ToSchema MLSPublicKeys where
  schema =
    named "MLSKeys" $
      MLSPublicKeys
        <$> unMLSPublicKeys
          .= map_ (map_ base64Schema)

mlsKeysToPublic1 :: MLSKeys -> Map SignatureSchemeTag ByteString
mlsKeysToPublic1 (MLSKeys mEd mEc256 mEc384 mEc521) =
  Map.fromList $
    [(Ed25519, convert ed) | (_, ed) <- toList mEd]
      <> [ (Ecdsa_secp256r1_sha256, ECDSA.encodePublic (Proxy @Curve_P256R1) ec)
           | (_, ec) <- toList mEc256
         ]
      <> [ (Ecdsa_secp384r1_sha384, ECDSA.encodePublic (Proxy @Curve_P384R1) ec)
           | (_, ec) <- toList mEc384
         ]
      <> [ (Ecdsa_secp521r1_sha512, ECDSA.encodePublic (Proxy @Curve_P521R1) ec)
           | (_, ec) <- toList mEc521
         ]

mlsKeysToPublic :: (SignaturePurpose -> MLSKeys) -> MLSPublicKeys
mlsKeysToPublic f = flip foldMap [minBound .. maxBound] $ \purpose ->
  MLSPublicKeys (Map.singleton purpose (mlsKeysToPublic1 (f purpose)))
