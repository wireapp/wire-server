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
  ( MLSKeys (..),
    MLSPublicKeys (..),
    mlsKeysToPublic,
    JWK (..),
  )
where

import Crypto.PubKey.Ed25519
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.ByteArray
import Data.Json.Util
import Data.Map qualified as Map
import Data.OpenApi qualified as S
import Data.Schema
import Data.Set qualified as Set
import Imports
import Wire.API.MLS.Credential

data JWK = JWK
  { keyType :: String,
    curve :: String,
    pubKey :: ByteString
  }
  deriving (Show, Eq, Ord)

instance ToSchema JWK where
  schema =
    object "JWK" $
      JWK
        <$> (.keyType) .= field "kty" schema
        <*> (.curve) .= field "crv" schema
        <*> (.pubKey) .= field "x" base64URLSchema

mkEd25519JWK :: PublicKey -> JWK
mkEd25519JWK = JWK "OKP" "Ed25519" . convert

data MLSKeys = MLSKeys
  { mlsKeyPair_ed25519 :: Maybe (SecretKey, PublicKey)
  }
  deriving (Show)

instance Semigroup MLSKeys where
  MLSKeys Nothing <> MLSKeys ed2 = MLSKeys ed2
  MLSKeys ed1 <> MLSKeys _ = MLSKeys ed1

instance Monoid MLSKeys where
  mempty = MLSKeys Nothing

newtype MLSPublicKeys = MLSPublicKeys
  { unMLSPublicKeys :: Map SignaturePurpose (Set JWK)
  }
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema MLSPublicKeys
  deriving newtype (Semigroup, Monoid)

instance ToSchema MLSPublicKeys where
  schema =
    named "MLSKeys" $
      MLSPublicKeys
        <$> unMLSPublicKeys
          .= map_ (object "JWKSet" (field "keys" (set schema)))

mlsKeysToPublic1 :: MLSKeys -> Set JWK
mlsKeysToPublic1 (MLSKeys mEd25519key) =
  foldMap (Set.singleton . mkEd25519JWK . snd) mEd25519key

mlsKeysToPublic :: (SignaturePurpose -> MLSKeys) -> MLSPublicKeys
mlsKeysToPublic f = flip foldMap [minBound .. maxBound] $ \purpose ->
  MLSPublicKeys (Map.singleton purpose (mlsKeysToPublic1 (f purpose)))
