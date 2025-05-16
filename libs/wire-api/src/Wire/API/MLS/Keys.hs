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

import Control.Lens ((?~))
import Crypto.ECC (Curve_P256R1, Curve_P384R1, Curve_P521R1)
import Crypto.PubKey.ECDSA qualified as ECDSA
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.Bifunctor
import Data.ByteArray (ByteArray)
import Data.ByteArray qualified as BA
import Data.Default
import Data.Json.Util
import Data.OpenApi qualified as S
import Data.Proxy
import Data.Schema hiding (HasField)
import Imports hiding (First, getFirst)
import Web.HttpApiData
import Wire.API.MLS.CipherSuite

data MLSKeysByPurpose a = MLSKeysByPurpose
  { removal :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema (MLSKeysByPurpose a)

instance (ToSchema a) => ToSchema (MLSKeysByPurpose a) where
  schema =
    object "MLSKeysByPurpose" $
      MLSKeysByPurpose
        <$> (.removal) .= field "removal" schema

data MLSKeys a = MLSKeys
  { ed25519 :: a,
    ecdsa_secp256r1_sha256 :: a,
    ecdsa_secp384r1_sha384 :: a,
    ecdsa_secp521r1_sha512 :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema (MLSKeys a)

instance (ToSchema a) => ToSchema (MLSKeys a) where
  schema =
    object "MLSKeys" $
      MLSKeys
        <$> ed25519 .= field "ed25519" schema
        <*> ecdsa_secp256r1_sha256 .= field "ecdsa_secp256r1_sha256" schema
        <*> ecdsa_secp384r1_sha384 .= field "ecdsa_secp384r1_sha384" schema
        <*> ecdsa_secp521r1_sha512 .= field "ecdsa_secp521r1_sha512" schema

data MLSPrivateKeys = MLSPrivateKeys
  { mlsKeyPair_ed25519 :: KeyPair Ed25519,
    mlsKeyPair_ecdsa_secp256r1_sha256 :: KeyPair Ecdsa_secp256r1_sha256,
    mlsKeyPair_ecdsa_secp384r1_sha384 :: KeyPair Ecdsa_secp384r1_sha384,
    mlsKeyPair_ecdsa_secp521r1_sha512 :: KeyPair Ecdsa_secp521r1_sha512
  }

type MLSPublicKeys = MLSKeys MLSPublicKey

newtype MLSPublicKey = MLSPublicKey {unwrapMLSPublicKey :: ByteString}
  deriving (Eq, Show)
  deriving (ToJSON) via Schema MLSPublicKey

instance ToSchema MLSPublicKey where
  schema = named "MLSPublicKey" $ MLSPublicKey <$> unwrapMLSPublicKey .= base64Schema

mlsKeysToPublic :: MLSPrivateKeys -> MLSPublicKeys
mlsKeysToPublic (MLSPrivateKeys (_, ed) (_, ec256) (_, ec384) (_, ec521)) =
  MLSKeys
    { ed25519 = MLSPublicKey $ BA.convert ed,
      ecdsa_secp256r1_sha256 = MLSPublicKey $ ECDSA.encodePublic (Proxy @Curve_P256R1) ec256,
      ecdsa_secp384r1_sha384 = MLSPublicKey $ ECDSA.encodePublic (Proxy @Curve_P384R1) ec384,
      ecdsa_secp521r1_sha512 = MLSPublicKey $ ECDSA.encodePublic (Proxy @Curve_P521R1) ec521
    }

data MLSPublicKeyFormat = MLSPublicKeyFormatRaw | MLSPublicKeyFormatJWK
  deriving (Eq, Ord, Show)

instance Default MLSPublicKeyFormat where
  def = MLSPublicKeyFormatRaw

instance FromHttpApiData MLSPublicKeyFormat where
  parseQueryParam "raw" = pure MLSPublicKeyFormatRaw
  parseQueryParam "jwk" = pure MLSPublicKeyFormatJWK
  parseQueryParam _ = Left "invalid MLSPublicKeyFormat"

instance ToHttpApiData MLSPublicKeyFormat where
  toQueryParam MLSPublicKeyFormatRaw = "raw"
  toQueryParam MLSPublicKeyFormatJWK = "jwk"

instance S.ToParamSchema MLSPublicKeyFormat where
  toParamSchema _ =
    mempty
      & S.type_ ?~ S.OpenApiString
      & S.enum_
        ?~ map
          (toJSON . toQueryParam)
          [MLSPublicKeyFormatRaw, MLSPublicKeyFormatJWK]

data JWK = JWK
  { keyType :: String,
    curve :: String,
    pubX :: ByteString,
    pubY :: Maybe ByteString
  }
  deriving (Show, Ord, Eq)
  deriving (ToJSON) via Schema JWK

instance ToSchema JWK where
  schema =
    object "JWK" $
      JWK
        <$> (.keyType) .= field "kty" schema
        <*> (.curve) .= field "crv" schema
        <*> (.pubX) .= field "x" base64URLSchema
        <*> (.pubY) .= maybe_ (optField "y" base64URLSchema)

type MLSPublicKeysJWK = MLSKeys JWK

mlsPublicKeysToJWK :: MLSPublicKeys -> Maybe MLSPublicKeysJWK
mlsPublicKeysToJWK (MLSKeys (MLSPublicKey ed) (MLSPublicKey ec256) (MLSPublicKey ec384) (MLSPublicKey ec521)) =
  -- The kty parameter for ECDSA is "EC", for Ed25519 it's "OKP" (octet key
  -- pair).
  -- https://www.rfc-editor.org/rfc/rfc7518.html#section-6.1

  -- The crv for P-256, P-384, and P-521 are their names.
  -- https://www.rfc-editor.org/rfc/rfc7518.html#section-6.2.1.1

  -- The x parameter is mandatory for all keys, the y parameter is mandatory for
  -- all ECDSA keys.
  -- https://www.rfc-editor.org/rfc/rfc7518.html#section-6.2.1
  ( MLSKeys
      (JWK "OKP" "Ed25519" (BA.convert ed) Nothing)
      . uncurry (JWK "EC" "P-256")
      . second Just
      <$> splitXY ec256
  )
    <*> (uncurry (JWK "EC" "P-384") . second Just <$> splitXY ec384)
    <*> (uncurry (JWK "EC" "P-521") . second Just <$> splitXY ec521)
  where
    -- Obtaining X and Y from an encoded curve point follows the logic of
    -- Crypto.ECC's encodeECPoint and decodeECPoint (the module is not
    -- exported). Points need to be encoded in uncompressed representation. This
    -- is true for ECDSA.encodePublic.
    -- https://www.rfc-editor.org/rfc/rfc8422#section-5.4.1
    splitXY :: forall {bs}. (ByteArray bs) => bs -> Maybe (bs, bs)
    splitXY mxy = do
      (m, xy) <- BA.uncons mxy
      -- The first Byte m is 4 for the uncompressed representation of curve points.
      guard (m == 4)
      -- The first half of the following Bytes belong to X and the second half
      -- to Y.
      let size = BA.length xy `div` 2
      pure $ BA.splitAt size xy

data SomeKey = SomeKey A.Value

instance ToSchema SomeKey where
  schema = mkSchema d r w
    where
      d = pure $ S.NamedSchema (Just "SomeKey") mempty
      r = fmap SomeKey . parseJSON
      w (SomeKey x) = Just (toJSON x)

mkSomeKey :: (ToJSON a) => a -> SomeKey
mkSomeKey = SomeKey . toJSON
