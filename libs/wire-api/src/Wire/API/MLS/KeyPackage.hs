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

module Wire.API.MLS.KeyPackage
  ( KeyPackageUpload (..),
    KeyPackageBundle (..),
    KeyPackageBundleEntry (..),
    KeyPackageCount (..),
    KeyPackageData (..),
    KeyPackage (..),
    keyPackageIdentity,
    kpRef,
    kpRef',
    KeyPackageTBS (..),
    KeyPackageRef (..),
    KeyPackageUpdate (..),
  )
where

import Cassandra.CQL hiding (Set)
import Control.Applicative
import Control.Lens hiding (set, (.=))
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import Data.Id
import Data.Json.Util
import Data.Qualified
import Data.Schema
import qualified Data.Swagger as S
import GHC.Records
import Imports
import Test.QuickCheck
import Web.HttpApiData
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Context
import Wire.API.MLS.Credential
import Wire.API.MLS.Extension
import Wire.API.MLS.HPKEPublicKey
import Wire.API.MLS.LeafNode
import Wire.API.MLS.ProtocolVersion
import Wire.API.MLS.Serialisation
import Wire.Arbitrary

data KeyPackageUpload = KeyPackageUpload
  {kpuKeyPackages :: [RawMLS KeyPackage]}
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema KeyPackageUpload

instance ToSchema KeyPackageUpload where
  schema =
    object "KeyPackageUpload" $
      KeyPackageUpload
        <$> kpuKeyPackages .= field "key_packages" (array rawKeyPackageSchema)

newtype KeyPackageData = KeyPackageData {kpData :: ByteString}
  deriving stock (Eq, Ord, Show)

instance ToSchema KeyPackageData where
  schema =
    (S.schema %~ addKeyPackageSwagger)
      ( KeyPackageData
          <$> kpData
            .= named "KeyPackage" base64Schema
      )

instance Cql KeyPackageData where
  ctype = Tagged BlobColumn
  toCql = CqlBlob . LBS.fromStrict . kpData
  fromCql (CqlBlob b) = pure . KeyPackageData . LBS.toStrict $ b
  fromCql _ = Left "Expected CqlBlob"

data KeyPackageBundleEntry = KeyPackageBundleEntry
  { kpbeUser :: Qualified UserId,
    kpbeClient :: ClientId,
    kpbeRef :: KeyPackageRef,
    kpbeKeyPackage :: KeyPackageData
  }
  deriving stock (Eq, Ord, Show)

instance ToSchema KeyPackageBundleEntry where
  schema =
    object "KeyPackageBundleEntry" $
      KeyPackageBundleEntry
        <$> kpbeUser .= qualifiedObjectSchema "user" schema
        <*> kpbeClient .= field "client" schema
        <*> kpbeRef .= field "key_package_ref" schema
        <*> kpbeKeyPackage .= field "key_package" schema

newtype KeyPackageBundle = KeyPackageBundle {kpbEntries :: Set KeyPackageBundleEntry}
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema KeyPackageBundle

instance ToSchema KeyPackageBundle where
  schema =
    object "KeyPackageBundle" $
      KeyPackageBundle
        <$> kpbEntries .= field "key_packages" (set schema)

newtype KeyPackageCount = KeyPackageCount {unKeyPackageCount :: Int}
  deriving newtype (Eq, Ord, Num, Show)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema KeyPackageCount

instance ToSchema KeyPackageCount where
  schema =
    object "OwnKeyPackages" $
      KeyPackageCount <$> unKeyPackageCount .= field "count" schema

newtype KeyPackageRef = KeyPackageRef {unKeyPackageRef :: ByteString}
  deriving stock (Eq, Ord, Show)
  deriving (FromHttpApiData, ToHttpApiData, S.ToParamSchema) via Base64ByteString
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema KeyPackageRef)

instance Arbitrary KeyPackageRef where
  arbitrary = KeyPackageRef . B.pack <$> vectorOf 16 arbitrary

instance ToSchema KeyPackageRef where
  schema = named "KeyPackageRef" $ unKeyPackageRef .= fmap KeyPackageRef base64Schema

instance ParseMLS KeyPackageRef where
  parseMLS = KeyPackageRef <$> getByteString 16

instance SerialiseMLS KeyPackageRef where
  serialiseMLS = putByteString . unKeyPackageRef

instance Cql KeyPackageRef where
  ctype = Tagged BlobColumn
  toCql = CqlBlob . LBS.fromStrict . unKeyPackageRef
  fromCql (CqlBlob b) = pure . KeyPackageRef . LBS.toStrict $ b
  fromCql _ = Left "Expected CqlBlob"

-- | Compute key package ref given a ciphersuite and the raw key package data.
kpRef :: CipherSuiteTag -> KeyPackageData -> KeyPackageRef
kpRef cs =
  KeyPackageRef
    . csHash cs keyPackageContext
    . kpData

-- | Compute ref of a key package. Return 'Nothing' if the key package cipher
-- suite is invalid or unsupported.
kpRef' :: RawMLS KeyPackage -> Maybe KeyPackageRef
kpRef' kp =
  kpRef
    <$> cipherSuiteTag (kp.rmValue.cipherSuite)
    <*> pure (KeyPackageData (rmRaw kp))

--------------------------------------------------------------------------------

data KeyPackageTBS = KeyPackageTBS
  { protocolVersion :: ProtocolVersion,
    cipherSuite :: CipherSuite,
    initKey :: HPKEPublicKey,
    leafNode :: LeafNode,
    credential :: Credential,
    extensions :: [Extension]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform KeyPackageTBS

instance ParseMLS KeyPackageTBS where
  parseMLS =
    KeyPackageTBS
      <$> parseMLS
      <*> parseMLS
      <*> parseMLS
      <*> parseMLS
      <*> parseMLS
      <*> parseMLSVector @VarInt parseMLS

data KeyPackage = KeyPackage
  { tbs :: RawMLS KeyPackageTBS,
    signature_ :: ByteString
  }
  deriving stock (Eq, Show)

instance S.ToSchema KeyPackage where
  declareNamedSchema _ = pure (mlsSwagger "KeyPackage")

instance HasField "protocolVersion" KeyPackage ProtocolVersion where
  getField = (.tbs.rmValue.protocolVersion)

instance HasField "cipherSuite" KeyPackage CipherSuite where
  getField = (.tbs.rmValue.cipherSuite)

instance HasField "initKey" KeyPackage HPKEPublicKey where
  getField = (.tbs.rmValue.initKey)

instance HasField "credential" KeyPackage Credential where
  getField = (.tbs.rmValue.credential)

instance HasField "extensions" KeyPackage [Extension] where
  getField = (.tbs.rmValue.extensions)

instance HasField "leafNode" KeyPackage LeafNode where
  getField = (.tbs.rmValue.leafNode)

keyPackageIdentity :: KeyPackage -> Either Text ClientIdentity
keyPackageIdentity = decodeMLS' @ClientIdentity . (.credential.identityData)

rawKeyPackageSchema :: ValueSchema NamedSwaggerDoc (RawMLS KeyPackage)
rawKeyPackageSchema =
  rawMLSSchema "KeyPackage" decodeMLS'
    & S.schema %~ addKeyPackageSwagger

addKeyPackageSwagger :: S.Schema -> S.Schema
addKeyPackageSwagger = S.example ?~ "a2V5IHBhY2thZ2UgZGF0YQo="

instance ParseMLS KeyPackage where
  parseMLS =
    KeyPackage
      <$> parseRawMLS parseMLS
      <*> parseMLSBytes @Word16

--------------------------------------------------------------------------------

data KeyPackageUpdate = KeyPackageUpdate
  { kpupPrevious :: KeyPackageRef,
    kpupNext :: KeyPackageRef
  }
