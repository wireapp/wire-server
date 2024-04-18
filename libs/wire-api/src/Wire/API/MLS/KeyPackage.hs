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
    DeleteKeyPackages (..),
    KeyPackage (..),
    credentialIdentityAndKey,
    keyPackageIdentity,
    kpRef,
    kpRef',
    KeyPackageTBS (..),
    KeyPackageRef (..),
    sanIdentity,
  )
where

import Cassandra.CQL hiding (Set)
import Control.Applicative
import Control.Lens hiding (set, (.=))
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor
import Data.ByteString.Lazy qualified as LBS
import Data.Id
import Data.Json.Util
import Data.OpenApi qualified as S
import Data.Qualified
import Data.Range
import Data.Schema hiding (HasField)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.X509 qualified as X509
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
  { keyPackages :: [RawMLS KeyPackage]
  }
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema KeyPackageUpload

instance ToSchema KeyPackageUpload where
  schema =
    object "KeyPackageUpload" $
      KeyPackageUpload
        <$> keyPackages .= field "key_packages" (array rawKeyPackageSchema)

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
  { user :: Qualified UserId,
    client :: ClientId,
    ref :: KeyPackageRef,
    keyPackage :: KeyPackageData
  }
  deriving stock (Eq, Ord, Show)

instance ToSchema KeyPackageBundleEntry where
  schema =
    object "KeyPackageBundleEntry" $
      KeyPackageBundleEntry
        <$> (.user) .= qualifiedObjectSchema "user" schema
        <*> (.client) .= field "client" schema
        <*> (.ref) .= field "key_package_ref" schema
        <*> (.keyPackage) .= field "key_package" schema

newtype KeyPackageBundle = KeyPackageBundle {entries :: Set KeyPackageBundleEntry}
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema KeyPackageBundle

instance ToSchema KeyPackageBundle where
  schema =
    object "KeyPackageBundle" $
      KeyPackageBundle
        <$> (.entries) .= field "key_packages" (set schema)

newtype KeyPackageCount = KeyPackageCount {unKeyPackageCount :: Int}
  deriving newtype (Eq, Ord, Num, Show)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema KeyPackageCount

instance ToSchema KeyPackageCount where
  schema =
    object "OwnKeyPackages" $
      KeyPackageCount <$> unKeyPackageCount .= field "count" schema

newtype DeleteKeyPackages = DeleteKeyPackages
  {unDeleteKeyPackages :: [KeyPackageRef]}
  deriving newtype (Eq, Ord, Show)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema DeleteKeyPackages

instance ToSchema DeleteKeyPackages where
  schema =
    object "DeleteKeyPackages" $
      DeleteKeyPackages
        <$> unDeleteKeyPackages
          .= field
            "key_packages"
            (untypedRangedSchema 1 1000 (array schema))

newtype KeyPackageRef = KeyPackageRef {unKeyPackageRef :: ByteString}
  deriving stock (Eq, Ord, Show)
  deriving (FromHttpApiData, ToHttpApiData, S.ToParamSchema) via Base64ByteString
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema KeyPackageRef)
  deriving newtype (Arbitrary)

instance ToSchema KeyPackageRef where
  schema = named "KeyPackageRef" $ unKeyPackageRef .= fmap KeyPackageRef base64Schema

instance ParseMLS KeyPackageRef where
  parseMLS = KeyPackageRef <$> parseMLSBytes @VarInt

instance SerialiseMLS KeyPackageRef where
  serialiseMLS = serialiseMLSBytes @VarInt . unKeyPackageRef

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
    . flip RawMLS ()
    . kpData

-- | Compute ref of a key package. Return 'Nothing' if the key package cipher
-- suite is invalid or unsupported.
kpRef' :: RawMLS KeyPackage -> Maybe KeyPackageRef
kpRef' kp =
  kpRef
    <$> cipherSuiteTag (kp.value.cipherSuite)
    <*> pure (KeyPackageData (raw kp))

--------------------------------------------------------------------------------

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-10-6
data KeyPackageTBS = KeyPackageTBS
  { protocolVersion :: ProtocolVersion,
    cipherSuite :: CipherSuite,
    initKey :: HPKEPublicKey,
    leafNode :: LeafNode,
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
      <*> parseMLSVector @VarInt parseMLS

instance SerialiseMLS KeyPackageTBS where
  serialiseMLS tbs = do
    serialiseMLS tbs.protocolVersion
    serialiseMLS tbs.cipherSuite
    serialiseMLS tbs.initKey
    serialiseMLS tbs.leafNode
    serialiseMLSVector @VarInt serialiseMLS tbs.extensions

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-10-6
data KeyPackage = KeyPackage
  { tbs :: RawMLS KeyPackageTBS,
    signature_ :: ByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform KeyPackage)

instance S.ToSchema KeyPackage where
  declareNamedSchema _ = pure (mlsSwagger "KeyPackage")

instance HasField "protocolVersion" KeyPackage ProtocolVersion where
  getField = (.tbs.value.protocolVersion)

instance HasField "cipherSuite" KeyPackage CipherSuite where
  getField = (.tbs.value.cipherSuite)

instance HasField "initKey" KeyPackage HPKEPublicKey where
  getField = (.tbs.value.initKey)

instance HasField "extensions" KeyPackage [Extension] where
  getField = (.tbs.value.extensions)

instance HasField "leafNode" KeyPackage LeafNode where
  getField = (.tbs.value.leafNode)

credentialIdentityAndKey :: Credential -> Either Text (ClientIdentity, Maybe X509.PubKey)
credentialIdentityAndKey (BasicCredential i) = (,) <$> decodeMLS' i <*> pure Nothing
credentialIdentityAndKey (X509Credential certs) = do
  bs <- case certs of
    [] -> Left "Invalid x509 certificate chain"
    (c : _) -> pure c
  signed <-
    first (\e -> "Failed to decode x509 certificate: " <> T.pack e) $
      X509.decodeSignedCertificate bs
  -- FUTUREWORK: verify signature
  let cert = X509.getCertificate signed
  certificateIdentityAndKey cert

keyPackageIdentity :: KeyPackage -> Either Text ClientIdentity
keyPackageIdentity kp = fst <$> credentialIdentityAndKey kp.leafNode.credential

certificateIdentityAndKey :: X509.Certificate -> Either Text (ClientIdentity, Maybe X509.PubKey)
certificateIdentityAndKey cert =
  let getNames (X509.ExtSubjectAltName names) = names
      getURI (X509.AltNameURI u) = Just u
      getURI _ = Nothing
      altNames = maybe [] getNames (X509.extensionGet (X509.certExtensions cert))
      ids = map sanIdentity (mapMaybe getURI altNames)
   in case partitionEithers ids of
        (_, (cid : _)) -> pure (cid, Just (X509.certPubKey cert))
        ((e : _), []) -> Left e
        _ -> Left "No SAN URIs found"

-- client identity format: wireapp://{userid}!{deviceid}@{host}
sanIdentity :: String -> Either Text ClientIdentity
sanIdentity s = case break (== ':') s of
  ("wireapp", ':' : '/' : '/' : s') ->
    first (\e -> e <> " (while parsing identity string " <> T.pack (show s') <> ")")
      . decodeMLSWith' parseX509ClientIdentity
      . T.encodeUtf8
      . T.pack
      $ s'
  _ -> Left "No wireapp label found"

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
      <*> parseMLSBytes @VarInt

instance SerialiseMLS KeyPackage where
  serialiseMLS kp = do
    serialiseMLS kp.tbs
    serialiseMLSBytes @VarInt kp.signature_
