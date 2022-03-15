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

module Wire.API.MLS.KeyPackage
  ( -- * API types
    KeyPackageUpload (..),
    KeyPackageBundle (..),
    KeyPackageBundleEntry (..),
    KeyPackageCount (..),
    KeyPackageData (..),
    KeyPackage (..),
    KeyPackageTBS (..),
    KeyPackageRef (..),

    -- * Key package types
    Timestamp (..),
    ProtocolVersion (..),
    ProtocolVersionTag (..),
    pvTag,
    Extension (..),

    -- * Extensions
    decodeExtension,
    parseExtension,
    ExtensionTag (..),
    CapabilitiesExtensionTagSym0,
    LifetimeExtensionTagSym0,
    SExtensionTag (..),
    SomeExtension (..),
    Capabilities (..),
    Lifetime (..),

    -- * Utilities
    tsPOSIX,
    kpRef,
    kpSigOffset,
  )
where

import Control.Applicative
import Control.Lens hiding (set, (.=))
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import Data.Id
import Data.Json.Util
import Data.Qualified
import Data.Schema
import Data.Singletons
import Data.Singletons.TH
import qualified Data.Swagger as S
import Data.Time.Clock.POSIX
import Imports
import Web.HttpApiData
import Wire.API.Arbitrary
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.Serialisation

data KeyPackageUpload = KeyPackageUpload
  {kpuKeyPackages :: [KeyPackageData]}
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema KeyPackageUpload

instance ToSchema KeyPackageUpload where
  schema =
    object "KeyPackageUpload" $
      KeyPackageUpload
        <$> kpuKeyPackages .= field "key_packages" (array schema)

newtype KeyPackageData = KeyPackageData {kpData :: LByteString}
  deriving stock (Eq, Ord, Show)

instance ToSchema KeyPackageData where
  schema =
    (S.schema . S.example ?~ "a2V5IHBhY2thZ2UgZGF0YQo=")
      ( KeyPackageData <$> kpData
          .= named "KeyPackage" base64SchemaL
      )

data KeyPackageBundleEntry = KeyPackageBundleEntry
  { kpbeUser :: Qualified UserId,
    kpbeClient :: ClientId,
    kpbeRef :: KeyPackageRef,
    kpbeKeyPackage :: KeyPackageData
  }
  deriving stock (Eq, Ord)

instance ToSchema KeyPackageBundleEntry where
  schema =
    object "KeyPackageBundleEntry" $
      KeyPackageBundleEntry
        <$> kpbeUser .= qualifiedObjectSchema "user" schema
        <*> kpbeClient .= field "client" schema
        <*> kpbeRef .= field "key_package_ref" schema
        <*> kpbeKeyPackage .= field "key_package" schema

newtype KeyPackageBundle = KeyPackageBundle {kpbEntries :: Set KeyPackageBundleEntry}
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

instance ToSchema KeyPackageRef where
  schema = named "KeyPackageRef" $ unKeyPackageRef .= fmap KeyPackageRef base64Schema

instance ParseMLS KeyPackageRef where
  parseMLS = KeyPackageRef <$> getByteString 16

kpRef :: CipherSuiteTag -> KeyPackageData -> KeyPackageRef
kpRef cs =
  KeyPackageRef
    . csHash cs "MLS 1.0 KeyPackage Reference"
    . LBS.toStrict
    . kpData

--------------------------------------------------------------------------------

newtype ProtocolVersion = ProtocolVersion {pvNumber :: Word8}
  deriving newtype (Eq, Ord, Show, Binary, Arbitrary)
  deriving (ParseMLS) via (BinaryMLS ProtocolVersion)
  deriving (S.ToSchema) via Schema ProtocolVersion

instance ToSchema ProtocolVersion where
  schema = ProtocolVersion <$> pvNumber .= schema

data ProtocolVersionTag = ProtocolMLS10 | ProtocolMLSDraft11
  deriving stock (Bounded, Enum, Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform ProtocolVersionTag

pvTag :: ProtocolVersion -> Maybe ProtocolVersionTag
pvTag (ProtocolVersion v) = case v of
  1 -> pure ProtocolMLS10
  200 -> pure ProtocolMLSDraft11
  _ -> Nothing

data Extension = Extension
  { extType :: Word16,
    extData :: ByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform Extension

instance ParseMLS Extension where
  parseMLS = Extension <$> parseMLS <*> parseMLSBytes @Word32

instance ToSchema Extension where
  schema =
    object "Extension" $
      Extension
        <$> extType .= field "type" schema
        <*> extData .= field "data" base64Schema

data ExtensionTag
  = CapabilitiesExtensionTag
  | LifetimeExtensionTag
  deriving (Bounded, Enum)

$(genSingletons [''ExtensionTag])

type family ExtensionType (t :: ExtensionTag) :: * where
  ExtensionType 'CapabilitiesExtensionTag = Capabilities
  ExtensionType 'LifetimeExtensionTag = Lifetime

parseExtension :: Sing t -> Get (ExtensionType t)
parseExtension SCapabilitiesExtensionTag = parseMLS
parseExtension SLifetimeExtensionTag = parseMLS

data SomeExtension where
  SomeExtension :: Sing t -> ExtensionType t -> SomeExtension

instance Eq SomeExtension where
  SomeExtension SCapabilitiesExtensionTag caps1 == SomeExtension SCapabilitiesExtensionTag caps2 = caps1 == caps2
  SomeExtension SLifetimeExtensionTag lt1 == SomeExtension SLifetimeExtensionTag lt2 = lt1 == lt2
  _ == _ = False

instance Show SomeExtension where
  show (SomeExtension SCapabilitiesExtensionTag caps) = show caps
  show (SomeExtension SLifetimeExtensionTag lt) = show lt

decodeExtension :: Extension -> Either Text (Maybe SomeExtension)
decodeExtension e = do
  case toMLSEnum' (extType e) of
    Left MLSEnumUnkonwn -> pure Nothing
    Left MLSEnumInvalid -> Left "Invalid extension type"
    Right t -> withSomeSing t $ \st ->
      Just <$> decodeMLSWith' (SomeExtension st <$> parseExtension st) (extData e)

data Capabilities = Capabilities
  { capVersions :: [ProtocolVersion],
    capCiphersuites :: [CipherSuite],
    capExtensions :: [Word16],
    capProposals :: [Word16]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Capabilities)

instance ParseMLS Capabilities where
  parseMLS =
    Capabilities
      <$> parseMLSVector @Word8 parseMLS
      <*> parseMLSVector @Word8 parseMLS
      <*> parseMLSVector @Word8 parseMLS
      <*> parseMLSVector @Word8 parseMLS

-- | Seconds since the UNIX epoch.
newtype Timestamp = Timestamp {timestampSeconds :: Word64}
  deriving newtype (Eq, Show, Arbitrary, ParseMLS)

tsPOSIX :: Timestamp -> POSIXTime
tsPOSIX = fromIntegral . timestampSeconds

data Lifetime = Lifetime
  { ltNotBefore :: Timestamp,
    ltNotAfter :: Timestamp
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform Lifetime

instance ParseMLS Lifetime where
  parseMLS = Lifetime <$> parseMLS <*> parseMLS

data KeyPackageTBS = KeyPackageTBS
  { kpProtocolVersion :: ProtocolVersion,
    kpCipherSuite :: CipherSuite,
    kpInitKey :: ByteString,
    kpCredential :: Credential,
    kpExtensions :: [Extension]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform KeyPackageTBS
  deriving (S.ToSchema) via Schema KeyPackageTBS

instance ToSchema KeyPackageTBS where
  schema =
    object "KeyPackageTBS" $
      KeyPackageTBS
        <$> kpProtocolVersion .= field "protocol-version" schema
        <*> kpCipherSuite .= field "cipher-suite" schema
        <*> kpInitKey .= field "init-key" base64Schema
        <*> kpCredential .= field "credential" schema
        <*> kpExtensions .= field "extensions" (array schema)

instance ParseMLS KeyPackageTBS where
  parseMLS =
    KeyPackageTBS
      <$> parseMLS
      <*> parseMLS
      <*> parseMLSBytes @Word16
      <*> parseMLS
      <*> parseMLSVector @Word32 parseMLS

data KeyPackage = KeyPackage
  { kpTBS :: KeyPackageTBS,
    kpSignature :: ByteString
  }
  deriving stock (Eq, Show)
  deriving (S.ToSchema) via Schema KeyPackage

instance ToSchema KeyPackage where
  schema =
    object "KeyPackage" $
      KeyPackage
        <$> kpTBS .= field "key-package-tbs" schema
        <*> kpSignature .= field "signature" base64Schema

instance ParseMLS KeyPackage where
  parseMLS = fst <$> kpSigOffset

-- | Parse a key package, and also return the offset of the signature. This can
-- be used to reconstruct the signed data.
kpSigOffset :: Get (KeyPackage, Int64)
kpSigOffset = do
  kp <- parseMLS
  off <- bytesRead
  sig <- parseMLSBytes @Word16
  pure (KeyPackage kp sig, off)
