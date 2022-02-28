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
    ReservedExtensionTagSym0,
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
import Control.Error.Util
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
          .= named "KeyPackage" (Base64ByteString .= fmap fromBase64ByteString base64Schema)
      )

data KeyPackageBundleEntry = KeyPackageBundleEntry
  { kpbeUser :: Qualified UserId,
    kpbeClient :: ClientId,
    kpbeKeyPackage :: KeyPackageData
  }
  deriving stock (Eq, Ord)

instance ToSchema KeyPackageBundleEntry where
  schema =
    object "KeyPackageBundleEntry" $
      KeyPackageBundleEntry
        <$> kpbeUser .= qualifiedObjectSchema "user" schema
        <*> kpbeClient .= field "client" schema
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

--------------------------------------------------------------------------------

newtype ProtocolVersion = ProtocolVersion {pvNumber :: Word8}
  deriving newtype (Eq, Ord, Show, Binary, Arbitrary)
  deriving (ParseMLS) via (BinaryMLS ProtocolVersion)

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

data ExtensionTag
  = ReservedExtensionTag
  | CapabilitiesExtensionTag
  | LifetimeExtensionTag
  deriving (Bounded, Enum)

$(genSingletons [''ExtensionTag])

type family ExtensionType (t :: ExtensionTag) :: * where
  ExtensionType 'ReservedExtensionTag = ()
  ExtensionType 'CapabilitiesExtensionTag = Capabilities
  ExtensionType 'LifetimeExtensionTag = Lifetime

parseExtension :: Sing t -> Get (ExtensionType t)
parseExtension SReservedExtensionTag = pure ()
parseExtension SCapabilitiesExtensionTag = parseMLS
parseExtension SLifetimeExtensionTag = parseMLS

data SomeExtension where
  SomeExtension :: Sing t -> ExtensionType t -> SomeExtension

instance Eq SomeExtension where
  SomeExtension SCapabilitiesExtensionTag caps1 == SomeExtension SCapabilitiesExtensionTag caps2 = caps1 == caps2
  SomeExtension SLifetimeExtensionTag lt1 == SomeExtension SLifetimeExtensionTag lt2 = lt1 == lt2
  _ == _ = False

instance Show SomeExtension where
  show (SomeExtension SReservedExtensionTag _) = show ()
  show (SomeExtension SCapabilitiesExtensionTag caps) = show caps
  show (SomeExtension SLifetimeExtensionTag lt) = show lt

decodeExtension :: Extension -> Maybe SomeExtension
decodeExtension e = do
  t <- safeToEnum (fromIntegral (extType e))
  hush $
    withSomeSing t $ \st ->
      decodeMLSWith' (SomeExtension st <$> parseExtension st) (extData e)

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
  deriving stock (Show, Generic)
  deriving (Arbitrary) via GenericUniform KeyPackageTBS

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
  deriving (Show)

newtype KeyPackageRef = KeyPackageRef {unKeyPackageRef :: ByteString}
  deriving stock (Show)

kpRef :: CipherSuiteTag -> KeyPackageData -> KeyPackageRef
kpRef cs =
  KeyPackageRef
    . csHash cs "MLS 1.0 KeyPackage Reference"
    . LBS.toStrict
    . kpData

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
