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

module Wire.API.MLS.KeyPackage where

import Control.Applicative
import Control.Error.Util
import Control.Lens hiding ((.=))
import Data.Aeson (FromJSON)
import Data.Binary
import Data.Json.Util
import Data.Schema
import Data.Singletons
import Data.Singletons.TH
import qualified Data.Swagger as S
import Imports
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation

data KeyPackageUpload = KeyPackageUpload
  {kpuKeyPackages :: [KeyPackageData]}
  deriving (FromJSON, S.ToSchema) via Schema KeyPackageUpload

instance ToSchema KeyPackageUpload where
  schema =
    object "KeyPackageUpload" $
      KeyPackageUpload
        <$> kpuKeyPackages .= field "key_packages" (array schema)

newtype KeyPackageData = KeyPackageData {kpData :: LByteString}

instance ToSchema KeyPackageData where
  schema =
    (S.schema . S.example ?~ "a2V5IHBhY2thZ2UgZGF0YQo=")
      ( KeyPackageData <$> kpData
          .= named "KeyPackage" (Base64ByteString .= fmap fromBase64ByteString base64Schema)
      )

--------------------------------------------------------------------------------

data ProtocolVersion = ProtocolReserved | ProtocolMLS
  deriving stock (Bounded, Enum)
  deriving (ParseMLS) via EnumMLS Word8 ProtocolVersion

data Extension = Extension
  { extType :: Word16,
    extData :: ByteString
  }

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

decodeExtension :: Extension -> Maybe SomeExtension
decodeExtension e = do
  t <- safeToEnum (fromIntegral (extType e))
  hush $
    withSomeSing t $ \st ->
      decodeMLSWith' (SomeExtension st <$> parseExtension st) (extData e)

-- t <- parse
-- parseMLS = do
--   t <- parseMLS
--   case toSing t of
--     SomeSing st -> SomeExtension st <$> parseExtension st

data Capabilities = Capabilities
  { capVersions :: [ProtocolVersion],
    capCiphersuites :: [CipherSuite],
    capExtensions :: [Word16],
    capProposals :: [ProposalType]
  }

instance ParseMLS Capabilities where
  parseMLS =
    Capabilities
      <$> parseMLSVector @Word8 parseMLS
      <*> parseMLSVector @Word8 parseMLS
      <*> parseMLSVector @Word8 parseMLS
      <*> parseMLSVector @Word8 parseMLS

-- | Seconds since the UNIX epoch.
newtype Timestamp = Timestamp {timestampSeconds :: Word64}
  deriving newtype (ParseMLS)

data Lifetime = Lifetime
  { ltNotBefore :: Timestamp,
    ltNotAfter :: Timestamp
  }

instance ParseMLS Lifetime where
  parseMLS = Lifetime <$> parseMLS <*> parseMLS

data KeyPackageTBS = KeyPackageTBS
  { kpProtocolVersion :: ProtocolVersion,
    kpCipherSuite :: CipherSuite,
    kpInitKey :: ByteString,
    kpCredential :: Credential,
    kpExtensions :: [Extension]
  }

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

instance ParseMLS KeyPackage where
  parseMLS = KeyPackage <$> parseMLS <*> parseMLSBytes @Word16
