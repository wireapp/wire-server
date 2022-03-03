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

module Wire.API.MLS.Extension
  ( -- * Extensions
    Extension (..),
    decodeExtension,
    parseExtension,
    ExtensionTag (..),
    CapabilitiesExtensionTagSym0,
    LifetimeExtensionTagSym0,
    SExtensionTag (..),
    SomeExtension (..),
    Capabilities (..),
    Lifetime (..),

    -- * Other types
    Timestamp (..),
    ProtocolVersion (..),
    ProtocolVersionTag (..),

    -- * Utilities
    pvTag,
    tsPOSIX,
  )
where

import Data.Binary
import Data.Singletons.TH
import Data.Time.Clock.POSIX
import Imports
import Wire.API.Arbitrary
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Serialisation

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
