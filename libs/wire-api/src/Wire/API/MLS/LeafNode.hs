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

module Wire.API.MLS.LeafNode
  ( LeafIndex,
    LeafNode (..),
    LeafNodeSource (..),
    LeafNodeSourceTag (..),
    leafNodeSourceTag,
  )
where

import qualified Data.Swagger as S
import GHC.Records
import Imports
import Test.QuickCheck
import Wire.API.MLS.Capabilities
import Wire.API.MLS.Credential
import Wire.API.MLS.Extension
import Wire.API.MLS.HPKEPublicKey
import Wire.API.MLS.Lifetime
import Wire.API.MLS.Serialisation
import Wire.Arbitrary

type LeafIndex = Word32

data LeafNodeTBS = LeafNodeTBS
  { encryptionKey :: HPKEPublicKey,
    signatureKey :: ByteString,
    credential :: Credential,
    capabilities :: Capabilities,
    source :: LeafNodeSource,
    extensions :: [Extension]
  }
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform LeafNodeTBS)

instance ParseMLS LeafNodeTBS where
  parseMLS =
    LeafNodeTBS
      <$> parseMLS
      <*> parseMLSBytes @VarInt
      <*> parseMLS
      <*> parseMLS
      <*> parseMLS
      <*> parseMLSVector @VarInt parseMLS

-- | This type can only verify the signature when the LeafNodeSource is
-- LeafNodeSourceKeyPackage
data LeafNode = LeafNode
  { tbs :: LeafNodeTBS,
    signature_ :: ByteString
  }
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform LeafNode)

instance ParseMLS LeafNode where
  parseMLS =
    LeafNode
      <$> parseMLS
      <*> parseMLSBytes @VarInt

instance S.ToSchema LeafNode where
  declareNamedSchema _ = pure (mlsSwagger "LeafNode")

instance HasField "encryptionKey" LeafNode HPKEPublicKey where
  getField = (.tbs.encryptionKey)

instance HasField "signatureKey" LeafNode ByteString where
  getField = (.tbs.signatureKey)

instance HasField "credential" LeafNode Credential where
  getField = (.tbs.credential)

instance HasField "capabilities" LeafNode Capabilities where
  getField = (.tbs.capabilities)

instance HasField "source" LeafNode LeafNodeSource where
  getField = (.tbs.source)

instance HasField "extensions" LeafNode [Extension] where
  getField = (.tbs.extensions)

data LeafNodeSource
  = LeafNodeSourceKeyPackage Lifetime
  | LeafNodeSourceUpdate
  | LeafNodeSourceCommit ByteString
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform LeafNodeSource)

instance ParseMLS LeafNodeSource where
  parseMLS =
    parseMLS >>= \case
      LeafNodeSourceKeyPackageTag -> LeafNodeSourceKeyPackage <$> parseMLS
      LeafNodeSourceUpdateTag -> pure LeafNodeSourceUpdate
      LeafNodeSourceCommitTag -> LeafNodeSourceCommit <$> parseMLSBytes @VarInt

data LeafNodeSourceTag
  = LeafNodeSourceKeyPackageTag
  | LeafNodeSourceUpdateTag
  | LeafNodeSourceCommitTag
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Bounded LeafNodeSourceTag => ParseMLS LeafNodeSourceTag where
  parseMLS = parseMLSEnum @Word8 "leaf node source"

instance HasField "name" LeafNodeSourceTag Text where
  getField LeafNodeSourceKeyPackageTag = "key_package"
  getField LeafNodeSourceUpdateTag = "update"
  getField LeafNodeSourceCommitTag = "commit"

leafNodeSourceTag :: LeafNodeSource -> LeafNodeSourceTag
leafNodeSourceTag (LeafNodeSourceKeyPackage _) = LeafNodeSourceKeyPackageTag
leafNodeSourceTag LeafNodeSourceUpdate = LeafNodeSourceUpdateTag
leafNodeSourceTag (LeafNodeSourceCommit _) = LeafNodeSourceCommitTag
