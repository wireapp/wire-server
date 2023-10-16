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
    LeafNodeCore (..),
    LeafNodeTBS (..),
    LeafNodeTBSExtra (..),
    LeafNodeSource (..),
    LeafNodeSourceTag (..),
    leafNodeSourceTag,
  )
where

import Data.Binary
import Data.OpenApi qualified as S
import GHC.Records
import Imports
import Test.QuickCheck
import Wire.API.MLS.Capabilities
import Wire.API.MLS.Credential
import Wire.API.MLS.Extension
import Wire.API.MLS.Group
import Wire.API.MLS.HPKEPublicKey
import Wire.API.MLS.Lifetime
import Wire.API.MLS.Serialisation
import Wire.Arbitrary

type LeafIndex = Word32

-- LeafNodeCore contains fields in the intersection of LeafNode and LeafNodeTBS
--
-- https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-7.2-2
data LeafNodeCore = LeafNodeCore
  { encryptionKey :: HPKEPublicKey,
    signatureKey :: ByteString,
    credential :: Credential,
    capabilities :: Capabilities,
    source :: LeafNodeSource,
    extensions :: [Extension]
  }
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform LeafNodeCore)

-- extra fields in LeafNodeTBS, but not in LeafNode
--
-- https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-7.2-2
data LeafNodeTBSExtra
  = LeafNodeTBSExtraKeyPackage
  | LeafNodeTBSExtraUpdate GroupId LeafIndex
  | LeafNodeTBSExtraCommit GroupId LeafIndex

serialiseUntaggedLeafNodeTBSExtra :: LeafNodeTBSExtra -> Put
serialiseUntaggedLeafNodeTBSExtra LeafNodeTBSExtraKeyPackage = pure ()
serialiseUntaggedLeafNodeTBSExtra (LeafNodeTBSExtraUpdate gid idx) = do
  serialiseMLS gid
  serialiseMLS idx
serialiseUntaggedLeafNodeTBSExtra (LeafNodeTBSExtraCommit gid idx) = do
  serialiseMLS gid
  serialiseMLS idx

instance HasField "tag" LeafNodeTBSExtra LeafNodeSourceTag where
  getField = \case
    LeafNodeTBSExtraKeyPackage -> LeafNodeSourceKeyPackageTag
    LeafNodeTBSExtraCommit _ _ -> LeafNodeSourceCommitTag
    LeafNodeTBSExtraUpdate _ _ -> LeafNodeSourceUpdateTag

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-7.2-2
data LeafNodeTBS = LeafNodeTBS
  { core :: RawMLS LeafNodeCore,
    extra :: LeafNodeTBSExtra
  }

instance SerialiseMLS LeafNodeTBS where
  serialiseMLS tbs = do
    serialiseMLS tbs.core
    serialiseUntaggedLeafNodeTBSExtra tbs.extra

instance ParseMLS LeafNodeCore where
  parseMLS =
    LeafNodeCore
      <$> parseMLS
      <*> parseMLSBytes @VarInt
      <*> parseMLS
      <*> parseMLS
      <*> parseMLS
      <*> parseMLSVector @VarInt parseMLS

instance SerialiseMLS LeafNodeCore where
  serialiseMLS core = do
    serialiseMLS core.encryptionKey
    serialiseMLSBytes @VarInt core.signatureKey
    serialiseMLS core.credential
    serialiseMLS core.capabilities
    serialiseMLS core.source
    serialiseMLSVector @VarInt serialiseMLS core.extensions

-- | This type can only verify the signature when the LeafNodeSource is
-- LeafNodeSourceKeyPackage
--
-- https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-7.2-2
data LeafNode = LeafNode
  { core :: RawMLS LeafNodeCore,
    signature_ :: ByteString
  }
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform LeafNode)

instance ParseMLS LeafNode where
  parseMLS =
    LeafNode
      <$> parseMLS
      <*> parseMLSBytes @VarInt

instance SerialiseMLS LeafNode where
  serialiseMLS ln = do
    serialiseMLS ln.core
    serialiseMLSBytes @VarInt ln.signature_

instance S.ToSchema LeafNode where
  declareNamedSchema _ = pure (mlsSwagger "LeafNode")

instance HasField "encryptionKey" LeafNode HPKEPublicKey where
  getField = (.core.value.encryptionKey)

instance HasField "signatureKey" LeafNode ByteString where
  getField = (.core.value.signatureKey)

instance HasField "credential" LeafNode Credential where
  getField = (.core.value.credential)

instance HasField "capabilities" LeafNode Capabilities where
  getField = (.core.value.capabilities)

instance HasField "source" LeafNode LeafNodeSource where
  getField = (.core.value.source)

instance HasField "extensions" LeafNode [Extension] where
  getField = (.core.value.extensions)

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-7.2-2
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

instance SerialiseMLS LeafNodeSource where
  serialiseMLS (LeafNodeSourceKeyPackage lt) = do
    serialiseMLS LeafNodeSourceKeyPackageTag
    serialiseMLS lt
  serialiseMLS LeafNodeSourceUpdate =
    serialiseMLS LeafNodeSourceUpdateTag
  serialiseMLS (LeafNodeSourceCommit bs) = do
    serialiseMLS LeafNodeSourceCommitTag
    serialiseMLSBytes @VarInt bs

data LeafNodeSourceTag
  = LeafNodeSourceKeyPackageTag
  | LeafNodeSourceUpdateTag
  | LeafNodeSourceCommitTag
  deriving (Show, Eq, Ord, Enum, Bounded)

instance ParseMLS LeafNodeSourceTag where
  parseMLS = parseMLSEnum @Word8 "leaf node source"

instance SerialiseMLS LeafNodeSourceTag where
  serialiseMLS = serialiseMLSEnum @Word8

instance HasField "name" LeafNodeSourceTag Text where
  getField LeafNodeSourceKeyPackageTag = "key_package"
  getField LeafNodeSourceUpdateTag = "update"
  getField LeafNodeSourceCommitTag = "commit"

leafNodeSourceTag :: LeafNodeSource -> LeafNodeSourceTag
leafNodeSourceTag (LeafNodeSourceKeyPackage _) = LeafNodeSourceKeyPackageTag
leafNodeSourceTag LeafNodeSourceUpdate = LeafNodeSourceUpdateTag
leafNodeSourceTag (LeafNodeSourceCommit _) = LeafNodeSourceCommitTag
