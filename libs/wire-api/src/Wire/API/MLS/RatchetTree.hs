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

module Wire.API.MLS.RatchetTree where

import Imports
import Wire.API.MLS.Extension
import Wire.API.MLS.HPKEPublicKey
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Serialisation

data RatchetTree = RatchetTree {nodes :: [RatchetTreeNode]}

instance IsExtension RatchetTree where
  extensionType = 2

instance ParseMLS RatchetTree where
  parseMLS = RatchetTree <$> parseMLSVector @VarInt parseMLS

data RatchetTreeNodeTag = RatchetTreeLeafNodeTag | RatchetTreeParentNodeTag
  deriving (Eq, Ord, Enum, Bounded)

instance ParseMLS RatchetTreeNodeTag where
  parseMLS = parseMLSEnum @Word8 "NodeType"

data RatchetTreeNode
  = RatchetTreeParentNode RatchetTreeParent
  | RatchetTreeLeafNode LeafNode

instance ParseMLS RatchetTreeNode where
  parseMLS =
    parseMLS >>= \case
      RatchetTreeParentNodeTag -> RatchetTreeParentNode <$> parseMLS
      RatchetTreeLeafNodeTag -> RatchetTreeLeafNode <$> parseMLS

data RatchetTreeParent = RatchetTreeParent
  { encryptionKey :: HPKEPublicKey,
    parentHash :: ByteString,
    unmergedLeaves :: [Word32]
  }

instance ParseMLS RatchetTreeParent where
  parseMLS =
    RatchetTreeParent
      <$> parseMLS
      <*> parseMLSBytes @VarInt
      <*> parseMLSVector @VarInt parseMLS
