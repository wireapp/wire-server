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

module Wire.API.MLS.Commit where

import Imports
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation
import Wire.Arbitrary

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-12.4-3
data Commit = Commit
  { proposals :: [ProposalOrRef],
    path :: Maybe UpdatePath
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Commit)

instance ParseMLS Commit where
  parseMLS =
    Commit
      <$> parseMLSVector @VarInt parseMLS
      <*> parseMLSOptional parseMLS

instance SerialiseMLS Commit where
  serialiseMLS c = do
    serialiseMLSVector @VarInt serialiseMLS c.proposals
    serialiseMLSOptional serialiseMLS c.path

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-7.6-2
data UpdatePath = UpdatePath
  { leaf :: RawMLS LeafNode,
    nodes :: [UpdatePathNode]
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UpdatePath)

instance ParseMLS UpdatePath where
  parseMLS = UpdatePath <$> parseMLS <*> parseMLSVector @VarInt parseMLS

instance SerialiseMLS UpdatePath where
  serialiseMLS up = do
    serialiseMLS up.leaf
    serialiseMLSVector @VarInt serialiseMLS up.nodes

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-7.6-2
data UpdatePathNode = UpdatePathNode
  { publicKey :: ByteString,
    secret :: [HPKECiphertext]
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UpdatePathNode)

instance ParseMLS UpdatePathNode where
  parseMLS = UpdatePathNode <$> parseMLSBytes @VarInt <*> parseMLSVector @VarInt parseMLS

instance SerialiseMLS UpdatePathNode where
  serialiseMLS upn = do
    serialiseMLSBytes @VarInt upn.publicKey
    serialiseMLSVector @VarInt serialiseMLS upn.secret

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-7.6-2
data HPKECiphertext = HPKECiphertext
  { output :: ByteString,
    ciphertext :: ByteString
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform HPKECiphertext)

instance ParseMLS HPKECiphertext where
  parseMLS = HPKECiphertext <$> parseMLSBytes @VarInt <*> parseMLSBytes @VarInt

instance SerialiseMLS HPKECiphertext where
  serialiseMLS (HPKECiphertext out ct) = do
    serialiseMLSBytes @VarInt out
    serialiseMLSBytes @VarInt ct
