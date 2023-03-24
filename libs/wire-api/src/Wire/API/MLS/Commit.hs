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

data Commit = Commit
  { cProposals :: [ProposalOrRef],
    cPath :: Maybe UpdatePath
  }
  deriving (Eq, Show)

instance ParseMLS Commit where
  parseMLS =
    Commit
      <$> traceMLS "proposals" (parseMLSVector @VarInt parseMLS)
      <*> traceMLS "update path" (parseMLSOptional parseMLS)

data UpdatePath = UpdatePath
  { upLeaf :: RawMLS LeafNode,
    upNodes :: [UpdatePathNode]
  }
  deriving (Eq, Show)

instance ParseMLS UpdatePath where
  parseMLS = UpdatePath <$> parseMLS <*> parseMLSVector @VarInt parseMLS

data UpdatePathNode = UpdatePathNode
  { upnPublicKey :: ByteString,
    upnSecret :: [HPKECiphertext]
  }
  deriving (Eq, Show)

instance ParseMLS UpdatePathNode where
  parseMLS = UpdatePathNode <$> parseMLSBytes @VarInt <*> parseMLSVector @VarInt parseMLS

data HPKECiphertext = HPKECiphertext
  { hcOutput :: ByteString,
    hcCiphertext :: ByteString
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform HPKECiphertext)

instance ParseMLS HPKECiphertext where
  parseMLS = HPKECiphertext <$> parseMLSBytes @VarInt <*> parseMLSBytes @VarInt

instance SerialiseMLS HPKECiphertext where
  serialiseMLS (HPKECiphertext out ct) = do
    serialiseMLSBytes @VarInt out
    serialiseMLSBytes @VarInt ct
