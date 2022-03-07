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
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation

data Commit = Commit
  { cProposals :: [ProposalOrRef],
    cPath :: Maybe UpdatePath
  }

instance ParseMLS Commit where
  parseMLS = Commit <$> parseMLSVector @Word32 parseMLS <*> parseMLSOptional parseMLS

data UpdatePath = UpdatePath
  { upLeaf :: KeyPackage,
    upNodes :: [UpdatePathNode]
  }

instance ParseMLS UpdatePath where
  parseMLS = UpdatePath <$> parseMLS <*> parseMLSVector @Word32 parseMLS

data UpdatePathNode = UpdatePathNode
  { upnPublicKey :: ByteString,
    upnSecret :: [HPKECiphertext]
  }

instance ParseMLS UpdatePathNode where
  parseMLS = UpdatePathNode <$> parseMLSBytes @Word16 <*> parseMLSVector @Word32 parseMLS

data HPKECiphertext = HPKECiphertext
  { hcOutput :: ByteString,
    hcCiphertext :: ByteString
  }

instance ParseMLS HPKECiphertext where
  parseMLS = HPKECiphertext <$> parseMLSBytes @Word16 <*> parseMLSBytes @Word16
