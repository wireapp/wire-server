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

module Wire.API.MLS.ProposalTag where

import Data.Binary
import Imports
import Wire.API.MLS.Serialisation
import Wire.Arbitrary

data ProposalTag
  = AddProposalTag
  | UpdateProposalTag
  | RemoveProposalTag
  | PreSharedKeyProposalTag
  | ReInitProposalTag
  | ExternalInitProposalTag
  | GroupContextExtensionsProposalTag
  deriving stock (Bounded, Enum, Eq, Ord, Generic, Show)
  deriving (Arbitrary) via GenericUniform ProposalTag

instance ParseMLS ProposalTag where
  parseMLS = parseMLSEnum @Word16 "proposal type"

instance SerialiseMLS ProposalTag where
  serialiseMLS = serialiseMLSEnum @Word16
