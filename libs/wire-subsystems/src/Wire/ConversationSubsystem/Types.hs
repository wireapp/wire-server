-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.ConversationSubsystem.Types where

import Galley.Types.Teams (FeatureDefaults)
import Imports
import Wire.API.Conversation.Protocol (ProtocolTag)
import Wire.API.MLS.Keys (MLSKeysByPurpose, MLSPrivateKeys)
import Wire.API.Team.Feature (LegalholdConfig)

data ConversationSubsystemConfig = ConversationSubsystemConfig
  { mlsKeys :: Maybe (MLSKeysByPurpose MLSPrivateKeys),
    federationProtocols :: Maybe [ProtocolTag],
    legalholdDefaults :: FeatureDefaults LegalholdConfig,
    maxConvSize :: Word16
  }
