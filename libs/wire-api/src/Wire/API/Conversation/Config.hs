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
module Wire.API.Conversation.Config
  ( ConversationSubsystemConfig (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Wire.API.Conversation.Protocol (ProtocolTag)
import Wire.API.MLS.Keys (MLSKeysByPurpose, MLSPrivateKeys)
import Wire.API.Team.Feature (LegalholdConfig)
import Wire.API.Team.FeatureFlags (FeatureDefaults)

data ConversationSubsystemConfig = ConversationSubsystemConfig
  { mlsKeys :: Maybe (MLSKeysByPurpose MLSPrivateKeys),
    federationProtocols :: Maybe [ProtocolTag],
    legalholdDefaults :: FeatureDefaults LegalholdConfig,
    maxConvSize :: Word16,
    listClientsUsingBrig :: Bool
  }
  deriving (Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema ConversationSubsystemConfig)

instance ToSchema ConversationSubsystemConfig where
  schema =
    object "ConversationSubsystemConfig" $
      ConversationSubsystemConfig
        <$> (.mlsKeys) .= maybe_ (optField "mls_keys" schema)
        <*> (.federationProtocols) .= maybe_ (optField "federation_protocols" (array schema))
        <*> (.legalholdDefaults) .= field "legalhold_defaults" schema
        <*> (.maxConvSize) .= field "max_conv_size" schema
        <*> (.listClientsUsingBrig) .= field "listClientsUsingBrig" schema
