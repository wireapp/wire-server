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

module Wire.API.MLS.GlobalTeamConversation where

import Control.Lens ((?~))
import Data.Aeson (FromJSON, ToJSON)
import Data.Id
import Data.Qualified
import Data.Schema
import qualified Data.Swagger as S
import Imports
import Wire.API.Conversation
import Wire.API.Conversation.Protocol
import Wire.Arbitrary (Arbitrary (..), GenericUniform (..))

-- | Public-facing global team conversation.
-- Membership is implicit. Every member of a team is part of it.
-- Protocol is also implicit: it's always MLS.
data GlobalTeamConversation = GlobalTeamConversation
  { gtcId :: Qualified ConvId,
    gtcMetadata :: GlobalTeamConversationMetadata,
    gtcMlsMetadata :: ConversationMLSData
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GlobalTeamConversation)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema GlobalTeamConversation

instance ToSchema GlobalTeamConversation where
  schema =
    objectWithDocModifier
      "GlobalTeamConversation"
      (description ?~ "The global team conversation object as returned from the server")
      $ GlobalTeamConversation
        <$> gtcId .= field "qualified_id" schema
        <*> gtcMetadata .= gtcMetadataSchema
        <*> gtcMlsMetadata .= mlsDataSchema

data GlobalTeamConversationMetadata = GlobalTeamConversationMetadata
  { gtcmCreator :: Maybe UserId,
    gtcmAccess :: [Access],
    gtcmName :: Text,
    gtcmTeam :: TeamId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GlobalTeamConversationMetadata)

gtcMetadataSchema :: ObjectSchema SwaggerDoc GlobalTeamConversationMetadata
gtcMetadataSchema =
  GlobalTeamConversationMetadata
    <$> gtcmCreator
      .= maybe_
        ( optFieldWithDocModifier
            "creator"
            (description ?~ "The creator's user ID")
            schema
        )
    <*> gtcmAccess .= field "access" (array schema)
    <*> gtcmName .= field "name" schema
    <*> gtcmTeam .= field "team" schema
