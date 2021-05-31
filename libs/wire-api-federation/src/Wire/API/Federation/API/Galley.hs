-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Federation.API.Galley where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Id (ConvId, UserId)
import Data.Qualified (Qualified)
import Imports
import Servant.API (JSON, Post, ReqBody, (:>))
import Servant.API.Generic ((:-))
import Servant.Client.Generic (AsClientT, genericClient)
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))
import Wire.API.Conversation (Conversation)
import Wire.API.Federation.Client (FederationClientError, FederatorClient)
import qualified Wire.API.Federation.GRPC.Types as Proto
import Wire.API.Federation.Util.Aeson (CustomEncoded (CustomEncoded))

-- FUTUREWORK: data types, json instances, more endpoints. See
-- https://wearezeta.atlassian.net/wiki/spaces/CORE/pages/356090113/Federation+Galley+Conversation+API
-- for the current list we need.

data Api routes = Api
  { getConversations ::
      routes
        :- "federation"
        :> "get-conversations"
        :> ReqBody '[JSON] GetConversationsRequest
        :> Post '[JSON] GetConversationsResponse,
    -- used by backend that owns the conversation to inform the backend about
    -- add/removal of its users to the conversation
    updateConversationMemberships ::
      routes
        :- "federation"
        :> "update-conversation-memberships"
        :> ReqBody '[JSON] ConversationMemberUpdate
        :> Post '[JSON] ()
  }
  deriving (Generic)

data GetConversationsRequest = GetConversationsRequest
  { gcrUserId :: Qualified UserId,
    gcrConvIds :: [ConvId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetConversationsRequest)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetConversationsRequest)

newtype GetConversationsResponse = GetConversationsResponse
  { gcresConvs :: [Conversation]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetConversationsResponse)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetConversationsResponse)

data ConversationMemberUpdate = ConversationMemberUpdate
  { cmuConvId :: Qualified ConvId,
    cmuUsersAdd :: [UserId],
    cmuUsersRemove :: [UserId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationMemberUpdate)
  deriving (ToJSON, FromJSON) via (CustomEncoded ConversationMemberUpdate)

clientRoutes :: (MonadError FederationClientError m, MonadIO m) => Api (AsClientT (FederatorClient 'Proto.Galley m))
clientRoutes = genericClient
