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
import Data.Domain (Domain)
import Data.Id (ConvId, UserId)
import Data.Misc (Milliseconds)
import Data.Qualified (Qualified)
import Data.Time.Clock (UTCTime)
import Imports
import Servant.API (JSON, Post, ReqBody, Summary, (:>))
import Servant.API.Generic ((:-))
import Servant.Client.Generic (AsClientT, genericClient)
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))
import Wire.API.Conversation (Access, AccessRole, ConvType, Conversation, ReceiptMode)
import Wire.API.Conversation.Member (Member (..))
import Wire.API.Conversation.Role (RoleName)
import Wire.API.Federation.Client (FederationClientFailure, FederatorClient)
import qualified Wire.API.Federation.GRPC.Types as Proto
import Wire.API.Federation.Util.Aeson (CustomEncoded (CustomEncoded))

-- FUTUREWORK: data types, json instances, more endpoints. See
-- https://wearezeta.atlassian.net/wiki/spaces/CORE/pages/356090113/Federation+Galley+Conversation+API
-- for the current list we need.

data Api routes = Api
  { -- | Create a new conversation
    createConversation ::
      routes
        :- "federation"
        :> Summary "Create a new conversation"
        :> "create-conversation"
        :> ReqBody '[JSON] RegisterConversation
        :> Post '[JSON] (),
    getConversations ::
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

-- | A record type describing a new federated conversation
--
-- FUTUREWORK: Think about extracting common conversation metadata into a
-- separarate data type that can be reused in several data types in this module.
data RegisterConversation = MkRegisterConversation
  { -- | The time when the conversation was created
    rcTime :: UTCTime,
    -- | The user that created the conversation
    rcOrigUserId :: Qualified UserId,
    -- | The qualified conversation ID
    rcCnvId :: Qualified ConvId,
    -- | The conversation type
    rcCnvType :: ConvType,
    rcCnvAccess :: [Access],
    rcCnvAccessRole :: AccessRole,
    -- | The conversation name,
    rcCnvName :: Maybe Text,
    -- | Members of the conversation grouped by their domain
    rcMembers :: Map Domain [Member],
    rcMessageTimer :: Maybe Milliseconds,
    rcReceiptMode :: Maybe ReceiptMode
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded RegisterConversation)

data ConversationMemberUpdate = ConversationMemberUpdate
  { cmuTime :: UTCTime,
    cmuOrigUserId :: Qualified UserId,
    cmuConvId :: Qualified ConvId,
    -- | A list of users from a remote backend that need to be sent
    -- notifications about this change. This is required as we do not expect a
    -- non-conversation owning backend to have an indexed mapping of
    -- conversation to users.
    cmuAlreadyPresentUsers :: [UserId],
    -- | Users that got added to the conversation.
    cmuUsersAdd :: [(Qualified UserId, RoleName)],
    -- | Users that got removed from the conversation. This should probably be
    -- Qualified, but as of now this is a stub.
    --
    -- FUTUREWORK: Implement this when supporting removal of remote conversation
    -- members.
    cmuUsersRemove :: [UserId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationMemberUpdate)
  deriving (ToJSON, FromJSON) via (CustomEncoded ConversationMemberUpdate)

clientRoutes :: (MonadError FederationClientFailure m, MonadIO m) => Api (AsClientT (FederatorClient 'Proto.Galley m))
clientRoutes = genericClient
