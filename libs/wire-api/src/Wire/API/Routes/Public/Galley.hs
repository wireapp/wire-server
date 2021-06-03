{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Routes.Public.Galley where

import Data.CommaSeparatedList
import Data.Id (ConvId, TeamId, UserId)
import Data.Domain
import Data.Range
import qualified Data.Set as Set
import qualified Data.Swagger as Swagger
import Imports hiding (head)
import Servant hiding (Handler, JSON, addHeader, contentType, respond)
import qualified Servant
import Servant.API.Generic (ToServantApi, (:-))
import Servant.Swagger.Internal
import Servant.Swagger.Internal.Orphans ()
import qualified Wire.API.Conversation as Public
import qualified Wire.API.Conversation.Role as Public
import Wire.API.ErrorDescription (ErrorDescription (ErrorDescription))
import qualified Wire.API.Event.Conversation as Public
import qualified Wire.API.Message as Public
import Wire.API.Routes.Public (EmptyResult, ZConn, ZUser)
import qualified Wire.API.Team.Conversation as Public

type ConversationResponses =
  '[ WithStatus 200 (Headers '[Servant.Header "Location" ConvId] Public.Conversation),
     WithStatus 201 (Headers '[Servant.Header "Location" ConvId] Public.Conversation)
   ]

type UpdateResponses =
  '[ WithStatus 200 Public.Event,
     NoContent
   ]

type ConversationNotFound = ErrorDescription 404 "Conversation not found"

convNotFound :: ConversationNotFound
convNotFound = ErrorDescription "no-conversation" "conversation not found"

type UnknownClient = ErrorDescription 403 "Unknown Client"

unknownClient :: UnknownClient
unknownClient = ErrorDescription "unknown-client" "Sending client not known"

type PostOtrResponses =
  '[ WithStatus 201 Public.ClientMismatch,
     WithStatus 412 Public.ClientMismatch,
     ConversationNotFound,
     UnknownClient
   ]

-- FUTUREWORK: Make a PR to the servant-swagger package with this instance
instance Swagger.ToSchema Servant.NoContent where
  declareNamedSchema _ = Swagger.declareNamedSchema (Proxy @())

data Api routes = Api
  { -- Conversations

    getUnqualifiedConversation ::
      routes
        :- Summary "Get a conversation by ID"
        :> ZUser
        :> "conversations"
        :> Capture "cnv" ConvId
        :> Get '[Servant.JSON] Public.Conversation,
    getConversation ::
      routes
        :- Summary "Get a conversation by ID"
        :> ZUser
        :> "conversations"
        :> Capture "domain" Domain
        :> Capture "cnv" ConvId
        :> Get '[Servant.JSON] Public.Conversation,
    getConversationRoles ::
      routes
        :- Summary "Get existing roles available for the given conversation"
        :> ZUser
        :> "conversations"
        :> Capture "cnv" ConvId
        :> "roles"
        :> Get '[Servant.JSON] Public.ConversationRolesList,
    getConversationIds ::
      routes
        :- Summary "Get all conversation IDs."
        -- FUTUREWORK: add bounds to swagger schema for Range
        :> ZUser
        :> "conversations"
        :> "ids"
        :> QueryParam'
             [ Optional,
               Strict,
               Description "Conversation ID to start from (exclusive)"
             ]
             "start"
             ConvId
        :> QueryParam'
             [ Optional,
               Strict,
               Description "Maximum number of IDs to return"
             ]
             "size"
             (Range 1 1000 Int32)
        :> Get '[Servant.JSON] (Public.ConversationList ConvId),
    getConversations ::
      routes
        :- Summary "Get all conversations"
        :> ZUser
        :> "conversations"
        :> QueryParam'
             [ Optional,
               Strict,
               Description "Mutually exclusive with 'start' (at most 32 IDs per request)"
             ]
             "ids"
             (Range 1 32 (CommaSeparatedList ConvId))
        :> QueryParam'
             [ Optional,
               Strict,
               Description "Conversation ID to start from (exclusive)"
             ]
             "start"
             ConvId
        :> QueryParam'
             [ Optional,
               Strict,
               Description "Maximum number of conversations to return"
             ]
             "size"
             (Range 1 500 Int32)
        :> Get '[Servant.JSON] (Public.ConversationList Public.Conversation),
    -- This endpoint can lead to the following events being sent:
    -- - ConvCreate event to members
    -- FUTUREWORK: errorResponse Error.notConnected
    --             errorResponse Error.notATeamMember
    --             errorResponse (Error.operationDenied Public.CreateConversation)
    createGroupConversation ::
      routes
        :- Summary "Create a new conversation"
        :> Description "This returns 201 when a new conversation is created, and 200 when the conversation already existed"
        :> ZUser
        :> ZConn
        :> "conversations"
        :> ReqBody '[Servant.JSON] Public.NewConvUnmanaged
        :> UVerb 'POST '[Servant.JSON] ConversationResponses,
    createSelfConversation ::
      routes
        :- Summary "Create a self-conversation"
        :> ZUser
        :> "conversations"
        :> "self"
        :> UVerb 'POST '[Servant.JSON] ConversationResponses,
    -- This endpoint can lead to the following events being sent:
    -- - ConvCreate event to members
    -- TODO: add note: "On 201, the conversation ID is the `Location` header"
    createOne2OneConversation ::
      routes
        :- Summary "Create a 1:1 conversation"
        :> ZUser
        :> ZConn
        :> "conversations"
        :> "one2one"
        :> ReqBody '[Servant.JSON] Public.NewConvUnmanaged
        :> UVerb 'POST '[Servant.JSON] ConversationResponses,
    addMembersToConversationV2 ::
      routes
        :- Summary "Add qualified members to an existing conversation: WIP, events not propagated yet."
        :> ZUser
        :> ZConn
        :> "conversations"
        :> Capture "cnv" ConvId
        :> "members"
        :> "v2"
        :> ReqBody '[Servant.JSON] Public.InviteQualified
        :> UVerb 'POST '[Servant.JSON] UpdateResponses,
    -- Team Conversations

    getTeamConversationRoles ::
      -- FUTUREWORK: errorResponse Error.notATeamMember
      routes
        :- Summary "Get existing roles available for the given team"
        :> ZUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "conversations"
        :> "roles"
        :> Get '[Servant.JSON] Public.ConversationRolesList,
    -- FUTUREWORK: errorResponse (Error.operationDenied Public.GetTeamConversations)
    getTeamConversations ::
      routes
        :- Summary "Get team conversations"
        :> ZUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "conversations"
        :> Get '[Servant.JSON] Public.TeamConversationList,
    -- FUTUREWORK: errorResponse (Error.operationDenied Public.GetTeamConversations)
    getTeamConversation ::
      routes
        :- Summary "Get one team conversation"
        :> ZUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "conversations"
        :> Capture "cid" ConvId
        :> Get '[Servant.JSON] Public.TeamConversation,
    -- FUTUREWORK: errorResponse (Error.actionDenied Public.DeleteConversation)
    --             errorResponse Error.notATeamMember
    deleteTeamConversation ::
      routes
        :- Summary "Remove a team conversation"
        :> ZUser
        :> ZConn
        :> "teams"
        :> Capture "tid" TeamId
        :> "conversations"
        :> Capture "cid" ConvId
        :> Delete '[] (EmptyResult 200),
    -- | This endpoint can lead to the following events being sent:
    --
    -- - OtrMessageAdd event to recipients
    --
    -- TODO: Add 404 for conv not found
    -- TODO: Add 403 for unknown sending client
    postOtrMessage ::
      routes
        :- Summary "Post an encrypted message to a conversation (accepts JSON)"
        :> ZUser
        :> ZConn
        :> "conversations"
        :> Capture "cnv" ConvId
        :> QueryParam "ignore_missing" IgnoreMissing
        :> QueryParam "report_missing" ReportMissing
        :> "otr"
        :> "messages"
        :> ReqBody '[Servant.JSON] Public.NewOtrMessage
        :> UVerb 'POST '[Servant.JSON] PostOtrResponses
  }
  deriving (Generic)

type ServantAPI = ToServantApi Api

data IgnoreMissing
  = IgnoreMissingAll
  | IgnoreMissingList (Set UserId)
  deriving (Show, Eq)

-- TODO: Fill this in
instance Swagger.ToParamSchema IgnoreMissing where
  toParamSchema _ = mempty

instance FromHttpApiData IgnoreMissing where
  parseQueryParam = \case
    "true" -> Right IgnoreMissingAll
    "false" -> Right $ IgnoreMissingList mempty
    list -> IgnoreMissingList . Set.fromList . fromCommaSeparatedList <$> parseQueryParam list

data ReportMissing
  = ReportMissingAll
  | ReportMissingList (Set UserId)

instance Swagger.ToParamSchema ReportMissing where
  toParamSchema _ = mempty

instance FromHttpApiData ReportMissing where
  parseQueryParam = \case
    "true" -> Right ReportMissingAll
    "false" -> Right $ ReportMissingList mempty
    list -> ReportMissingList . Set.fromList . fromCommaSeparatedList <$> parseQueryParam list

swaggerDoc :: Swagger.Swagger
swaggerDoc = toSwagger (Proxy @ServantAPI)
