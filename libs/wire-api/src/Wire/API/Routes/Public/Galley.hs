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

import qualified Data.Code as Code
import Data.CommaSeparatedList
import Data.Id (ConvId, TeamId)
import Data.Qualified (Qualified (..))
import Data.Range
import Data.SOP (I (..), NS (..))
import qualified Data.Swagger as Swagger
import GHC.TypeLits (AppendSymbol)
import Imports hiding (head)
import Servant
import Servant.API.Generic (ToServantApi, (:-))
import Servant.Swagger.Internal
import Servant.Swagger.Internal.Orphans ()
import Wire.API.Conversation as Public
import qualified Wire.API.Conversation.Role as Public
import Wire.API.ErrorDescription
import qualified Wire.API.Event.Conversation as Public
import Wire.API.Message
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Public (ZConn, ZUser)
import Wire.API.Routes.QualifiedCapture
import Wire.API.ServantProto (Proto, RawProto)
import qualified Wire.API.Team.Conversation as Public
import Wire.API.Team.Feature

instance AsHeaders '[Header "Location" ConvId] Conversation Conversation where
  -- FUTUREWORK: use addHeader
  toHeaders c = Headers c (HCons (Header (qUnqualified (Public.cnvQualifiedId c))) HNil)
  fromHeaders = getResponse

instance
  (ResponseType r1 ~ a, ResponseType r2 ~ a) =>
  AsUnion '[r1, r2] (ConversationResponseFor a)
  where
  toUnion (ConversationExisted x) = Z (I x)
  toUnion (ConversationCreated x) = S (Z (I x))

  fromUnion (Z (I x)) = ConversationExisted x
  fromUnion (S (Z (I x))) = ConversationCreated x
  fromUnion (S (S x)) = case x of

data ConversationResponseFor a
  = ConversationExisted !a
  | ConversationCreated !a

type ConversationResponse = ConversationResponseFor Conversation

type ConversationHeaders = '[DescHeader "Location" "Conversation ID" ConvId]

type ConversationVerb =
  MultiVerb
    'POST
    '[JSON]
    '[ WithHeaders
         ConversationHeaders
         Conversation
         (Respond 200 "Conversation existed" Conversation),
       WithHeaders
         ConversationHeaders
         Conversation
         (Respond 201 "Conversation created" Conversation)
     ]
    ConversationResponse

type UpdateResponses =
  '[ RespondEmpty 204 "Conversation unchanged",
     Respond 200 "Conversation updated" Public.Event
   ]

data UpdateResult
  = Unchanged
  | Updated Public.Event

instance AsUnion UpdateResponses UpdateResult where
  toUnion Unchanged = inject (I ())
  toUnion (Updated e) = inject (I e)

  fromUnion (Z (I ())) = Unchanged
  fromUnion (S (Z (I e))) = Updated e
  fromUnion (S (S x)) = case x of

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
        :> QualifiedCapture "cnv" ConvId
        :> Get '[Servant.JSON] Public.Conversation,
    getConversationRoles ::
      routes
        :- Summary "Get existing roles available for the given conversation"
        :> ZUser
        :> "conversations"
        :> Capture "cnv" ConvId
        :> "roles"
        :> Get '[Servant.JSON] Public.ConversationRolesList,
    listConversationIdsUnqualified ::
      routes
        :- Summary "[deprecated] Get all local conversation IDs."
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
    listConversationIds ::
      routes
        :- Summary "Get all conversation IDs."
          :> Description "To retrieve the next page, a client must pass the paging_state returned by the previous page."
          :> ZUser
          :> "conversations"
          :> "list-ids"
          :> ReqBody '[Servant.JSON] Public.GetPaginatedConversationIds
          :> Post '[Servant.JSON] Public.ConvIdsPage,
    getConversations ::
      routes
        :- Summary "Get all *local* conversations."
        :> Description "Will not return remote conversations (will eventually be deprecated in favour of list-conversations)"
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
    listConversations ::
      routes
        :- Summary "[deprecated] Get all conversations (also returns remote conversations)"
        :> Description "Like GET /conversations, but allows specifying a list of remote conversations in its request body. \
                       \Will return all or the requested qualified conversations, including remote ones. \
                       \Size parameter is not yet honoured for remote conversations.\n\
                       \**NOTE** This endpoint will soon be removed."
        :> ZUser
        :> "list-conversations"
        :> ReqBody '[Servant.JSON] Public.ListConversations
        :> Post '[Servant.JSON] (Public.ConversationList Public.Conversation),
    listConversationsV2 ::
      routes
        :- Summary "Get conversation metadata for a list of conversation ids"
        :> ZUser
        :> "conversations"
        :> "list"
        :> "v2"
        :> ReqBody '[Servant.JSON] Public.ListConversationsV2
        :> Post '[Servant.JSON] Public.ConversationsResponse,
    -- This endpoint can lead to the following events being sent:
    -- - ConvCreate event to members
    getConversationByReusableCode ::
      routes
        :- Summary "Get limited conversation information by key/code pair"
        :> CanThrow NotATeamMember
        :> CanThrow CodeNotFound
        :> CanThrow ConvNotFound
        :> CanThrow ConvAccessDenied
        :> ZUser
        :> "conversations"
        :> "join"
        :> QueryParam' [Required, Strict] "key" Code.Key
        :> QueryParam' [Required, Strict] "code" Code.Value
        :> Get '[Servant.JSON] Public.ConversationCoverView,
    createGroupConversation ::
      routes
        :- Summary "Create a new conversation"
        :> CanThrow NotConnected
        :> CanThrow OperationDenied
        :> CanThrow NotATeamMember
        :> Description "This returns 201 when a new conversation is created, and 200 when the conversation already existed"
        :> ZUser
        :> ZConn
        :> "conversations"
        :> ReqBody '[Servant.JSON] Public.NewConvUnmanaged
        :> ConversationVerb,
    createSelfConversation ::
      routes
        :- Summary "Create a self-conversation"
        :> ZUser
        :> "conversations"
        :> "self"
        :> ConversationVerb,
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
        :> ConversationVerb,
    addMembersToConversationV2 ::
      routes
        :- Summary "Add qualified members to an existing conversation."
        :> ZUser
        :> ZConn
        :> "conversations"
        :> Capture "cnv" ConvId
        :> "members"
        :> "v2"
        :> ReqBody '[Servant.JSON] Public.InviteQualified
        :> MultiVerb 'POST '[Servant.JSON] UpdateResponses UpdateResult,
    -- Team Conversations

    getTeamConversationRoles ::
      routes
        :- Summary "Get existing roles available for the given team"
        :> CanThrow NotATeamMember
        :> ZUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "conversations"
        :> "roles"
        :> Get '[Servant.JSON] Public.ConversationRolesList,
    getTeamConversations ::
      routes
        :- Summary "Get team conversations"
        :> CanThrow OperationDenied
        :> ZUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "conversations"
        :> Get '[Servant.JSON] Public.TeamConversationList,
    getTeamConversation ::
      routes
        :- Summary "Get one team conversation"
        :> CanThrow OperationDenied
        :> ZUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "conversations"
        :> Capture "cid" ConvId
        :> Get '[Servant.JSON] Public.TeamConversation,
    deleteTeamConversation ::
      routes
        :- Summary "Remove a team conversation"
        :> CanThrow NotATeamMember
        :> CanThrow ActionDenied
        :> ZUser
        :> ZConn
        :> "teams"
        :> Capture "tid" TeamId
        :> "conversations"
        :> Capture "cid" ConvId
        :> MultiVerb 'DELETE '[JSON] '[RespondEmpty 200 "Conversation deleted"] (),
    postOtrMessageUnqualified ::
      routes
        :- Summary "Post an encrypted message to a conversation (accepts JSON or Protobuf)"
        :> Description PostOtrDescriptionUnqualified
        :> ZUser
        :> ZConn
        :> "conversations"
        :> Capture "cnv" ConvId
        :> QueryParam "ignore_missing" IgnoreMissing
        :> QueryParam "report_missing" ReportMissing
        :> "otr"
        :> "messages"
        :> ReqBody '[Servant.JSON, Proto] NewOtrMessage
        :> MultiVerb
             'POST
             '[Servant.JSON]
             (PostOtrResponses ClientMismatch)
             (Either (MessageNotSent ClientMismatch) ClientMismatch),
    postProteusMessage ::
      routes
        :- Summary "Post an encrypted message to a conversation (accepts only Protobuf)"
        :> Description PostOtrDescription
        :> ZUser
        :> ZConn
        :> "conversations"
        :> QualifiedCapture "cnv" ConvId
        :> "proteus"
        :> "messages"
        :> ReqBody '[Proto] (RawProto QualifiedNewOtrMessage)
        :> MultiVerb
             'POST
             '[Servant.JSON]
             (PostOtrResponses MessageSendingStatus)
             (Either (MessageNotSent MessageSendingStatus) MessageSendingStatus),
    teamFeatureStatusSSOGet ::
      routes
        :- FeatureStatusGet 'TeamFeatureSSO,
    teamFeatureStatusLegalHoldGet ::
      routes
        :- FeatureStatusGet 'TeamFeatureLegalHold,
    teamFeatureStatusLegalHoldPut ::
      routes
        :- FeatureStatusPut 'TeamFeatureLegalHold,
    teamFeatureStatusSearchVisibilityGet ::
      routes
        :- FeatureStatusGet 'TeamFeatureSearchVisibility,
    teamFeatureStatusSearchVisibilityPut ::
      routes
        :- FeatureStatusPut 'TeamFeatureSearchVisibility,
    teamFeatureStatusSearchVisibilityDeprecatedGet ::
      routes
        :- FeatureStatusDeprecatedGet 'TeamFeatureSearchVisibility,
    teamFeatureStatusSearchVisibilityDeprecatedPut ::
      routes
        :- FeatureStatusDeprecatedPut 'TeamFeatureSearchVisibility,
    teamFeatureStatusValidateSAMLEmailsGet ::
      routes
        :- FeatureStatusGet 'TeamFeatureValidateSAMLEmails,
    teamFeatureStatusValidateSAMLEmailsDeprecatedGet ::
      routes
        :- FeatureStatusDeprecatedGet 'TeamFeatureValidateSAMLEmails,
    teamFeatureStatusDigitalSignaturesGet ::
      routes
        :- FeatureStatusGet 'TeamFeatureDigitalSignatures,
    teamFeatureStatusDigitalSignaturesDeprecatedGet ::
      routes
        :- FeatureStatusDeprecatedGet 'TeamFeatureDigitalSignatures,
    teamFeatureStatusAppLockGet ::
      routes
        :- FeatureStatusGet 'TeamFeatureAppLock,
    teamFeatureStatusAppLockPut ::
      routes
        :- FeatureStatusPut 'TeamFeatureAppLock,
    teamFeatureStatusFileSharingGet ::
      routes
        :- FeatureStatusGet 'TeamFeatureFileSharing,
    teamFeatureStatusFileSharingPut ::
      routes
        :- FeatureStatusPut 'TeamFeatureFileSharing,
    teamFeatureStatusClassifiedDomainsGet ::
      routes
        :- FeatureStatusGet 'TeamFeatureClassifiedDomains,
    teamFeatureStatusConferenceCallingGet ::
      routes
        :- FeatureStatusGet 'TeamFeatureConferenceCalling,
    featureAllFeatureConfigsGet ::
      routes
        :- AllFeatureConfigsGet,
    featureConfigLegalHoldGet ::
      routes
        :- FeatureConfigGet 'TeamFeatureLegalHold,
    featureConfigSSOGet ::
      routes
        :- FeatureConfigGet 'TeamFeatureSSO,
    featureConfigSearchVisibilityGet ::
      routes
        :- FeatureConfigGet 'TeamFeatureSearchVisibility,
    featureConfigValidateSAMLEmailsGet ::
      routes
        :- FeatureConfigGet 'TeamFeatureValidateSAMLEmails,
    featureConfigDigitalSignaturesGet ::
      routes
        :- FeatureConfigGet 'TeamFeatureDigitalSignatures,
    featureConfigAppLockGet ::
      routes
        :- FeatureConfigGet 'TeamFeatureAppLock,
    featureConfigFileSharingGet ::
      routes
        :- FeatureConfigGet 'TeamFeatureFileSharing,
    featureConfigClassifiedDomainsGet ::
      routes
        :- FeatureConfigGet 'TeamFeatureClassifiedDomains,
    featureConfigConferenceCallingGet ::
      routes
        :- FeatureConfigGet 'TeamFeatureConferenceCalling
  }
  deriving (Generic)

type ServantAPI = ToServantApi Api

type FeatureStatusGet featureName =
  Summary (AppendSymbol "Get config for " (KnownTeamFeatureNameSymbol featureName))
    :> ZUser
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> KnownTeamFeatureNameSymbol featureName
    :> Get '[Servant.JSON] (TeamFeatureStatus featureName)

type FeatureStatusPut featureName =
  Summary (AppendSymbol "Put config for " (KnownTeamFeatureNameSymbol featureName))
    :> ZUser
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> KnownTeamFeatureNameSymbol featureName
    :> ReqBody '[Servant.JSON] (TeamFeatureStatus featureName)
    :> Put '[Servant.JSON] (TeamFeatureStatus featureName)

-- | A type for a GET endpoint for a feature with a deprecated path
type FeatureStatusDeprecatedGet featureName =
  Summary (AppendSymbol "[deprecated] Get config for " (KnownTeamFeatureNameSymbol featureName))
    :> ZUser
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> DeprecatedFeatureName featureName
    :> Get '[Servant.JSON] (TeamFeatureStatus featureName)

-- | A type for a PUT endpoint for a feature with a deprecated path
type FeatureStatusDeprecatedPut featureName =
  Summary (AppendSymbol "[deprecated] Get config for " (KnownTeamFeatureNameSymbol featureName))
    :> ZUser
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> DeprecatedFeatureName featureName
    :> ReqBody '[Servant.JSON] (TeamFeatureStatus featureName)
    :> Put '[Servant.JSON] (TeamFeatureStatus featureName)

type FeatureConfigGet featureName =
  Summary (AppendSymbol "Get feature config for feature " (KnownTeamFeatureNameSymbol featureName))
    :> ZUser
    :> "feature-configs"
    :> KnownTeamFeatureNameSymbol featureName
    :> Get '[Servant.JSON] (TeamFeatureStatus featureName)

type AllFeatureConfigsGet =
  Summary "Get configurations of all features"
    :> ZUser
    :> "feature-configs"
    :> Get '[Servant.JSON] AllFeatureConfigs

type PostOtrDescriptionUnqualified =
  "This endpoint ensures that the list of clients is correct and only sends the message if the list is correct.\n\
  \To override this, the endpoint accepts two query params:\n\
  \- `ignore_missing`: Can be 'true' 'false' or a comma separated list of user IDs.\n\
  \  - When 'true' all missing clients are ignored.\n\
  \  - When 'false' all missing clients are reported.\n\
  \  - When comma separated list of user-ids, only clients for listed users are ignored.\n\
  \- `report_missing`: Can be 'true' 'false' or a comma separated list of user IDs.\n\
  \  - When 'true' all missing clients are reported.\n\
  \  - When 'false' all missing clients are ignored.\n\
  \  - When comma separated list of user-ids, only clients for listed users are reported.\n\
  \\n\
  \Apart from these, the request body also accepts `report_missing` which can only be a list of user ids and behaves the same way as the query parameter.\n\
  \\n\
  \All three of these should be considered mutually exclusive. The server however does not error if more than one is specified, it reads them in this order of precedence:\n\
  \- `report_missing` in the request body has highest precedence.\n\
  \- `ignore_missing` in the query param is the next.\n\
  \- `report_missing` in the query param has the lowest precedence.\n\
  \\n\
  \This endpoint can lead to OtrMessageAdd event being sent to the recipients.\n\
  \\n\
  \**NOTE:** The protobuf definitions of the request body can be found at https://github.com/wireapp/generic-message-proto/blob/master/proto/otr.proto."

type PostOtrDescription =
  "This endpoint ensures that the list of clients is correct and only sends the message if the list is correct.\n\
  \To override this, the endpoint accepts `client_mismatch_strategy` in the body. It can have these values:\n\
  \- `report_all`: When set, the message is not sent if any clients are missing. The missing clients are reported in the response.\n\
  \- `ignore_all`: When set, no checks about missing clients are carried out.\n\
  \- `report_only`: Takes a list of qualified UserIDs. If any clients of the listed users are missing, the message is not sent. The missing clients are reported in the response.\n\
  \- `ignore_only`: Takes a list of qualified UserIDs. If any clients of the non-listed users are missing, the message is not sent. The missing clients are reported in the response.\n\
  \\n\
  \The sending of messages in a federated conversation could theorectically fail partially. \
  \To make this case unlikely, the backend first gets a list of clients from all the involved backends and then tries to send a message. \
  \So, if any backend is down, the message is not propagated to anyone. \
  \But the actual message fan out to multiple backends could still fail partially. This type of failure is reported as a 201, \
  \the clients for which the message sending failed are part of the response body.\n\
  \\n\
  \This endpoint can lead to OtrMessageAdd event being sent to the recipients.\n\
  \\n\
  \**NOTE:** The protobuf definitions of the request body can be found at https://github.com/wireapp/generic-message-proto/blob/master/proto/otr.proto."

swaggerDoc :: Swagger.Swagger
swaggerDoc = toSwagger (Proxy @ServantAPI)
