module Galley.API where

import Brig.Types.Team.LegalHold
import Data.Aeson (encode)
import Data.ByteString.Conversion (fromByteString, fromList)
import Data.Id (ConvId, OpaqueUserId)
import qualified Data.Predicate as P
import Data.Range
import qualified Data.Set as Set
import Data.Swagger.Build.Api hiding (Response, def, min)
import Data.Text.Encoding (decodeLatin1)
import qualified Galley.API.Clients as Clients
import qualified Galley.API.Create as Create
import qualified Galley.API.CustomBackend as CustomBackend
import qualified Galley.API.Error as Error
import qualified Galley.API.Internal as Internal
import qualified Galley.API.LegalHold as LegalHold
import qualified Galley.API.Query as Query
import Galley.API.Swagger (swagger)
import qualified Galley.API.Teams as Teams
import qualified Galley.API.Update as Update
import Galley.App
import Galley.Types
import Galley.Types.Bot (AddBot, RemoveBot)
import Galley.Types.Bot.Service
import Galley.Types.Conversations.Roles
import qualified Galley.Types.Swagger as Model
import Galley.Types.Teams
import Galley.Types.Teams.Intra
import Galley.Types.Teams.SSO
import qualified Galley.Types.Teams.Swagger as TeamsModel
import Imports hiding (head)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate
import qualified Network.Wai.Predicate as P
import Network.Wai.Predicate.Request (HasQuery)
import Network.Wai.Routing hiding (route)
import Network.Wai.Utilities
import Network.Wai.Utilities.Swagger
import Network.Wai.Utilities.ZAuth
import Wire.Swagger (int32Between)

sitemap :: Routes ApiBuilder Galley ()
sitemap = do
  post "/teams" (continue Teams.createNonBindingTeamH) $
    zauthUserId
      .&. zauthConnId
      .&. jsonRequest @NonBindingNewTeam
      .&. accept "application" "json"
  document "POST" "createNonBindingTeam" $ do
    summary "Create a new non binding team"
    body (ref TeamsModel.newNonBindingTeam) $
      description "JSON body"
    response 201 "Team ID as `Location` header value" end
    errorResponse Error.notConnected
  put "/teams/:tid" (continue Teams.updateTeamH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "tid"
      .&. jsonRequest @TeamUpdateData
      .&. accept "application" "json"
  document "PUT" "updateTeam" $ do
    summary "Update team properties"
    parameter Path "tid" bytes' $
      description "Team ID"
    body (ref TeamsModel.update) $
      description "JSON body"
    errorResponse Error.notATeamMember
    errorResponse (Error.operationDenied SetTeamData)
  --

  get "/teams" (continue Teams.getManyTeamsH) $
    zauthUserId
      .&. opt (query "ids" ||| query "start")
      .&. def (unsafeRange 100) (query "size")
      .&. accept "application" "json"
  document "GET" "getManyTeams" $ do
    summary "Get teams"
    returns (ref TeamsModel.teamList)
    response 200 "Teams list" end
  --

  get "/teams/:tid" (continue Teams.getTeamH) $
    zauthUserId
      .&. capture "tid"
      .&. accept "application" "json"
  document "GET" "getTeam" $ do
    summary "Get a team by ID"
    parameter Path "tid" bytes' $
      description "Team ID"
    returns (ref TeamsModel.team)
    response 200 "Team data" end
    errorResponse Error.teamNotFound
  --

  delete "/teams/:tid" (continue Teams.deleteTeamH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "tid"
      .&. optionalJsonRequest @TeamDeleteData
      .&. accept "application" "json"
  document "DELETE" "deleteTeam" $ do
    summary "Delete a team"
    parameter Path "tid" bytes' $
      description "Team ID"
    body (ref TeamsModel.teamDelete) $ do
      optional
      description "JSON body, required only for binding teams."
    response 202 "Team is scheduled for removal" end
    errorResponse Error.notATeamMember
    errorResponse (Error.operationDenied DeleteTeam)
    errorResponse Error.deleteQueueFull
    errorResponse Error.reAuthFailed
    errorResponse Error.teamNotFound
  --

  get "/teams/:tid/conversations/roles" (continue Teams.getTeamConversationRolesH) $
    zauthUserId
      .&. capture "tid"
      .&. accept "application" "json"
  document "GET" "getTeamConversationsRoles" $ do
    summary "Get existing roles available for the given team"
    parameter Path "tid" bytes' $
      description "Team ID"
    returns (ref Model.conversationRolesList)
    response 200 "Team conversations roles list" end
    errorResponse Error.teamNotFound
    errorResponse Error.notATeamMember
  --

  get "/teams/:tid/members" (continue Teams.getTeamMembersH) $
    zauthUserId
      .&. capture "tid"
      .&. def (unsafeRange hardTruncationLimit) (query "maxResults")
      .&. accept "application" "json"
  document "GET" "getTeamMembers" $ do
    summary "Get team members"
    parameter Path "tid" bytes' $
      description "Team ID"
    parameter Query "maxResults" (int32Between 1 hardTruncationLimit) $
      description "Maximum Results to be returned"
    returns (ref TeamsModel.teamMemberList)
    response 200 "Team members" end
    errorResponse Error.notATeamMember
  --

  get "/teams/:tid/members/:uid" (continue Teams.getTeamMemberH) $
    zauthUserId
      .&. capture "tid"
      .&. capture "uid"
      .&. accept "application" "json"
  document "GET" "getTeamMember" $ do
    summary "Get single team member"
    parameter Path "tid" bytes' $
      description "Team ID"
    parameter Path "uid" bytes' $
      description "User ID"
    returns (ref TeamsModel.teamMember)
    response 200 "Team member" end
    errorResponse Error.notATeamMember
    errorResponse Error.teamMemberNotFound
  --

  post "/teams/:tid/members" (continue Teams.addTeamMemberH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "tid"
      .&. jsonRequest @NewTeamMember
      .&. accept "application" "json"
  document "POST" "addTeamMember" $ do
    summary "Add a new team member"
    parameter Path "tid" bytes' $
      description "Team ID"
    body (ref TeamsModel.newTeamMember) $
      description "JSON body"
    errorResponse Error.notATeamMember
    errorResponse (Error.operationDenied AddTeamMember)
    errorResponse Error.notConnected
    errorResponse Error.invalidPermissions
    errorResponse Error.tooManyTeamMembers
  --

  delete "/teams/:tid/members/:uid" (continue Teams.deleteTeamMemberH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "tid"
      .&. capture "uid"
      .&. optionalJsonRequest @TeamMemberDeleteData
      .&. accept "application" "json"
  document "DELETE" "deleteTeamMember" $ do
    summary "Remove an existing team member"
    parameter Path "tid" bytes' $
      description "Team ID"
    parameter Path "uid" bytes' $
      description "User ID"
    body (ref TeamsModel.teamMemberDelete) $ do
      optional
      description "JSON body, required only for binding teams."
    response 202 "Team member scheduled for deletion" end
    errorResponse Error.notATeamMember
    errorResponse (Error.operationDenied RemoveTeamMember)
    errorResponse Error.reAuthFailed
  --

  put "/teams/:tid/members" (continue Teams.updateTeamMemberH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "tid"
      .&. jsonRequest @NewTeamMember
      .&. accept "application" "json"
  document "PUT" "updateTeamMember" $ do
    summary "Update an existing team member"
    parameter Path "tid" bytes' $
      description "Team ID"
    body (ref TeamsModel.newTeamMember) $
      description "JSON body"
    errorResponse Error.notATeamMember
    errorResponse Error.teamMemberNotFound
    errorResponse (Error.operationDenied SetMemberPermissions)
  --

  get "/teams/:tid/conversations" (continue Teams.getTeamConversationsH) $
    zauthUserId
      .&. capture "tid"
      .&. accept "application" "json"
  document "GET" "getTeamConversations" $ do
    summary "Get team conversations"
    parameter Path "tid" bytes' $
      description "Team ID"
    returns (ref TeamsModel.teamConversationList)
    response 200 "Team conversations" end
    errorResponse Error.teamNotFound
    errorResponse (Error.operationDenied GetTeamConversations)
  --

  get "/teams/:tid/conversations/:cid" (continue Teams.getTeamConversationH) $
    zauthUserId
      .&. capture "tid"
      .&. capture "cid"
      .&. accept "application" "json"
  document "GET" "getTeamConversation" $ do
    summary "Get one team conversation"
    parameter Path "tid" bytes' $
      description "Team ID"
    parameter Path "cid" bytes' $
      description "Conversation ID"
    returns (ref TeamsModel.teamConversation)
    response 200 "Team conversation" end
    errorResponse Error.teamNotFound
    errorResponse Error.convNotFound
    errorResponse (Error.operationDenied GetTeamConversations)
  --

  delete "/teams/:tid/conversations/:cid" (continue Teams.deleteTeamConversationH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "tid"
      .&. capture "cid"
      .&. accept "application" "json"
  document "DELETE" "deleteTeamConversation" $ do
    summary "Remove a team conversation"
    parameter Path "tid" bytes' $
      description "Team ID"
    parameter Path "cid" bytes' $
      description "Conversation ID"
    errorResponse Error.notATeamMember
    errorResponse (Error.actionDenied DeleteConversation)
  --

  -- i added servant-based swagger docs here because (a) it was faster to write than
  -- learning our legacy approach and (b) swagger2 is more useful for the client teams.  we
  -- can discuss at the end of the sprint whether to keep it here, move it elsewhere, or
  -- abandon it entirely.
  get "/teams/api-docs" (continue . const . pure . json $ swagger) $
    accept "application" "json"
  post "/teams/:tid/legalhold/settings" (continue LegalHold.createSettingsH) $
    zauthUserId
      .&. capture "tid"
      .&. jsonRequest @NewLegalHoldService
      .&. accept "application" "json"
  get "/teams/:tid/legalhold/settings" (continue LegalHold.getSettingsH) $
    zauthUserId
      .&. capture "tid"
      .&. accept "application" "json"
  -- This endpoint can lead to the following events being sent to clients:
  -- - ClientRemoved event to members with a legalhold client (via brig)
  -- - UserLegalHoldDisabled event to contacts of members with a legalhold client (via brig)
  delete "/teams/:tid/legalhold/settings" (continue LegalHold.removeSettingsH) $
    zauthUserId
      .&. capture "tid"
      .&. jsonRequest @RemoveLegalHoldSettingsRequest
      .&. accept "application" "json"
  get "/teams/:tid/legalhold/:uid" (continue LegalHold.getUserStatusH) $
    zauthUserId
      .&. capture "tid"
      .&. capture "uid"
      .&. accept "application" "json"
  -- This endpoint can lead to the following events being sent to clients:
  -- - LegalHoldClientRequested event to contacts of the user, if they didn't already have a
  --   legalhold client (via brig)
  post "/teams/:tid/legalhold/:uid" (continue LegalHold.requestDeviceH) $
    zauthUserId
      .&. capture "tid"
      .&. capture "uid"
      .&. accept "application" "json"
  -- This endpoint can lead to the following events being sent to clients:
  -- - ClientRemoved event to the user (via brig)
  -- - UserLegalHoldDisabled event to contacts of the user (via brig)
  delete "/teams/:tid/legalhold/:uid" (continue LegalHold.disableForUserH) $
    zauthUserId
      .&. capture "tid"
      .&. capture "uid"
      .&. jsonRequest @DisableLegalHoldForUserRequest
      .&. accept "application" "json"
  -- This endpoint can lead to the following events being sent to clients:
  -- - ClientAdded event to the user (via brig)
  -- - UserLegalHoldEnabled event to contacts of the user (via brig)
  -- - ClientRemoved event to the user, if removing old clients due to max number (via brig)
  put "/teams/:tid/legalhold/:uid/approve" (continue LegalHold.approveDeviceH) $
    zauthUserId
      .&. capture "tid"
      .&. capture "uid"
      .&. zauthConnId
      .&. jsonRequest @ApproveLegalHoldForUserRequest
      .&. accept "application" "json"
  ---

  get "/bot/conversation" (continue Query.getBotConversationH) $
    zauth ZAuthBot
      .&> zauthBotId
      .&. zauthConvId
      .&. accept "application" "json"
  -- This endpoint can lead to the following events being sent to clients:
  -- - OtrMessageAdd event to recipients
  post "/bot/messages" (continue Update.postBotMessageH) $
    zauth ZAuthBot
      .&> zauthBotId
      .&. zauthConvId
      .&. def OtrReportAllMissing filterMissing
      .&. jsonRequest @NewOtrMessage
      .&. accept "application" "json"
  --

  get "/conversations/:cnv" (continue Query.getConversationH) $
    zauthUserId
      .&. capture "cnv"
      .&. accept "application" "json"
  document "GET" "conversation" $ do
    summary "Get a conversation by ID"
    returns (ref Model.conversation)
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    errorResponse Error.convNotFound
    errorResponse Error.convAccessDenied
  --

  get "/conversations/:cnv/roles" (continue Query.getConversationRolesH) $
    zauthUserId
      .&. capture "cnv"
      .&. accept "application" "json"
  document "GET" "getConversationsRoles" $ do
    summary "Get existing roles available for the given conversation"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    returns (ref Model.conversationRolesList)
    response 200 "Conversations roles list" end
    errorResponse Error.convNotFound
  ---

  get "/conversations/ids" (continue Query.getConversationIdsH) $
    zauthUserId
      .&. opt (query "start")
      .&. def (unsafeRange 1000) (query "size")
      .&. accept "application" "json"
  document "GET" "conversationIds" $ do
    summary "Get all conversation IDs"
    notes "At most 1000 IDs are returned per request"
    parameter Query "start" string' $ do
      optional
      description "Conversation ID to start from (exclusive)"
    parameter Query "size" string' $ do
      optional
      description "Max. number of IDs to return"
    returns (ref Model.conversationIds)
  ---

  get "/conversations" (continue Query.getConversationsH) $
    zauthUserId
      .&. opt (query "ids" ||| query "start")
      .&. def (unsafeRange 100) (query "size")
      .&. accept "application" "json"
  document "GET" "conversations" $ do
    summary "Get all conversations"
    notes "At most 500 conversations are returned per request"
    returns (ref Model.conversations)
    parameter Query "ids" (array string') $ do
      optional
      description "Mutually exclusive with 'start'. At most 32 IDs per request."
    parameter Query "start" string' $ do
      optional
      description
        "Conversation ID to start from (exclusive). \
        \Mutually exclusive with 'ids'."
    parameter Query "size" int32' $ do
      optional
      description "Max. number of conversations to return"
  ---

  -- This endpoint can lead to the following events being sent to clients:
  -- - ConvCreate event to members
  post "/conversations" (continue Create.createGroupConversationH) $
    zauthUserId
      .&. zauthConnId
      .&. jsonRequest @NewConvUnmanaged
  document "POST" "createGroupConversation" $ do
    summary "Create a new conversation"
    notes "On 201, the conversation ID is the `Location` header"
    body (ref Model.newConversation) $
      description "JSON body"
    response 201 "Conversation created" end
    errorResponse Error.notConnected
    errorResponse Error.notATeamMember
    errorResponse (Error.operationDenied CreateConversation)
  ---

  post "/conversations/self" (continue Create.createSelfConversationH) $
    zauthUserId
  document "POST" "createSelfConversation" $ do
    summary "Create a self-conversation"
    notes "On 201, the conversation ID is the `Location` header"
    response 201 "Conversation created" end
  ---

  -- This endpoint can lead to the following events being sent to clients:
  -- - ConvCreate event to members
  post "/conversations/one2one" (continue Create.createOne2OneConversationH) $
    zauthUserId
      .&. zauthConnId
      .&. jsonRequest @NewConvUnmanaged
  document "POST" "createOne2OneConversation" $ do
    summary "Create a 1:1-conversation"
    notes "On 201, the conversation ID is the `Location` header"
    body (ref Model.newConversation) $
      description "JSON body"
    response 201 "Conversation created" end
    errorResponse Error.noManagedTeamConv
  ---

  -- This endpoint can lead to the following events being sent to clients:
  -- - ConvRename event to members
  put "/conversations/:cnv/name" (continue Update.updateConversationNameH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. jsonRequest @ConversationRename
  document "PUT" "updateConversationName" $ do
    summary "Update conversation name"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    body (ref Model.conversationUpdateName) $
      description "JSON body"
    returns (ref Model.event)
    errorResponse Error.convNotFound
  ---

  -- This endpoint can lead to the following events being sent to clients:
  -- - ConvRename event to members
  put "/conversations/:cnv" (continue Update.updateConversationDeprecatedH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. jsonRequest @ConversationRename
  document "PUT" "updateConversationName" $ do
    summary "DEPRECATED! Please use updateConversationName instead!"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    body (ref Model.conversationUpdateName) $
      description "JSON body"
    returns (ref Model.event)
    errorResponse Error.convNotFound
  ---

  -- This endpoint can lead to the following events being sent to clients:
  -- - MemberJoin event to members
  post "/conversations/:cnv/join" (continue Update.joinConversationByIdH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. accept "application" "json"
  document "POST" "joinConversationById" $ do
    summary "Join a conversation by its ID (if link access enabled)"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    returns (ref Model.event)
    response 200 "Conversation joined." end
    errorResponse Error.convNotFound
  ---

  post "/conversations/code-check" (continue Update.checkReusableCodeH) $
    jsonRequest @ConversationCode
  document "POST" "checkConversationCode" $ do
    summary "Check validity of a conversation code"
    response 200 "Valid" end
    body (ref Model.conversationCode) $
      description "JSON body"
    errorResponse Error.codeNotFound
  -- This endpoint can lead to the following events being sent to clients:
  -- - MemberJoin event to members
  post "/conversations/join" (continue Update.joinConversationByReusableCodeH) $
    zauthUserId
      .&. zauthConnId
      .&. jsonRequest @ConversationCode
  document "POST" "joinConversationByCode" $ do
    summary "Join a conversation using a reusable code"
    returns (ref Model.event)
    response 200 "Conversation joined." end
    body (ref Model.conversationCode) $
      description "JSON body"
    errorResponse Error.codeNotFound
    errorResponse Error.convNotFound
    errorResponse Error.tooManyMembers
  ---

  -- This endpoint can lead to the following events being sent to clients:
  -- - ConvCodeUpdate event to members, if code didn't exist before
  post "/conversations/:cnv/code" (continue Update.addCodeH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
  document "POST" "createConversationCode" $ do
    summary "Create or recreate a conversation code"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    returns (ref Model.event)
    returns (ref Model.conversationCode)
    response 201 "Conversation code created." (model Model.event)
    response 200 "Conversation code already exists." (model Model.conversationCode)
    errorResponse Error.convNotFound
    errorResponse Error.invalidAccessOp
  ---

  -- This endpoint can lead to the following events being sent to clients:
  -- - ConvCodeDelete event to members
  delete "/conversations/:cnv/code" (continue Update.rmCodeH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
  document "DELETE" "deleteConversationCode" $ do
    summary "Delete conversation code"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    returns (ref Model.event)
    response 200 "Conversation code deleted." end
    errorResponse Error.convNotFound
    errorResponse Error.invalidAccessOp
  ---

  get "/conversations/:cnv/code" (continue Update.getCodeH) $
    zauthUserId
      .&. capture "cnv"
  document "GET" "getConversationCode" $ do
    summary "Get existing conversation code"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    returns (ref Model.conversationCode)
    response 200 "Conversation Code" end
    errorResponse Error.convNotFound
    errorResponse Error.invalidAccessOp
  ---

  -- This endpoint can lead to the following events being sent to clients:
  -- - MemberLeave event to members, if members get removed
  -- - ConvAccessUpdate event to members
  put "/conversations/:cnv/access" (continue Update.updateConversationAccessH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. jsonRequest @ConversationAccessUpdate
  document "PUT" "updateConversationAccess" $ do
    summary "Update access modes for a conversation"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    returns (ref Model.event)
    response 200 "Conversation access updated." end
    response 204 "Conversation access unchanged." end
    body (ref Model.conversationAccessUpdate) $
      description "JSON body"
    errorResponse Error.convNotFound
    errorResponse Error.convAccessDenied
    errorResponse Error.invalidTargetAccess
    errorResponse Error.invalidSelfOp
    errorResponse Error.invalidOne2OneOp
    errorResponse Error.invalidConnectOp
  ---

  -- This endpoint can lead to the following events being sent to clients:
  -- - ConvReceiptModeUpdate event to members
  put "/conversations/:cnv/receipt-mode" (continue Update.updateConversationReceiptModeH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. jsonRequest @ConversationReceiptModeUpdate
      .&. accept "application" "json"
  document "PUT" "updateConversationReceiptMode" $ do
    summary "Update receipts mode for a conversation"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    returns (ref Model.event)
    response 200 "Conversation receipt mode updated." end
    response 204 "Conversation receipt mode unchanged." end
    body (ref Model.conversationReceiptModeUpdate) $
      description "JSON body"
    errorResponse Error.convNotFound
    errorResponse Error.convAccessDenied
  ---

  -- This endpoint can lead to the following events being sent to clients:
  -- - ConvMessageTimerUpdate event to members
  put "/conversations/:cnv/message-timer" (continue Update.updateConversationMessageTimerH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. jsonRequest @ConversationMessageTimerUpdate
  document "PUT" "updateConversationMessageTimer" $ do
    summary "Update the message timer for a conversation"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    returns (ref Model.event)
    response 200 "Message timer updated." end
    response 204 "Message timer unchanged." end
    body (ref Model.conversationMessageTimerUpdate) $
      description "JSON body"
    errorResponse Error.convNotFound
    errorResponse Error.convAccessDenied
    errorResponse Error.invalidSelfOp
    errorResponse Error.invalidOne2OneOp
    errorResponse Error.invalidConnectOp
  ---

  -- This endpoint can lead to the following events being sent to clients:
  -- - MemberJoin event to members
  post "/conversations/:cnv/members" (continue Update.addMembersH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. jsonRequest @Invite
  document "POST" "addMembers" $ do
    summary "Add users to an existing conversation"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    body (ref Model.invite) $
      description "JSON body"
    returns (ref Model.event)
    response 200 "Members added" end
    response 204 "No change" end
    errorResponse Error.convNotFound
    errorResponse (Error.invalidOp "Conversation type does not allow adding members")
    errorResponse Error.notConnected
    errorResponse Error.convAccessDenied
  ---

  get "/conversations/:cnv/self" (continue Query.getSelfH) $
    zauthUserId
      .&. capture "cnv"
  document "GET" "getSelf" $ do
    summary "Get self membership properties"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    returns (ref Model.member)
    errorResponse Error.convNotFound
  ---

  -- This endpoint can lead to the following events being sent to clients:
  -- - MemberStateUpdate event to self
  put "/conversations/:cnv/self" (continue Update.updateSelfMemberH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. jsonRequest @MemberUpdate
  document "PUT" "updateSelf" $ do
    summary "Update self membership properties"
    notes "Even though all fields are optional, at least one needs to be given."
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    body (ref Model.memberUpdate) $
      description "JSON body"
    errorResponse Error.convNotFound
  ---

  -- This endpoint can lead to the following events being sent to clients:
  -- - MemberStateUpdate event to members
  put "/conversations/:cnv/members/:usr" (continue Update.updateOtherMemberH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. capture "usr"
      .&. jsonRequest @OtherMemberUpdate
  document "PUT" "updateOtherMember" $ do
    summary "Update membership of the specified user"
    notes "Even though all fields are optional, at least one needs to be given."
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    parameter Path "usr" bytes' $
      description "Target User ID"
    body (ref Model.otherMemberUpdate) $
      description "JSON body"
    errorResponse Error.convNotFound
    errorResponse Error.convMemberNotFound
    errorResponse Error.invalidTargetUserOp
  ---

  -- This endpoint can lead to the following events being sent to clients:
  -- - Typing event to members
  post "/conversations/:cnv/typing" (continue Update.isTypingH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. jsonRequest @TypingData
  document "POST" "isTyping" $ do
    summary "Sending typing notifications"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    body (ref Model.typing) $
      description "JSON body"
    errorResponse Error.convNotFound
  ---

  -- This endpoint can lead to the following events being sent to clients:
  -- - MemberLeave event to members
  delete "/conversations/:cnv/members/:usr" (continue Update.removeMemberH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. capture "usr"
  document "DELETE" "removeMember" $ do
    summary "Remove member from conversation"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    parameter Path "usr" bytes' $
      description "Target User ID"
    returns (ref Model.event)
    response 200 "Member removed" end
    response 204 "No change" end
    errorResponse Error.convNotFound
    errorResponse $ Error.invalidOp "Conversation type does not allow removing members"
  ---

  -- This endpoint can lead to the following events being sent to clients:
  -- - OtrMessageAdd event to recipients
  post "/broadcast/otr/messages" (continue Update.postOtrBroadcastH) $
    zauthUserId
      .&. zauthConnId
      .&. def OtrReportAllMissing filterMissing
      .&. jsonRequest @NewOtrMessage
  document "POST" "postOtrBroadcast" $ do
    summary "Broadcast an encrypted message to all team members and all contacts (accepts JSON)"
    parameter Query "ignore_missing" bool' $ do
      description "Force message delivery even when clients are missing."
      optional
    body (ref Model.newOtrMessage) $
      description "JSON body"
    returns (ref Model.clientMismatch)
    response 201 "Message posted" end
    response 412 "Missing clients" end
    errorResponse Error.teamNotFound
    errorResponse Error.nonBindingTeam
  ---

  -- This endpoint can lead to the following events being sent to clients:
  -- - OtrMessageAdd event to recipients
  post "/broadcast/otr/messages" (continue Update.postProtoOtrBroadcastH) $
    zauthUserId
      .&. zauthConnId
      .&. def OtrReportAllMissing filterMissing
      .&. request
      .&. contentType "application" "x-protobuf"
  document "POST" "postOtrBroadcast" $ do
    summary "Broadcast an encrypted message to all team members and all contacts (accepts Protobuf)"
    parameter Query "ignore_missing" bool' $ do
      description
        "Force message delivery even when clients are missing. \
        \NOTE: can also be a comma-separated list of user IDs, \
        \in which case it specifies who exactly is allowed to \
        \have missing clients."
      optional
    parameter Query "report_missing" bool' $ do
      description
        "Don't allow message delivery when clients are missing \
        \('ignore_missing' takes precedence when present). \
        \NOTE: can also be a comma-separated list of user IDs, \
        \in which case it specifies who exactly is forbidden from \
        \having missing clients."
      optional
    body (ref Model.newOtrMessage) $
      description "Protobuf body"
    returns (ref Model.clientMismatch)
    response 201 "Message posted" end
    response 412 "Missing clients" end
    errorResponse Error.teamNotFound
    errorResponse Error.nonBindingTeam
  ---

  -- This endpoint can lead to the following events being sent to clients:
  -- - OtrMessageAdd event to recipients
  post "/conversations/:cnv/otr/messages" (continue Update.postOtrMessageH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. def OtrReportAllMissing filterMissing
      .&. jsonRequest @NewOtrMessage
  document "POST" "postOtrMessage" $ do
    summary "Post an encrypted message to a conversation (accepts JSON)"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    parameter Query "ignore_missing" bool' $ do
      description
        "Force message delivery even when clients are missing. \
        \NOTE: can also be a comma-separated list of user IDs, \
        \in which case it specifies who exactly is allowed to \
        \have missing clients."
      optional
    parameter Query "report_missing" bool' $ do
      description
        "Don't allow message delivery when clients are missing \
        \('ignore_missing' takes precedence when present). \
        \NOTE: can also be a comma-separated list of user IDs, \
        \in which case it specifies who exactly is forbidden from \
        \having missing clients."
      optional
    body (ref Model.newOtrMessage) $
      description "JSON body"
    returns (ref Model.clientMismatch)
    response 201 "Message posted" end
    response 412 "Missing clients" end
    errorResponse Error.convNotFound
  ---

  -- This endpoint can lead to the following events being sent to clients:
  -- - OtrMessageAdd event to recipients
  post "/conversations/:cnv/otr/messages" (continue Update.postProtoOtrMessageH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. def OtrReportAllMissing filterMissing
      .&. request
      .&. contentType "application" "x-protobuf"
  document "POST" "postProtoOtrMessage" $ do
    summary "Post an encrypted message to a conversation (accepts Protobuf)"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    parameter Query "ignore_missing" bool' $ do
      description "Force message delivery even when clients are missing."
      optional
    body (ref Model.newOtrMessage) $
      description "Protobuf body"
    returns (ref Model.clientMismatch)
    response 201 "Message posted" end
    response 403 "Unknown sending client" end
    response 412 "Missing clients" end
    errorResponse Error.convNotFound
  ---

  get "/conversations/api-docs" (continue docs) $
    accept "application" "json"
      .&. query "base_url"
  --- team feature flags (public)

  get "/teams/:tid/features/legalhold" (continue Teams.getLegalholdStatusH) $
    zauthUserId
      .&. capture "tid"
      .&. accept "application" "json"
  document "GET" "getLegalholdStatus" $ do
    summary "Shows whether the LegalHold feature is enabled for team"
    parameter Path "tid" bytes' $
      description "Team ID"
    returns (ref Model.legalHoldTeamConfig)
    response 200 "LegalHold status" end
  get "/teams/:tid/features/sso" (continue Teams.getSSOStatusH) $
    zauthUserId
      .&. capture "tid"
      .&. accept "application" "json"
  document "GET" "getSSOStatus" $ do
    summary "Shows whether SSO feature is enabled for team"
    parameter Path "tid" bytes' $
      description "Team ID"
    returns (ref Model.ssoTeamConfig)
    response 200 "SSO status" end
  get "/custom-backend/by-domain/:domain" (continue CustomBackend.getCustomBackendByDomainH) $
    capture "domain"
      .&. accept "application" "json"
  document "GET" "getCustomBackendByDomain" $ do
    summary "Shows information about custom backends related to a given email domain"
    parameter Path "domain" string' $
      description "URL-encoded email domain"
    returns (ref Model.customBackend)
    response 200 "Custom backend" end
  -- internal

  put "/i/conversations/:cnv/channel" (continue $ const (return empty)) $
    zauthUserId
      .&. (capture "cnv" :: HasCaptures r => Predicate r P.Error ConvId)
      .&. request
  head "/i/status" (continue $ const (return empty)) true
  get "/i/status" (continue $ const (return empty)) true
  get "/i/conversations/:cnv/members/:usr" (continue Query.internalGetMemberH) $
    capture "cnv"
      .&. capture "usr"
  -- This endpoint can lead to the following events being sent to clients:
  -- - ConvCreate event to members
  post "/i/conversations/managed" (continue Create.internalCreateManagedConversationH) $
    zauthUserId
      .&. zauthConnId
      .&. jsonRequest @NewConvManaged
  -- This endpoint can lead to the following events being sent to clients:
  -- - ConvCreate event to self, if conversation did not exist before
  -- - ConvConnect event to self, if other didn't join the connect conversation before
  post "/i/conversations/connect" (continue Create.createConnectConversationH) $
    zauthUserId
      .&. opt zauthConnId
      .&. jsonRequest @Connect
  -- This endpoint can lead to the following events being sent to clients:
  -- - MemberJoin event to you, if the conversation existed and had < 2 members before
  -- - MemberJoin event to other, if the conversation existed and only the other was member
  --   before
  put "/i/conversations/:cnv/accept/v2" (continue Update.acceptConvH) $
    zauthUserId
      .&. opt zauthConnId
      .&. capture "cnv"
  put "/i/conversations/:cnv/block" (continue Update.blockConvH) $
    zauthUserId
      .&. capture "cnv"
  -- This endpoint can lead to the following events being sent to clients:
  -- - MemberJoin event to you, if the conversation existed and had < 2 members before
  -- - MemberJoin event to other, if the conversation existed and only the other was member
  --   before
  put "/i/conversations/:cnv/unblock" (continue Update.unblockConvH) $
    zauthUserId
      .&. opt zauthConnId
      .&. capture "cnv"
  get "/i/conversations/:cnv/meta" (continue Query.getConversationMetaH) $
    capture "cnv"
  get "/i/teams/:tid" (continue Teams.getTeamInternalH) $
    capture "tid"
      .&. accept "application" "json"
  get "/i/teams/:tid/name" (continue Teams.getTeamNameInternalH) $
    capture "tid"
      .&. accept "application" "json"
  put "/i/teams/:tid" (continue Teams.createBindingTeamH) $
    zauthUserId
      .&. capture "tid"
      .&. jsonRequest @BindingNewTeam
      .&. accept "application" "json"
  put "/i/teams/:tid/status" (continue Teams.updateTeamStatusH) $
    capture "tid"
      .&. jsonRequest @TeamStatusUpdate
      .&. accept "application" "json"
  post "/i/teams/:tid/members" (continue Teams.uncheckedAddTeamMemberH) $
    capture "tid"
      .&. jsonRequest @NewTeamMember
      .&. accept "application" "json"
  get "/i/teams/:tid/members" (continue Teams.uncheckedGetTeamMembersH) $
    capture "tid"
      .&. def (unsafeRange hardTruncationLimit) (query "maxResults")
      .&. accept "application" "json"
  get "/i/teams/:tid/members/:uid" (continue Teams.uncheckedGetTeamMemberH) $
    capture "tid"
      .&. capture "uid"
      .&. accept "application" "json"
  get "/i/users/:uid/team/members" (continue Teams.getBindingTeamMembersH) $
    capture "uid"
  get "/i/users/:uid/team" (continue Teams.getBindingTeamIdH) $
    capture "uid"
  get "/i/teams/:tid/truncated-size/:size" (continue Teams.getTruncatedTeamSizeH) $
    capture "tid"
      .&. capture "size"
      .&. accept "application" "json"
  -- Start of team features (internal); enabling this should only be
  -- possible internally. Viewing the status should be allowed
  -- for any admin

  get "/i/teams/:tid/features/legalhold" (continue Teams.getLegalholdStatusInternalH) $
    capture "tid"
      .&. accept "application" "json"
  put "/i/teams/:tid/features/legalhold" (continue Teams.setLegalholdStatusInternalH) $
    capture "tid"
      .&. jsonRequest @LegalHoldTeamConfig
      .&. accept "application" "json"
  get "/i/teams/:tid/features/sso" (continue Teams.getSSOStatusInternalH) $
    capture "tid"
      .&. accept "application" "json"
  put "/i/teams/:tid/features/sso" (continue Teams.setSSOStatusInternalH) $
    capture "tid"
      .&. jsonRequest @SSOTeamConfig
      .&. accept "application" "json"
  -- End of team features

  get "/i/test/clients" (continue Clients.getClientsH) $
    zauthUserId
  -- eg. https://github.com/wireapp/wire-server/blob/3bdca5fc8154e324773802a0deb46d884bd09143/services/brig/test/integration/API/User/Client.hs#L319

  post "/i/clients/:client" (continue Clients.addClientH) $
    zauthUserId
      .&. capture "client"
  delete "/i/clients/:client" (continue Clients.rmClientH) $
    zauthUserId
      .&. capture "client"
  -- This endpoint can lead to the following events being sent to clients:
  -- - MemberLeave event to members for all conversations the user was in
  delete "/i/user" (continue Internal.rmUserH) $
    zauthUserId .&. opt zauthConnId
  post "/i/services" (continue Update.addServiceH) $
    jsonRequest @Service
  delete "/i/services" (continue Update.rmServiceH) $
    jsonRequest @ServiceRef
  -- This endpoint can lead to the following events being sent to clients:
  -- - MemberJoin event to members
  post "/i/bots" (continue Update.addBotH) $
    zauthUserId
      .&. zauthConnId
      .&. jsonRequest @AddBot
  -- This endpoint can lead to the following events being sent to clients:
  -- - MemberLeave event to members
  delete "/i/bots" (continue Update.rmBotH) $
    zauthUserId
      .&. opt zauthConnId
      .&. jsonRequest @RemoveBot
  put "/i/custom-backend/by-domain/:domain" (continue CustomBackend.internalPutCustomBackendByDomainH) $
    capture "domain"
      .&. jsonRequest @CustomBackend
  delete "/i/custom-backend/by-domain/:domain" (continue CustomBackend.internalDeleteCustomBackendByDomainH) $
    capture "domain"
      .&. accept "application" "json"

type JSON = Media "application" "json"

docs :: JSON ::: ByteString -> Galley Response
docs (_ ::: url) = do
  let models = Model.galleyModels ++ TeamsModel.teamsModels
  let apidoc = encode $ mkSwaggerApi (decodeLatin1 url) models sitemap
  pure $ responseLBS status200 [jsonContent] apidoc

filterMissing :: HasQuery r => Predicate r P.Error OtrFilterMissing
filterMissing = (>>= go) <$> (query "ignore_missing" ||| query "report_missing")
  where
    go (Left ign) = case fromByteString ign of
      Just True -> return OtrIgnoreAllMissing
      Just False -> return OtrReportAllMissing
      Nothing -> OtrIgnoreMissing <$> users "ignore_missing" ign
    go (Right rep) = case fromByteString rep of
      Just True -> return OtrReportAllMissing
      Just False -> return OtrIgnoreAllMissing
      Nothing -> OtrReportMissing <$> users "report_missing" rep
    users :: ByteString -> ByteString -> P.Result P.Error (Set OpaqueUserId)
    users src bs = case fromByteString bs of
      Nothing ->
        P.Fail $ P.setMessage "Boolean or list of user IDs expected."
          $ P.setReason P.TypeError
          $ P.setSource src
          $ P.err status400
      -- NB. 'fromByteString' parses a comma-separated list ('List') of
      -- user IDs, and then 'fromList' unwraps it; took me a while to
      -- understand this
      Just l -> P.Okay 0 (Set.fromList (fromList l))
