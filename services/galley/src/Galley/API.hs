module Galley.API where

import Imports hiding (head)
import Brig.Types.Team.LegalHold
import Control.Lens hiding (enum)
import Data.Aeson (encode)
import Data.ByteString.Conversion (fromByteString, fromList)
import Data.Id (UserId, ConvId)
import Data.Metrics.Middleware as Metrics
import Data.Range
import Data.Swagger.Build.Api hiding (def, min, Response)
import Data.Text.Encoding (decodeLatin1)
import Galley.App
import Galley.API.Clients
import Galley.API.Create
import Galley.API.Update
import Galley.API.Teams
import Galley.API.Query
import Galley.API.Swagger (swagger)
import Galley.Types
import Galley.Types.Teams
import Galley.Types.Teams.Intra
import Galley.Types.Teams.SSO
import Galley.Types.Bot.Service
import Galley.Types.Bot (AddBot, RemoveBot)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate
import Network.Wai.Predicate.Request (HasQuery)
import Network.Wai.Routing hiding (route)
import Network.Wai.Utilities
import Network.Wai.Utilities.ZAuth
import Network.Wai.Utilities.Swagger

import qualified Data.Predicate                as P
import qualified Data.Set                      as Set
import qualified Galley.API.Error              as Error
import qualified Galley.API.Internal           as Internal
import qualified Galley.API.LegalHold          as LegalHold
import qualified Galley.API.Teams              as Teams
import qualified Galley.Types.Swagger          as Model
import qualified Galley.Types.Teams.Swagger    as TeamsModel
import qualified Network.Wai.Predicate         as P

sitemap :: Routes ApiBuilder Galley ()
sitemap = do
    post "/teams" (continue createNonBindingTeam) $
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

    put "/teams/:tid" (continue updateTeam) $
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
        errorResponse Error.noTeamMember
        errorResponse (Error.operationDenied SetTeamData)

    --

    get "/teams" (continue getManyTeams) $
        zauthUserId
        .&. opt (query "ids" ||| query "start")
        .&. def (unsafeRange 100) (query "size")
        .&. accept "application" "json"

    document "GET" "getManyTeams" $ do
        summary "Get teams"
        returns (ref TeamsModel.teamList)
        response 200 "Teams list" end

    --

    get "/teams/:tid" (continue getTeam) $
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

    delete "/teams/:tid" (continue deleteTeam) $
        zauthUserId
        .&. zauthConnId
        .&. capture "tid"
        .&. request
        .&. opt (contentType "application" "json")
        .&. accept "application" "json"

    document "DELETE" "deleteTeam" $ do
        summary "Delete a team"
        parameter Path "tid" bytes' $
            description "Team ID"
        body (ref TeamsModel.teamDelete) $ do
            optional
            description "JSON body, required only for binding teams."
        response 202 "Team is scheduled for removal" end
        errorResponse Error.noTeamMember
        errorResponse (Error.operationDenied DeleteTeam)
        errorResponse Error.deleteQueueFull
        errorResponse Error.reAuthFailed
        errorResponse Error.teamNotFound

    --

    get "/teams/:tid/conversations/roles" (continue getTeamConversationRoles) $
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
        errorResponse Error.noTeamMember

    --

    get "/teams/:tid/members" (continue getTeamMembers) $
        zauthUserId
        .&. capture "tid"
        .&. accept "application" "json"

    document "GET" "getTeamMembers" $ do
        summary "Get team members"
        parameter Path "tid" bytes' $
            description "Team ID"
        returns (ref TeamsModel.teamMemberList)
        response 200 "Team members" end
        errorResponse Error.noTeamMember

    --

    get "/teams/:tid/members/:uid" (continue getTeamMember) $
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
        errorResponse Error.noTeamMember
        errorResponse Error.teamMemberNotFound

    --

    post "/teams/:tid/members" (continue addTeamMember) $
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
        errorResponse Error.noTeamMember
        errorResponse (Error.operationDenied AddTeamMember)
        errorResponse Error.notConnected
        errorResponse Error.invalidPermissions
        errorResponse Error.tooManyTeamMembers

    --

    delete "/teams/:tid/members/:uid" (continue deleteTeamMember) $
        zauthUserId
        .&. zauthConnId
        .&. capture "tid"
        .&. capture "uid"
        .&. request
        .&. opt (contentType "application" "json")
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
        errorResponse Error.noTeamMember
        errorResponse (Error.operationDenied RemoveTeamMember)
        errorResponse Error.reAuthFailed

    --

    put "/teams/:tid/members" (continue updateTeamMember) $
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
        errorResponse Error.noTeamMember
        errorResponse Error.teamMemberNotFound
        errorResponse (Error.operationDenied SetMemberPermissions)

    --

    get "/teams/:tid/conversations" (continue getTeamConversations) $
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

    get "/teams/:tid/conversations/:cid" (continue getTeamConversation) $
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

    delete "/teams/:tid/conversations/:cid" (continue deleteTeamConversation) $
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
        errorResponse Error.noTeamMember
        errorResponse (Error.operationDenied DeleteConversation)

   --

    -- i added servant-based swagger docs here because (a) it was faster to write than
    -- learning our legacy approach and (b) swagger2 is more useful for the client teams.  we
    -- can discuss at the end of the sprint whether to keep it here, move it elsewhere, or
    -- abandon it entirely.
    get "/teams/api-docs" (continue . const . pure . json $ swagger) $
        accept "application" "json"

    post "/teams/:tid/legalhold/settings" (continue LegalHold.createSettings) $
        zauthUserId
        .&. capture "tid"
        .&. jsonRequest @NewLegalHoldService
        .&. accept "application" "json"

    get "/teams/:tid/legalhold/settings" (continue LegalHold.getSettings) $
        zauthUserId
        .&. capture "tid"
        .&. accept "application" "json"

    delete "/teams/:tid/legalhold/settings" (continue LegalHold.removeSettings) $
        zauthUserId
        .&. capture "tid"
        .&. jsonRequest @RemoveLegalHoldSettingsRequest
        .&. accept "application" "json"

    get "/teams/:tid/legalhold/:uid" (continue LegalHold.getUserStatus) $
        zauthUserId
        .&. capture "tid"
        .&. capture "uid"
        .&. accept "application" "json"

    post "/teams/:tid/legalhold/:uid" (continue LegalHold.requestDevice) $
        zauthUserId
        .&. capture "tid"
        .&. capture "uid"
        .&. accept "application" "json"

    delete "/teams/:tid/legalhold/:uid" (continue LegalHold.disableForUser) $
        zauthUserId
        .&. capture "tid"
        .&. capture "uid"
        .&. jsonRequest @DisableLegalHoldForUserRequest
        .&. accept "application" "json"

    put "/teams/:tid/legalhold/:uid/approve" (continue LegalHold.approveDevice) $
        zauthUserId
        .&. capture "tid"
        .&. capture "uid"
        .&. zauthConnId
        .&. jsonRequest @ApproveLegalHoldForUserRequest
        .&. accept "application" "json"

   ---

    get "/bot/conversation" (continue getBotConversation) $
        zauth ZAuthBot
        .&> zauthBotId
        .&. zauthConvId
        .&. accept "application" "json"

    post "/bot/messages" (continue postBotMessage) $
        zauth ZAuthBot
        .&> zauthBotId
        .&. zauthConvId
        .&. def OtrReportAllMissing filterMissing
        .&. jsonRequest @NewOtrMessage
        .&. accept "application" "json"

    --

    get "/conversations/:cnv" (continue getConversation) $
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

    get "/conversations/:cnv/roles" (continue getConversationRoles) $
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

    get "/conversations/ids" (continue getConversationIds) $
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

    get "/conversations" (continue getConversations) $
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
            description "Conversation ID to start from (exclusive). \
                        \Mutually exclusive with 'ids'."
        parameter Query "size" int32' $ do
            optional
            description "Max. number of conversations to return"

    ---

    post "/conversations" (continue createGroupConversation) $
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
        errorResponse Error.noTeamMember
        errorResponse (Error.operationDenied CreateConversation)

    ---

    post "/conversations/self" (continue createSelfConversation)
        zauthUserId

    document "POST" "createSelfConversation" $ do
        summary "Create a self-conversation"
        notes "On 201, the conversation ID is the `Location` header"
        response 201 "Conversation created" end

    ---

    post "/conversations/one2one" (continue createOne2OneConversation) $
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

    -- TODO: We should IMHO deprecate this in favor of
    --       `put "/conversations/:cnv/name"`. The event is called
    --       "conversation.rename", adding anything to this endpoint
    --       would be problematic anyways. Mixing "conversation rename"
    --       and "conversation metadata" should be avoided.
    put "/conversations/:cnv" (continue updateConversationName) $
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

    post "/conversations/:cnv/join" (continue joinConversationById) $
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

    post "/conversations/code-check" (continue checkReusableCode) $
        jsonRequest @ConversationCode

    document "POST" "checkConversationCode" $ do
        summary "Check validity of a conversation code"
        response 200 "Valid" end
        body (ref Model.conversationCode) $
            description "JSON body"
        errorResponse Error.codeNotFound


    post "/conversations/join" (continue joinConversationByReusableCode) $
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

    post "/conversations/:cnv/code" (continue addCode) $
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

    delete "/conversations/:cnv/code" (continue rmCode) $
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

    get "/conversations/:cnv/code" (continue getCode) $
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

    put "/conversations/:cnv/access" (continue updateConversationAccess) $
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

    put "/conversations/:cnv/receipt-mode" (continue updateConversationReceiptMode) $
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

    put "/conversations/:cnv/message-timer" (continue updateConversationMessageTimer) $
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

    post "/conversations/:cnv/members" (continue addMembers) $
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

    get "/conversations/:cnv/self" (continue getMember) $
        zauthUserId
        .&. capture "cnv"

    document "GET" "getSelf" $ do
        summary "Get self membership properties"
        parameter Path "cnv" bytes' $
            description "Conversation ID"
        returns (ref Model.member)
        errorResponse Error.convNotFound

    ---

    put "/conversations/:cnv/self" (continue updateMember) $
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

    post "/conversations/:cnv/typing" (continue isTyping) $
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

    delete "/conversations/:cnv/members/:usr" (continue removeMember) $
        zauthUserId
        .&. zauthConnId
        .&. capture "cnv"
        .&. capture "usr"

    document "DELETE" "removeMember" $ do
        summary "Remove member from conversation"
        parameter Path "cnv" bytes' $
            description "Conversation ID"
        parameter Path "usr" bytes' $
            description "User ID"
        returns (ref Model.event)
        response 200 "Member removed" end
        response 204 "No change" end
        errorResponse Error.convNotFound
        errorResponse $ Error.invalidOp "Conversation type does not allow removing members"

    ---

    post "/broadcast/otr/messages" (continue postOtrBroadcast) $
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

    post "/broadcast/otr/messages" (continue postProtoOtrBroadcast) $
        zauthUserId
        .&. zauthConnId
        .&. def OtrReportAllMissing filterMissing
        .&. request
        .&. contentType "application" "x-protobuf"

    document "POST" "postOtrBroadcast" $ do
        summary "Broadcast an encrypted message to all team members and all contacts (accepts Protobuf)"
        parameter Query "ignore_missing" bool' $ do
            description "Force message delivery even when clients are missing. \
                        \NOTE: can also be a comma-separated list of user IDs, \
                        \in which case it specifies who exactly is allowed to \
                        \have missing clients."
            optional
        parameter Query "report_missing" bool' $ do
            description "Don't allow message delivery when clients are missing \
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

    post "/conversations/:cnv/otr/messages" (continue postOtrMessage) $
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
            description "Force message delivery even when clients are missing. \
                        \NOTE: can also be a comma-separated list of user IDs, \
                        \in which case it specifies who exactly is allowed to \
                        \have missing clients."
            optional
        parameter Query "report_missing" bool' $ do
            description "Don't allow message delivery when clients are missing \
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

    post "/conversations/:cnv/otr/messages" (continue postProtoOtrMessage) $
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

    get "/teams/:tid/features/legalhold" (continue Teams.getLegalholdStatus) $
        zauthUserId
        .&. capture "tid"
        .&. accept "application" "json"

    document "GET" "getLegalholdStatus" $ do
        summary "Shows whether the LegalHold feature is enabled for team"
        parameter Path "tid" bytes' $
            description "Team ID"
        returns (ref Model.legalHoldTeamConfig)
        response 200 "LegalHold status" end

    get "/teams/:tid/features/sso" (continue Teams.getSSOStatus) $
        zauthUserId
        .&. capture "tid"
        .&. accept "application" "json"

    document "GET" "getSSOStatus" $ do
        summary "Shows whether SSO feature is enabled for team"
        parameter Path "tid" bytes' $
            description "Team ID"
        returns (ref Model.ssoTeamConfig)
        response 200 "SSO status" end

    -- internal

    put "/i/conversations/:cnv/channel" (continue $ const (return empty)) $
       zauthUserId
       .&. (capture "cnv" :: HasCaptures r => Predicate r P.Error ConvId)
       .&. request

    head "/i/status" (continue $ const (return empty)) true

    get "/i/status" (continue $ const (return empty)) true

    get "/i/monitoring" (continue monitoring) $
        accept "application" "json"

    get "/i/conversations/:cnv/members/:usr" (continue internalGetMember) $
        capture "cnv"
        .&. capture "usr"

    post "/i/conversations/managed" (continue internalCreateManagedConversation) $
        zauthUserId
        .&. zauthConnId
        .&. jsonRequest @NewConvManaged

    post "/i/conversations/connect" (continue createConnectConversation) $
        zauthUserId
        .&. opt zauthConnId
        .&. jsonRequest @Connect

    put "/i/conversations/:cnv/accept/v2" (continue acceptConv) $
        zauthUserId
        .&. opt zauthConnId
        .&. capture "cnv"

    put "/i/conversations/:cnv/block" (continue blockConv) $
        zauthUserId
        .&. capture "cnv"

    put "/i/conversations/:cnv/unblock" (continue unblockConv) $
        zauthUserId
        .&. opt zauthConnId
        .&. capture "cnv"

    get "/i/conversations/:cnv/meta" (continue getConversationMeta) $
        capture "cnv"

    get "/i/teams/:tid" (continue getTeamInternal) $
        capture "tid"
        .&. accept "application" "json"

    get "/i/teams/:tid/name" (continue getTeamNameInternal) $
        capture "tid"
        .&. accept "application" "json"

    put "/i/teams/:tid" (continue createBindingTeam) $
        zauthUserId
        .&. capture "tid"
        .&. jsonRequest @BindingNewTeam
        .&. accept "application" "json"

    put "/i/teams/:tid/status" (continue updateTeamStatus) $
        capture "tid"
        .&. jsonRequest @TeamStatusUpdate
        .&. accept "application" "json"

    post "/i/teams/:tid/members" (continue uncheckedAddTeamMember) $
        capture "tid"
        .&. jsonRequest @NewTeamMember
        .&. accept "application" "json"

    get "/i/teams/:tid/members" (continue uncheckedGetTeamMembers) $
        capture "tid"
        .&. accept "application" "json"

    get "/i/teams/:tid/members/:uid" (continue uncheckedGetTeamMember) $
        capture "tid"
        .&. capture "uid"
        .&. accept "application" "json"

    get "/i/users/:uid/team/members" (continue getBindingTeamMembers) $
        capture "uid"

    get "/i/users/:uid/team" (continue getBindingTeamId) $
        capture "uid"

    -- Start of team features (internal); enabling this should only be
    -- possible internally. Viewing the status should be allowed
    -- for any admin

    get "/i/teams/:tid/features/legalhold" (continue Teams.getLegalholdStatusInternal) $
        capture "tid"
        .&. accept "application" "json"

    put "/i/teams/:tid/features/legalhold" (continue Teams.setLegalholdStatusInternal) $
        capture "tid"
        .&. jsonRequest @LegalHoldTeamConfig
        .&. accept "application" "json"

    get "/i/teams/:tid/features/sso" (continue Teams.getSSOStatusInternal) $
        capture "tid"
        .&. accept "application" "json"

    put "/i/teams/:tid/features/sso" (continue Teams.setSSOStatusInternal) $
        capture "tid"
        .&. jsonRequest @SSOTeamConfig
        .&. accept "application" "json"

    -- End of team features

    get "/i/test/clients" (continue getClients)
        zauthUserId
        -- eg. https://github.com/wireapp/wire-server/blob/3bdca5fc8154e324773802a0deb46d884bd09143/services/brig/test/integration/API/User/Client.hs#L319

    post "/i/clients/:client" (continue addClient) $
        zauthUserId
        .&. capture "client"

    delete "/i/clients/:client" (continue rmClient) $
        zauthUserId
        .&. capture "client"

    delete "/i/user" (continue Internal.rmUser) $
        zauthUserId .&. opt zauthConnId

    post "/i/services" (continue addService) $
        jsonRequest @Service

    delete "/i/services" (continue rmService) $
        jsonRequest @ServiceRef

    post "/i/bots" (continue addBot) $
        zauthUserId
        .&. zauthConnId
        .&. jsonRequest @AddBot

    delete "/i/bots" (continue rmBot) $
        zauthUserId
        .&. opt zauthConnId
        .&. jsonRequest @RemoveBot

type JSON = Media "application" "json"

docs :: JSON ::: ByteString -> Galley Response
docs (_ ::: url) = do
    let models = Model.galleyModels ++ TeamsModel.teamsModels
    let apidoc = encode $ mkSwaggerApi (decodeLatin1 url) models sitemap
    pure $ responseLBS status200 [jsonContent] apidoc

monitoring :: JSON -> Galley Response
monitoring _ = do
    json <$> (render =<< view monitor)

filterMissing :: HasQuery r => Predicate r P.Error OtrFilterMissing
filterMissing = (>>= go) <$> (query "ignore_missing" ||| query "report_missing")
  where
    go (Left ign) = case fromByteString ign of
        Just True  -> return OtrIgnoreAllMissing
        Just False -> return OtrReportAllMissing
        Nothing    -> OtrIgnoreMissing <$> users "ignore_missing" ign

    go (Right rep) = case fromByteString rep of
        Just True  -> return OtrReportAllMissing
        Just False -> return OtrIgnoreAllMissing
        Nothing    -> OtrReportMissing <$> users "report_missing" rep

    users :: ByteString -> ByteString -> P.Result P.Error (Set UserId)
    users src bs = case fromByteString bs of
        Nothing -> P.Fail $ P.setMessage "Boolean or list of user IDs expected."
                          $ P.setReason P.TypeError
                          $ P.setSource src
                          $ P.err status400
        -- NB. 'fromByteString' parses a comma-separated list ('List') of
        -- user IDs, and then 'fromList' unwraps it; took me a while to
        -- understand this
        Just  l -> P.Okay 0 (Set.fromList (fromList l))
