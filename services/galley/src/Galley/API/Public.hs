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
module Galley.API.Public
  ( sitemap,
    apiDocs,
    filterMissing, -- for tests
    servantSitemap,
  )
where

import Data.Aeson (encode)
import Data.ByteString.Conversion (fromByteString, fromList)
import Data.Id (UserId)
import qualified Data.Predicate as P
import Data.Range
import qualified Data.Set as Set
import Data.Swagger.Build.Api hiding (Response, def, min)
import qualified Data.Swagger.Build.Api as Swagger
import Data.Text.Encoding (decodeLatin1)
import qualified Galley.API.Create as Create
import qualified Galley.API.CustomBackend as CustomBackend
import qualified Galley.API.Error as Error
import qualified Galley.API.LegalHold as LegalHold
import qualified Galley.API.Query as Query
import qualified Galley.API.Teams as Teams
import Galley.API.Teams.Features (DoAuth (..), getFeatureStatus, setFeatureStatus)
import qualified Galley.API.Teams.Features as Features
import qualified Galley.API.Update as Update
import Galley.App
import Imports hiding (head)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate
import qualified Network.Wai.Predicate as P
import Network.Wai.Predicate.Request (HasQuery)
import Network.Wai.Routing hiding (route)
import Network.Wai.Utilities
import Network.Wai.Utilities.Swagger
import Network.Wai.Utilities.ZAuth hiding (ZAuthUser)
import Servant hiding (Handler, JSON, addHeader, contentType, respond)
import Servant.Server.Generic (genericServerT)
import Servant.Swagger.Internal.Orphans ()
import qualified Wire.API.Conversation as Public
import qualified Wire.API.Conversation.Code as Public
import qualified Wire.API.Conversation.Typing as Public
import qualified Wire.API.CustomBackend as Public
import qualified Wire.API.ErrorDescription as Error
import qualified Wire.API.Event.Team as Public ()
import qualified Wire.API.Message as Public
import qualified Wire.API.Notification as Public
import qualified Wire.API.Routes.Public.Galley as GalleyAPI
import qualified Wire.API.Swagger as Public.Swagger (models)
import qualified Wire.API.Team as Public
import qualified Wire.API.Team.Feature as Public
import qualified Wire.API.Team.LegalHold as Public
import qualified Wire.API.Team.Member as Public
import qualified Wire.API.Team.Permission as Public
import qualified Wire.API.Team.SearchVisibility as Public
import qualified Wire.API.User as Public (UserIdList, modelUserIdList)
import Wire.Swagger (int32Between)

servantSitemap :: ServerT GalleyAPI.ServantAPI Galley
servantSitemap =
  genericServerT $
    GalleyAPI.Api
      { GalleyAPI.getUnqualifiedConversation = Query.getUnqualifiedConversation,
        GalleyAPI.getConversation = Query.getConversation,
        GalleyAPI.getConversationRoles = Query.getConversationRoles,
        GalleyAPI.listConversationIdsUnqualified = Query.conversationIdsPageFromUnqualified,
        GalleyAPI.listConversationIds = Query.conversationIdsPageFrom,
        GalleyAPI.getConversations = Query.getConversations,
        GalleyAPI.getConversationByReusableCode = Query.getConversationByReusableCode,
        GalleyAPI.listConversations = Query.listConversations,
        GalleyAPI.listConversationsV2 = Query.listConversationsV2,
        GalleyAPI.createGroupConversation = Create.createGroupConversation,
        GalleyAPI.createSelfConversation = Create.createSelfConversation,
        GalleyAPI.createOne2OneConversation = Create.createOne2OneConversation,
        GalleyAPI.addMembersToConversationV2 = Update.addMembers,
        GalleyAPI.getTeamConversationRoles = Teams.getTeamConversationRoles,
        GalleyAPI.getTeamConversations = Teams.getTeamConversations,
        GalleyAPI.getTeamConversation = Teams.getTeamConversation,
        GalleyAPI.deleteTeamConversation = Teams.deleteTeamConversation,
        GalleyAPI.postOtrMessageUnqualified = Update.postOtrMessageUnqualified,
        GalleyAPI.postProteusMessage = Update.postProteusMessage,
        GalleyAPI.teamFeatureStatusSSOGet =
          getFeatureStatus @'Public.TeamFeatureSSO Features.getSSOStatusInternal
            . DoAuth,
        GalleyAPI.teamFeatureStatusLegalHoldGet =
          getFeatureStatus @'Public.TeamFeatureLegalHold Features.getLegalholdStatusInternal
            . DoAuth,
        GalleyAPI.teamFeatureStatusLegalHoldPut =
          setFeatureStatus @'Public.TeamFeatureLegalHold Features.setLegalholdStatusInternal . DoAuth,
        GalleyAPI.teamFeatureStatusSearchVisibilityGet =
          getFeatureStatus @'Public.TeamFeatureSearchVisibility Features.getTeamSearchVisibilityAvailableInternal
            . DoAuth,
        GalleyAPI.teamFeatureStatusSearchVisibilityPut =
          setFeatureStatus @'Public.TeamFeatureSearchVisibility Features.setTeamSearchVisibilityAvailableInternal
            . DoAuth,
        GalleyAPI.teamFeatureStatusSearchVisibilityDeprecatedGet =
          getFeatureStatus @'Public.TeamFeatureSearchVisibility Features.getTeamSearchVisibilityAvailableInternal
            . DoAuth,
        GalleyAPI.teamFeatureStatusSearchVisibilityDeprecatedPut =
          setFeatureStatus @'Public.TeamFeatureSearchVisibility Features.setTeamSearchVisibilityAvailableInternal
            . DoAuth,
        GalleyAPI.teamFeatureStatusValidateSAMLEmailsGet =
          getFeatureStatus @'Public.TeamFeatureValidateSAMLEmails Features.getValidateSAMLEmailsInternal
            . DoAuth,
        GalleyAPI.teamFeatureStatusValidateSAMLEmailsDeprecatedGet =
          getFeatureStatus @'Public.TeamFeatureValidateSAMLEmails Features.getValidateSAMLEmailsInternal
            . DoAuth,
        GalleyAPI.teamFeatureStatusDigitalSignaturesGet =
          getFeatureStatus @'Public.TeamFeatureDigitalSignatures Features.getDigitalSignaturesInternal
            . DoAuth,
        GalleyAPI.teamFeatureStatusDigitalSignaturesDeprecatedGet =
          getFeatureStatus @'Public.TeamFeatureDigitalSignatures Features.getDigitalSignaturesInternal
            . DoAuth,
        GalleyAPI.teamFeatureStatusAppLockGet =
          getFeatureStatus @'Public.TeamFeatureAppLock Features.getAppLockInternal
            . DoAuth,
        GalleyAPI.teamFeatureStatusAppLockPut =
          setFeatureStatus @'Public.TeamFeatureAppLock Features.setAppLockInternal
            . DoAuth,
        GalleyAPI.teamFeatureStatusFileSharingGet =
          getFeatureStatus @'Public.TeamFeatureFileSharing Features.getFileSharingInternal . DoAuth,
        GalleyAPI.teamFeatureStatusFileSharingPut =
          setFeatureStatus @'Public.TeamFeatureFileSharing Features.setFileSharingInternal . DoAuth,
        GalleyAPI.teamFeatureStatusClassifiedDomainsGet =
          getFeatureStatus @'Public.TeamFeatureClassifiedDomains Features.getClassifiedDomainsInternal
            . DoAuth,
        GalleyAPI.teamFeatureStatusConferenceCallingGet =
          getFeatureStatus @'Public.TeamFeatureConferenceCalling Features.getConferenceCallingInternal
            . DoAuth,
        GalleyAPI.featureAllFeatureConfigsGet = Features.getAllFeatureConfigs,
        GalleyAPI.featureConfigLegalHoldGet = Features.getFeatureConfig @'Public.TeamFeatureLegalHold Features.getLegalholdStatusInternal,
        GalleyAPI.featureConfigSSOGet = Features.getFeatureConfig @'Public.TeamFeatureSSO Features.getSSOStatusInternal,
        GalleyAPI.featureConfigSearchVisibilityGet = Features.getFeatureConfig @'Public.TeamFeatureSearchVisibility Features.getTeamSearchVisibilityAvailableInternal,
        GalleyAPI.featureConfigValidateSAMLEmailsGet = Features.getFeatureConfig @'Public.TeamFeatureValidateSAMLEmails Features.getValidateSAMLEmailsInternal,
        GalleyAPI.featureConfigDigitalSignaturesGet = Features.getFeatureConfig @'Public.TeamFeatureDigitalSignatures Features.getDigitalSignaturesInternal,
        GalleyAPI.featureConfigAppLockGet = Features.getFeatureConfig @'Public.TeamFeatureAppLock Features.getAppLockInternal,
        GalleyAPI.featureConfigFileSharingGet = Features.getFeatureConfig @'Public.TeamFeatureFileSharing Features.getFileSharingInternal,
        GalleyAPI.featureConfigClassifiedDomainsGet = Features.getFeatureConfig @'Public.TeamFeatureClassifiedDomains Features.getClassifiedDomainsInternal,
        GalleyAPI.featureConfigConferenceCallingGet = Features.getFeatureConfig @'Public.TeamFeatureConferenceCalling Features.getConferenceCallingInternal
      }

sitemap :: Routes ApiBuilder Galley ()
sitemap = do
  -- Team API -----------------------------------------------------------

  post "/teams" (continue Teams.createNonBindingTeamH) $
    zauthUserId
      .&. zauthConnId
      .&. jsonRequest @Public.NonBindingNewTeam
      .&. accept "application" "json"
  document "POST" "createNonBindingTeam" $ do
    summary "Create a new non binding team"
    body (ref Public.modelNewNonBindingTeam) $
      description "JSON body"
    response 201 "Team ID as `Location` header value" end
    errorResponse (Error.errorDescriptionToWai Error.notConnected)

  put "/teams/:tid" (continue Teams.updateTeamH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "tid"
      .&. jsonRequest @Public.TeamUpdateData
      .&. accept "application" "json"
  document "PUT" "updateTeam" $ do
    summary "Update team properties"
    parameter Path "tid" bytes' $
      description "Team ID"
    body (ref Public.modelUpdateData) $
      description "JSON body"
    errorResponse (Error.errorDescriptionToWai Error.notATeamMember)
    errorResponse (Error.errorDescriptionToWai (Error.operationDenied Public.SetTeamData))

  get "/teams" (continue Teams.getManyTeamsH) $
    zauthUserId
      .&. opt (query "ids" ||| query "start")
      .&. def (unsafeRange 100) (query "size")
      .&. accept "application" "json"
  document "GET" "getManyTeams" $ do
    parameter Query "ids" (array string') $ do
      optional
      description "At most 32 team IDs per request. Mutually exclusive with `start`."
    parameter Query "start" string' $ do
      optional
      description "Team ID to start from (exclusive). Mutually exclusive with `ids`."
    parameter Query "size" (int32Between 1 100) $ do
      optional
      description "Max. number of teams to return"
    summary "Get teams"
    returns (ref Public.modelTeamList)
    response 200 "Teams list" end

  get "/teams/:tid" (continue Teams.getTeamH) $
    zauthUserId
      .&. capture "tid"
      .&. accept "application" "json"
  document "GET" "getTeam" $ do
    summary "Get a team by ID"
    parameter Path "tid" bytes' $
      description "Team ID"
    returns (ref Public.modelTeam)
    response 200 "Team data" end
    errorResponse Error.teamNotFound

  delete "/teams/:tid" (continue Teams.deleteTeamH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "tid"
      .&. optionalJsonRequest @Public.TeamDeleteData
      .&. accept "application" "json"
  document "DELETE" "deleteTeam" $ do
    summary "Delete a team"
    parameter Path "tid" bytes' $
      description "Team ID"
    body (ref Public.modelTeamDelete) $ do
      optional
      description "JSON body, required only for binding teams."
    response 202 "Team is scheduled for removal" end
    errorResponse (Error.errorDescriptionToWai Error.notATeamMember)
    errorResponse (Error.errorDescriptionToWai (Error.operationDenied Public.DeleteTeam))
    errorResponse Error.deleteQueueFull
    errorResponse Error.reAuthFailed
    errorResponse Error.teamNotFound

  -- Team Member API -----------------------------------------------------

  get "/teams/:tid/members" (continue Teams.getTeamMembersH) $
    zauthUserId
      .&. capture "tid"
      .&. def (unsafeRange Public.hardTruncationLimit) (query "maxResults")
      .&. accept "application" "json"
  document "GET" "getTeamMembers" $ do
    summary "Get team members"
    parameter Path "tid" bytes' $
      description "Team ID"
    parameter Query "maxResults" (int32Between 1 Public.hardTruncationLimit) $ do
      optional
      description "Maximum Results to be returned"
    returns (ref Public.modelTeamMemberList)
    response 200 "Team members" end
    errorResponse (Error.errorDescriptionToWai Error.notATeamMember)

  get "/teams/:tid/members/csv" (continue Teams.getTeamMembersCSVH) $
    -- we could discriminate based on accept header only, but having two paths makes building
    -- nginz metrics dashboards easier.
    zauthUserId
      .&. capture "tid"
      .&. accept "text" "csv"
  document "GET" "getTeamMembersCSV" $ do
    summary "Get all members of the team as a CSV file"
    notes
      "The endpoint returns data in chunked transfer encoding.\
      \ Internal server errors might result in a failed transfer instead of a 500 response."
    parameter Path "tid" bytes' $
      description "Team ID"
    response 200 "Team members CSV file" end
    errorResponse Error.accessDenied

  post "/teams/:tid/get-members-by-ids-using-post" (continue Teams.bulkGetTeamMembersH) $
    zauthUserId
      .&. capture "tid"
      .&. def (unsafeRange Public.hardTruncationLimit) (query "maxResults")
      .&. jsonRequest @Public.UserIdList
      .&. accept "application" "json"
  document "POST" "bulkGetTeamMembers" $ do
    summary "Get team members by user id list"
    notes "The `has_more` field in the response body is always `false`."
    parameter Path "tid" bytes' $
      description "Team ID"
    parameter Query "maxResults" (int32Between 1 Public.hardTruncationLimit) $ do
      optional
      description "Maximum Results to be returned"
    body (ref Public.modelUserIdList) $
      description "JSON body"
    returns (ref Public.modelTeamMemberList)
    response 200 "Team members" end
    errorResponse (Error.errorDescriptionToWai Error.notATeamMember)
    errorResponse Error.bulkGetMemberLimitExceeded

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
    returns (ref Public.modelTeamMember)
    response 200 "Team member" end
    errorResponse (Error.errorDescriptionToWai Error.notATeamMember)
    errorResponse Error.teamMemberNotFound

  get "/teams/notifications" (continue Teams.getTeamNotificationsH) $
    zauthUserId
      .&. opt (query "since")
      .&. def (unsafeRange 1000) (query "size")
      .&. accept "application" "json"
  document "GET" "getTeamNotifications" $ do
    summary "Read recently added team members from team queue"
    notes
      "This is a work-around for scalability issues with gundeck user event fan-out. \
      \It does not track all team-wide events, but only `member-join`.\
      \\n\
      \Note that `/teams/notifications` behaves different from `/notifications`:\
      \\n\
      \- If there is a gap between the notification id requested with `since` and the \
      \available data, team queues respond with 200 and the data that could be found. \
      \The do NOT respond with status 404, but valid data in the body.\
      \\n\
      \- The notification with the id given via `since` is included in the \
      \response if it exists.  You should remove this and only use it to decide whether \
      \there was a gap between your last request and this one.\
      \\n\
      \- If the notification id does *not* exist, you get the more recent events from the queue \
      \(instead of all of them).  This can be done because a notification id is a UUIDv1, which \
      \is essentially a time stamp.\
      \\n\
      \- There is no corresponding `/last` end-point to get only the most recent event. \
      \That end-point was only useful to avoid having to pull the entire queue.  In team \
      \queues, if you have never requested the queue before and \
      \have no prior notification id, just pull with timestamp 'now'."
    parameter Query "since" bytes' $ do
      optional
      description "Notification id to start with in the response (UUIDv1)"
    parameter Query "size" (int32 (Swagger.def 1000)) $ do
      optional
      description "Maximum number of events to return (1..10000; default: 1000)"
    returns (ref Public.modelNotificationList)
    response 200 "List of team notifications" end
    errorResponse Error.teamNotFound
    errorResponse Error.invalidTeamNotificationId

  post "/teams/:tid/members" (continue Teams.addTeamMemberH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "tid"
      .&. jsonRequest @Public.NewTeamMember
      .&. accept "application" "json"
  document "POST" "addTeamMember" $ do
    summary "Add a new team member"
    parameter Path "tid" bytes' $
      description "Team ID"
    body (ref Public.modelNewTeamMember) $
      description "JSON body"
    errorResponse (Error.errorDescriptionToWai Error.notATeamMember)
    errorResponse (Error.errorDescriptionToWai (Error.operationDenied Public.AddTeamMember))
    errorResponse (Error.errorDescriptionToWai Error.notConnected)
    errorResponse Error.invalidPermissions
    errorResponse Error.tooManyTeamMembers

  delete "/teams/:tid/members/:uid" (continue Teams.deleteTeamMemberH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "tid"
      .&. capture "uid"
      .&. optionalJsonRequest @Public.TeamMemberDeleteData
      .&. accept "application" "json"
  document "DELETE" "deleteTeamMember" $ do
    summary "Remove an existing team member"
    parameter Path "tid" bytes' $
      description "Team ID"
    parameter Path "uid" bytes' $
      description "User ID"
    body (ref Public.modelTeamMemberDelete) $ do
      optional
      description "JSON body, required only for binding teams."
    response 202 "Team member scheduled for deletion" end
    errorResponse (Error.errorDescriptionToWai Error.notATeamMember)
    errorResponse (Error.errorDescriptionToWai (Error.operationDenied Public.RemoveTeamMember))
    errorResponse Error.reAuthFailed

  put "/teams/:tid/members" (continue Teams.updateTeamMemberH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "tid"
      .&. jsonRequest @Public.NewTeamMember
      .&. accept "application" "json"
  document "PUT" "updateTeamMember" $ do
    summary "Update an existing team member"
    parameter Path "tid" bytes' $
      description "Team ID"
    body (ref Public.modelNewTeamMember) $
      description "JSON body"
    errorResponse (Error.errorDescriptionToWai Error.notATeamMember)
    errorResponse Error.teamMemberNotFound
    errorResponse (Error.errorDescriptionToWai (Error.operationDenied Public.SetMemberPermissions))

  -- Team Legalhold API -------------------------------------------------
  --
  -- The Swagger docs of this part of the documentation are not generated
  -- using wai-utilities, but with Servant.
  -- See 'apiDocsTeamsLegalhold'.

  post "/teams/:tid/legalhold/settings" (continue LegalHold.createSettingsH) $
    zauthUserId
      .&. capture "tid"
      .&. jsonRequest @Public.NewLegalHoldService
      .&. accept "application" "json"

  get "/teams/:tid/legalhold/settings" (continue LegalHold.getSettingsH) $
    zauthUserId
      .&. capture "tid"
      .&. accept "application" "json"

  -- This endpoint can lead to the following events being sent:
  -- - ClientRemoved event to members with a legalhold client (via brig)
  -- - UserLegalHoldDisabled event to contacts of members with a legalhold client (via brig)
  delete "/teams/:tid/legalhold/settings" (continue LegalHold.removeSettingsH) $
    zauthUserId
      .&. capture "tid"
      .&. jsonRequest @Public.RemoveLegalHoldSettingsRequest
      .&. accept "application" "json"

  get "/teams/:tid/legalhold/:uid" (continue LegalHold.getUserStatusH) $
    zauthUserId
      .&. capture "tid"
      .&. capture "uid"
      .&. accept "application" "json"

  -- This endpoint can lead to the following events being sent:
  -- - tbd. (currently, there are not events, but maybe there should be.)  (fisx, 2021-05-10)
  post "/teams/:tid/legalhold/consent" (continue LegalHold.grantConsentH) $
    zauthUserId
      .&. capture "tid"
      .&. accept "application" "json"

  -- This endpoint can lead to the following events being sent:
  -- - LegalHoldClientRequested event to contacts of the user the device is requested for,
  --   if they didn't already have a legalhold client (via brig)
  post "/teams/:tid/legalhold/:uid" (continue LegalHold.requestDeviceH) $
    zauthUserId
      .&. capture "tid"
      .&. capture "uid"
      .&. accept "application" "json"

  -- This endpoint can lead to the following events being sent:
  -- - ClientRemoved event to the user owning the client (via brig)
  -- - UserLegalHoldDisabled event to contacts of the user owning the client (via brig)
  delete "/teams/:tid/legalhold/:uid" (continue LegalHold.disableForUserH) $
    zauthUserId
      .&. capture "tid"
      .&. capture "uid"
      .&. jsonRequest @Public.DisableLegalHoldForUserRequest
      .&. accept "application" "json"

  -- This endpoint can lead to the following events being sent:
  -- - ClientAdded event to the user owning the client (via brig)
  -- - UserLegalHoldEnabled event to contacts of the user owning the client (via brig)
  -- - ClientRemoved event to the user, if removing old client due to max number (via brig)
  put "/teams/:tid/legalhold/:uid/approve" (continue LegalHold.approveDeviceH) $
    zauthUserId
      .&. capture "tid"
      .&. capture "uid"
      .&. zauthConnId
      .&. jsonRequest @Public.ApproveLegalHoldForUserRequest
      .&. accept "application" "json"

  get "/teams/:tid/search-visibility" (continue Teams.getSearchVisibilityH) $
    zauthUserId
      .&. capture "tid"
      .&. accept "application" "json"
  document "GET" "getSearchVisibility" $ do
    summary "Shows the value for search visibility"
    parameter Path "tid" bytes' $
      description "Team ID"
    returns (ref Public.modelTeamSearchVisibility)
    response 200 "Search visibility" end

  put "/teams/:tid/search-visibility" (continue Teams.setSearchVisibilityH) $
    zauthUserId
      .&. capture "tid"
      .&. jsonRequest @Public.TeamSearchVisibilityView
      .&. accept "application" "json"
  document "POST" "setSearchVisibility" $ do
    summary "Sets the search visibility for the whole team"
    parameter Path "tid" bytes' $
      description "Team ID"
    body (ref Public.modelTeamSearchVisibility) $
      description "Search visibility to be set"
    response 204 "Search visibility set" end
    errorResponse Error.teamSearchVisibilityNotEnabled

  get "/teams/:tid/features" (continue Features.getAllFeaturesH) $
    zauthUserId
      .&. capture "tid"
      .&. accept "application" "json"
  document "GET" "getAllFeatures" $ do
    summary "Shows the configuration status of every team feature"
    parameter Path "tid" bytes' $
      description "Team ID"
    response 200 "All feature statuses" end

  -- Custom Backend API -------------------------------------------------

  get "/custom-backend/by-domain/:domain" (continue CustomBackend.getCustomBackendByDomainH) $
    capture "domain"
      .&. accept "application" "json"
  document "GET" "getCustomBackendByDomain" $ do
    summary "Shows information about custom backends related to a given email domain"
    parameter Path "domain" string' $
      description "URL-encoded email domain"
    returns (ref Public.modelCustomBackend)
    response 200 "Custom backend" end

  -- Bot API ------------------------------------------------------------

  get "/bot/conversation" (continue Query.getBotConversationH) $
    zauth ZAuthBot
      .&> zauthBotId
      .&. zauthConvId
      .&. accept "application" "json"

  -- This endpoint can lead to the following events being sent:
  -- - OtrMessageAdd event to recipients
  post "/bot/messages" (continue Update.postBotMessageH) $
    zauth ZAuthBot
      .&> zauthBotId
      .&. zauthConvId
      .&. def Public.OtrReportAllMissing filterMissing
      .&. jsonRequest @Public.NewOtrMessage
      .&. accept "application" "json"

  -- Conversation API ---------------------------------------------------

  -- This endpoint can lead to the following events being sent:
  -- - ConvRename event to members
  put "/conversations/:cnv/name" (continue Update.updateConversationNameH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. jsonRequest @Public.ConversationRename
  document "PUT" "updateConversationName" $ do
    summary "Update conversation name"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    body (ref Public.modelConversationUpdateName) $
      description "JSON body"
    returns (ref Public.modelEvent)
    errorResponse (Error.errorDescriptionToWai Error.convNotFound)

  -- This endpoint can lead to the following events being sent:
  -- - ConvRename event to members
  put "/conversations/:cnv" (continue Update.updateConversationDeprecatedH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. jsonRequest @Public.ConversationRename
  document "PUT" "updateConversationName" $ do
    summary "DEPRECATED! Please use updateConversationName instead!"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    body (ref Public.modelConversationUpdateName) $
      description "JSON body"
    returns (ref Public.modelEvent)
    errorResponse (Error.errorDescriptionToWai Error.convNotFound)

  -- This endpoint can lead to the following events being sent:
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
    returns (ref Public.modelEvent)
    response 200 "Conversation joined." end
    errorResponse (Error.errorDescriptionToWai Error.convNotFound)

  post "/conversations/code-check" (continue Update.checkReusableCodeH) $
    jsonRequest @Public.ConversationCode
  document "POST" "checkConversationCode" $ do
    summary "Check validity of a conversation code"
    response 200 "Valid" end
    body (ref Public.modelConversationCode) $
      description "JSON body"
    errorResponse (Error.errorDescriptionToWai Error.codeNotFound)

  -- This endpoint can lead to the following events being sent:
  -- - MemberJoin event to members
  post "/conversations/join" (continue Update.joinConversationByReusableCodeH) $
    zauthUserId
      .&. zauthConnId
      .&. jsonRequest @Public.ConversationCode
  document "POST" "joinConversationByCode" $ do
    summary "Join a conversation using a reusable code"
    returns (ref Public.modelEvent)
    response 200 "Conversation joined." end
    body (ref Public.modelConversationCode) $
      description "JSON body"
    errorResponse (Error.errorDescriptionToWai Error.codeNotFound)
    errorResponse (Error.errorDescriptionToWai Error.convNotFound)
    errorResponse Error.tooManyMembers

  -- This endpoint can lead to the following events being sent:
  -- - ConvCodeUpdate event to members, if code didn't exist before
  post "/conversations/:cnv/code" (continue Update.addCodeH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
  document "POST" "createConversationCode" $ do
    summary "Create or recreate a conversation code"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    returns (ref Public.modelEvent)
    returns (ref Public.modelConversationCode)
    response 201 "Conversation code created." (model Public.modelEvent)
    response 200 "Conversation code already exists." (model Public.modelConversationCode)
    errorResponse (Error.errorDescriptionToWai Error.convNotFound)
    errorResponse Error.invalidAccessOp

  -- This endpoint can lead to the following events being sent:
  -- - ConvCodeDelete event to members
  delete "/conversations/:cnv/code" (continue Update.rmCodeH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
  document "DELETE" "deleteConversationCode" $ do
    summary "Delete conversation code"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    returns (ref Public.modelEvent)
    response 200 "Conversation code deleted." end
    errorResponse (Error.errorDescriptionToWai Error.convNotFound)
    errorResponse Error.invalidAccessOp

  get "/conversations/:cnv/code" (continue Update.getCodeH) $
    zauthUserId
      .&. capture "cnv"
  document "GET" "getConversationCode" $ do
    summary "Get existing conversation code"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    returns (ref Public.modelConversationCode)
    response 200 "Conversation Code" end
    errorResponse (Error.errorDescriptionToWai Error.convNotFound)
    errorResponse Error.invalidAccessOp

  -- This endpoint can lead to the following events being sent:
  -- - MemberLeave event to members, if members get removed
  -- - ConvAccessUpdate event to members
  put "/conversations/:cnv/access" (continue Update.updateConversationAccessH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. jsonRequest @Public.ConversationAccessUpdate
  document "PUT" "updateConversationAccess" $ do
    summary "Update access modes for a conversation"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    returns (ref Public.modelEvent)
    response 200 "Conversation access updated." end
    response 204 "Conversation access unchanged." end
    body (ref Public.modelConversationAccessUpdate) $
      description "JSON body"
    errorResponse (Error.errorDescriptionToWai Error.convNotFound)
    errorResponse (Error.errorDescriptionToWai Error.convAccessDenied)
    errorResponse Error.invalidTargetAccess
    errorResponse Error.invalidSelfOp
    errorResponse Error.invalidOne2OneOp
    errorResponse Error.invalidConnectOp

  -- This endpoint can lead to the following events being sent:
  -- - ConvReceiptModeUpdate event to members
  put "/conversations/:cnv/receipt-mode" (continue Update.updateConversationReceiptModeH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. jsonRequest @Public.ConversationReceiptModeUpdate
      .&. accept "application" "json"
  document "PUT" "updateConversationReceiptMode" $ do
    summary "Update receipts mode for a conversation"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    returns (ref Public.modelEvent)
    response 200 "Conversation receipt mode updated." end
    response 204 "Conversation receipt mode unchanged." end
    body (ref Public.modelConversationReceiptModeUpdate) $
      description "JSON body"
    errorResponse (Error.errorDescriptionToWai Error.convNotFound)
    errorResponse (Error.errorDescriptionToWai Error.convAccessDenied)

  -- This endpoint can lead to the following events being sent:
  -- - ConvMessageTimerUpdate event to members
  put "/conversations/:cnv/message-timer" (continue Update.updateConversationMessageTimerH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. jsonRequest @Public.ConversationMessageTimerUpdate
  document "PUT" "updateConversationMessageTimer" $ do
    summary "Update the message timer for a conversation"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    returns (ref Public.modelEvent)
    response 200 "Message timer updated." end
    response 204 "Message timer unchanged." end
    body (ref Public.modelConversationMessageTimerUpdate) $
      description "JSON body"
    errorResponse (Error.errorDescriptionToWai Error.convNotFound)
    errorResponse (Error.errorDescriptionToWai Error.convAccessDenied)
    errorResponse Error.invalidSelfOp
    errorResponse Error.invalidOne2OneOp
    errorResponse Error.invalidConnectOp

  -- This endpoint can lead to the following events being sent:
  -- - MemberJoin event to members
  post "/conversations/:cnv/members" (continue Update.addMembersH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. jsonRequest @Public.Invite
  document "POST" "addMembers" $ do
    summary "Add users to an existing conversation"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    body (ref Public.modelInvite) $
      description "JSON body"
    returns (ref Public.modelEvent)
    response 200 "Members added" end
    response 204 "No change" end
    response 412 "The user(s) cannot be added to the conversation (eg., due to legalhold policy conflict)." end
    errorResponse (Error.errorDescriptionToWai Error.convNotFound)
    errorResponse (Error.invalidOp "Conversation type does not allow adding members")
    errorResponse (Error.errorDescriptionToWai Error.notConnected)
    errorResponse (Error.errorDescriptionToWai Error.convAccessDenied)

  get "/conversations/:cnv/self" (continue Query.getSelfH) $
    zauthUserId
      .&. capture "cnv"
  document "GET" "getSelf" $ do
    summary "Get self membership properties"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    returns (ref Public.modelMember)
    errorResponse (Error.errorDescriptionToWai Error.convNotFound)

  -- This endpoint can lead to the following events being sent:
  -- - MemberStateUpdate event to self
  put "/conversations/:cnv/self" (continue Update.updateSelfMemberH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. jsonRequest @Public.MemberUpdate
  document "PUT" "updateSelf" $ do
    summary "Update self membership properties"
    notes "Even though all fields are optional, at least one needs to be given."
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    body (ref Public.modelMemberUpdate) $
      description "JSON body"
    errorResponse (Error.errorDescriptionToWai Error.convNotFound)

  -- This endpoint can lead to the following events being sent:
  -- - MemberStateUpdate event to members
  put "/conversations/:cnv/members/:usr" (continue Update.updateOtherMemberH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. capture "usr"
      .&. jsonRequest @Public.OtherMemberUpdate
  document "PUT" "updateOtherMember" $ do
    summary "Update membership of the specified user"
    notes "Even though all fields are optional, at least one needs to be given."
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    parameter Path "usr" bytes' $
      description "Target User ID"
    body (ref Public.modelOtherMemberUpdate) $
      description "JSON body"
    errorResponse (Error.errorDescriptionToWai Error.convNotFound)
    errorResponse Error.convMemberNotFound
    errorResponse Error.invalidTargetUserOp

  -- This endpoint can lead to the following events being sent:
  -- - Typing event to members
  post "/conversations/:cnv/typing" (continue Update.isTypingH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "cnv"
      .&. jsonRequest @Public.TypingData
  document "POST" "isTyping" $ do
    summary "Sending typing notifications"
    parameter Path "cnv" bytes' $
      description "Conversation ID"
    body (ref Public.modelTyping) $
      description "JSON body"
    errorResponse (Error.errorDescriptionToWai Error.convNotFound)

  -- This endpoint can lead to the following events being sent:
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
    returns (ref Public.modelEvent)
    response 200 "Member removed" end
    response 204 "No change" end
    errorResponse (Error.errorDescriptionToWai Error.convNotFound)
    errorResponse $ Error.invalidOp "Conversation type does not allow removing members"

  -- This endpoint can lead to the following events being sent:
  -- - OtrMessageAdd event to recipients
  post "/broadcast/otr/messages" (continue Update.postOtrBroadcastH) $
    zauthUserId
      .&. zauthConnId
      .&. def Public.OtrReportAllMissing filterMissing
      .&. jsonRequest @Public.NewOtrMessage
  document "POST" "postOtrBroadcast" $ do
    summary "Broadcast an encrypted message to all team members and all contacts (accepts JSON)"
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
        \having missing clients. \
        \To support large lists of user IDs exceeding the allowed \
        \URL length, you can also put this list in the body, in \
        \the optional field 'report_missing'.  That body field takes \
        \precedence over both query params."
      optional
    body (ref Public.modelNewOtrMessage) $
      description "JSON body"
    returns (ref Public.modelClientMismatch)
    response 201 "Message posted" end
    response 412 "Missing clients" end
    errorResponse Error.teamNotFound
    errorResponse Error.nonBindingTeam
    errorResponse (Error.errorDescriptionToWai Error.unknownClient)
    errorResponse Error.broadcastLimitExceeded

  -- This endpoint can lead to the following events being sent:
  -- - OtrMessageAdd event to recipients
  post "/broadcast/otr/messages" (continue Update.postProtoOtrBroadcastH) $
    zauthUserId
      .&. zauthConnId
      .&. def Public.OtrReportAllMissing filterMissing
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
    body (ref Public.modelNewOtrMessage) $
      description "Protobuf body"
    returns (ref Public.modelClientMismatch)
    response 201 "Message posted" end
    response 412 "Missing clients" end
    errorResponse Error.teamNotFound
    errorResponse Error.nonBindingTeam
    errorResponse (Error.errorDescriptionToWai Error.unknownClient)
    errorResponse Error.broadcastLimitExceeded

apiDocs :: Routes ApiBuilder Galley ()
apiDocs =
  get "/conversations/api-docs" (continue docs) $
    accept "application" "json"
      .&. query "base_url"

type JSON = Media "application" "json"

docs :: JSON ::: ByteString -> Galley Response
docs (_ ::: url) = do
  let models = Public.Swagger.models
  let apidoc = encode $ mkSwaggerApi (decodeLatin1 url) models sitemap
  pure $ responseLBS status200 [jsonContent] apidoc

-- FUTUREWORK: Maybe would be better to move it to wire-api?
filterMissing :: HasQuery r => Predicate r P.Error Public.OtrFilterMissing
filterMissing = (>>= go) <$> (query "ignore_missing" ||| query "report_missing")
  where
    go (Left ign) = case fromByteString ign of
      Just True -> return Public.OtrIgnoreAllMissing
      Just False -> return Public.OtrReportAllMissing
      Nothing -> Public.OtrIgnoreMissing <$> users "ignore_missing" ign
    go (Right rep) = case fromByteString rep of
      Just True -> return Public.OtrReportAllMissing
      Just False -> return Public.OtrIgnoreAllMissing
      Nothing -> Public.OtrReportMissing <$> users "report_missing" rep
    users :: ByteString -> ByteString -> P.Result P.Error (Set UserId)
    users src bs = case fromByteString bs of
      Nothing ->
        P.Fail $
          P.setMessage "Boolean or list of user IDs expected." $
            P.setReason P.TypeError $
              P.setSource src $
                P.err status400
      -- NB. 'fromByteString' parses a comma-separated list ('List') of
      -- user IDs, and then 'fromList' unwraps it; took me a while to
      -- understand this
      Just l -> P.Okay 0 (Set.fromList (fromList l))
