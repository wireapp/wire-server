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

module Galley.API.Public
  ( sitemap,
    apiDocs,
    filterMissing, -- for tests
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
import qualified Galley.API.CustomBackend as CustomBackend
import qualified Galley.API.Error as Error
import qualified Galley.API.LegalHold as LegalHold
import qualified Galley.API.Query as Query
import qualified Galley.API.Teams as Teams
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
import Polysemy
import qualified Wire.API.CustomBackend as Public
import qualified Wire.API.ErrorDescription as Error
import qualified Wire.API.Event.Team as Public ()
import qualified Wire.API.Message as Public
import qualified Wire.API.Notification as Public
import qualified Wire.API.Swagger as Public.Swagger (models)
import qualified Wire.API.Team.LegalHold as Public
import qualified Wire.API.Team.Member as Public
import qualified Wire.API.Team.Permission as Public
import qualified Wire.API.Team.SearchVisibility as Public
import qualified Wire.API.User as Public (UserIdList, modelUserIdList)
import Wire.Swagger (int32Between)

sitemap :: Routes ApiBuilder (Sem GalleyEffects) ()
sitemap = do
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
    errorResponse (Error.errorDescriptionTypeToWai @Error.NotATeamMember)

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
    errorResponse (Error.errorDescriptionTypeToWai @Error.NotATeamMember)
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
    errorResponse (Error.errorDescriptionTypeToWai @Error.NotATeamMember)
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
    errorResponse (Error.errorDescriptionTypeToWai @Error.TeamNotFound)
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
    errorResponse (Error.errorDescriptionTypeToWai @Error.NotATeamMember)
    errorResponse (Error.errorDescriptionToWai (Error.operationDenied Public.AddTeamMember))
    errorResponse (Error.errorDescriptionTypeToWai @Error.NotConnected)
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
    errorResponse (Error.errorDescriptionTypeToWai @Error.NotATeamMember)
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
    errorResponse (Error.errorDescriptionTypeToWai @Error.NotATeamMember)
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

apiDocs :: Routes ApiBuilder (Sem r) ()
apiDocs =
  get "/conversations/api-docs" (continue docs) $
    accept "application" "json"
      .&. query "base_url"

type JSON = Media "application" "json"

docs :: JSON ::: ByteString -> Sem r Response
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
