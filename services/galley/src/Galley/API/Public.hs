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
    continueE,
  )
where

import Data.Aeson (encode)
import Data.ByteString.Conversion (fromByteString, fromList)
import Data.Id
import qualified Data.Predicate as P
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Data.Swagger.Build.Api hiding (Response, def, min)
import qualified Data.Swagger.Build.Api as Swagger
import Data.Text.Encoding (decodeLatin1)
import qualified Galley.API.Error as Error
import qualified Galley.API.Query as Query
import qualified Galley.API.Teams as Teams
import qualified Galley.API.Teams.Features as Features
import Galley.App
import Galley.Effects
import qualified Galley.Effects as E
import Galley.Options
import Imports hiding (head)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (Error, or, result, setStatus)
import qualified Network.Wai.Predicate as P
import Network.Wai.Predicate.Request (HasQuery)
import Network.Wai.Routing hiding (route)
import Network.Wai.Utilities hiding (Error)
import Network.Wai.Utilities.Swagger
import Network.Wai.Utilities.ZAuth hiding (ZAuthUser)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import qualified Wire.API.Event.Team as Public ()
import qualified Wire.API.Message as Public
import qualified Wire.API.Notification as Public
import Wire.API.Routes.API
import qualified Wire.API.Swagger as Public.Swagger (models)

-- These are all the errors that can be thrown by wai-routing handlers.
-- We don't do any static checks on these errors, so we simply remap them to
-- dynamic errors. See 'continueE'.
type ErrorEffects =
  '[ ErrorS ('ActionDenied 'AddConversationMember),
     ErrorS ('ActionDenied 'LeaveConversation),
     ErrorS ('ActionDenied 'RemoveConversationMember),
     ErrorS 'ConvNotFound,
     ErrorS 'InvalidOperation,
     ErrorS 'NotConnected,
     ErrorS 'TeamNotFound,
     ErrorS 'InvalidTeamStatusUpdate,
     ErrorS 'TooManyTeamMembers,
     ErrorS 'TooManyMembers,
     ErrorS 'TeamMemberNotFound,
     ErrorS 'AccessDenied,
     ErrorS 'NotATeamMember,
     ErrorS 'NonBindingTeam,
     ErrorS OperationDenied,
     ErrorS 'InvalidPermissions,
     ErrorS 'NoAddToBinding,
     ErrorS 'UserBindingExists,
     ErrorS 'CustomBackendNotFound,
     ErrorS 'DeleteQueueFull,
     ErrorS 'NoBindingTeam,
     ErrorS 'NotAOneMemberTeam,
     ErrorS 'TeamSearchVisibilityNotEnabled,
     ErrorS 'TooManyTeamMembersOnTeamWithLegalhold,
     Error AuthenticationError
   ]

-- Wrapper of 'continue' that remaps all static errors to dynamic ones.
continueE ::
  forall a r.
  Member (Error DynError) r =>
  (a -> Sem (Append ErrorEffects r) Response) ->
  a ->
  Continue (Sem r) ->
  Sem r ResponseReceived
continueE h = continue (interpretServerEffects @ErrorEffects . h)

errorSResponse :: forall e. KnownError (MapError e) => OperationBuilder
errorSResponse = errorResponse (toWai (dynError @(MapError e)))

sitemap :: Routes ApiBuilder (Sem GalleyEffects) ()
sitemap = do
  -- Team Member API -----------------------------------------------------

  get "/teams/:tid/members/csv" (continueE Teams.getTeamMembersCSVH) $
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
    errorSResponse @'AccessDenied

  get "/teams/notifications" (continueE Teams.getTeamNotificationsH) $
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
    errorSResponse @'TeamNotFound
    errorResponse Error.invalidTeamNotificationId

  -- Bot API ------------------------------------------------------------

  get "/bot/conversation" (continueE getBotConversationH) $
    zauth ZAuthBot
      .&> zauthBotId
      .&. zauthConvId
      .&. accept "application" "json"

getBotConversationH ::
  forall r.
  ( Member E.ConversationStore r,
    Member (Input (Local ())) r,
    Member (Input Opts) r,
    Member TeamFeatureStore r,
    Member (ErrorS 'AccessDenied) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r
  ) =>
  BotId ::: ConvId ::: JSON ->
  Sem r Response
getBotConversationH arg@(bid ::: cid ::: _) =
  Features.guardSecondFactorDisabled (botUserId bid) cid (Query.getBotConversationH arg)

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
