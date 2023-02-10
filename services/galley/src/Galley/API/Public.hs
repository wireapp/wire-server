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
    filterMissing, -- for tests
    continueE,
  )
where

import Data.ByteString.Conversion (fromByteString, fromList)
import Data.Id
import qualified Data.Predicate as P
import Data.Qualified
import qualified Data.Set as Set
import qualified Galley.API.Query as Query
import qualified Galley.API.Teams.Features as Features
import Galley.App
import Galley.Cassandra.TeamFeatures
import Galley.Effects
import qualified Galley.Effects as E
import Galley.Effects.TeamFeatureStore (FeaturePersistentConstraint)
import Galley.Options
import Imports hiding (head)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (Error, or, result, setStatus)
import qualified Network.Wai.Predicate as P
import Network.Wai.Predicate.Request (HasQuery)
import Network.Wai.Routing hiding (route)
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
import Wire.API.Routes.API
import Wire.API.Team.Feature

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

sitemap :: Routes () (Sem GalleyEffects) ()
sitemap = do
  -- Bot API ------------------------------------------------------------

  get "/bot/conversation" (continueE (getBotConversationH @Cassandra)) $
    zauth ZAuthBot
      .&> zauthBotId
        .&. zauthConvId
        .&. accept "application" "json"

getBotConversationH ::
  forall db r.
  ( Member E.ConversationStore r,
    Member (Input (Local ())) r,
    Member (Input Opts) r,
    Member (TeamFeatureStore db) r,
    Member (ErrorS 'AccessDenied) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r,
    FeaturePersistentConstraint db SndFactorPasswordChallengeConfig
  ) =>
  BotId ::: ConvId ::: JSON ->
  Sem r Response
getBotConversationH arg@(bid ::: cid ::: _) =
  Features.guardSecondFactorDisabled @db (botUserId bid) cid (Query.getBotConversationH arg)

type JSON = Media "application" "json"

-- FUTUREWORK: Maybe would be better to move it to wire-api?
filterMissing :: HasQuery r => Predicate r P.Error Public.OtrFilterMissing
filterMissing = (>>= go) <$> (query "ignore_missing" ||| query "report_missing")
  where
    go (Left ign) = case fromByteString ign of
      Just True -> pure Public.OtrIgnoreAllMissing
      Just False -> pure Public.OtrReportAllMissing
      Nothing -> Public.OtrIgnoreMissing <$> users "ignore_missing" ign
    go (Right rep) = case fromByteString rep of
      Just True -> pure Public.OtrReportAllMissing
      Just False -> pure Public.OtrIgnoreAllMissing
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
