{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

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

module Galley.API.Create where

import Data.Default
import Data.Id
import Data.Json.Util (ToJSONObject (toJSONObject))
import Data.Qualified
import Galley.API.Mapping
import Galley.Types.Error
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog qualified as P
import Wire.API.Conversation (CreateGroupConversation (..), CreateGroupOwnConversation (..), NewConv, NewOne2OneConv)
import Wire.API.Conversation qualified as Public
import Wire.API.Error.Galley (UnreachableBackends)
import Wire.API.Event.Conversation
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error
import Wire.API.Push.V2 qualified as PushV2
import Wire.API.Routes.Public.Galley.Conversation
import Wire.API.Routes.Public.Util (ResponseForExistedCreated (..))
import Wire.BackendNotificationQueueAccess (BackendNotificationQueueAccess)
import Wire.ConversationStore (ConversationStore)
import Wire.ConversationSubsystem qualified as ConversationSubsystem
import Wire.ConversationSubsystem.Interpreter qualified as Interpreter
import Wire.FederationAPIAccess (FederationAPIAccess)
import Wire.FederationAPIAccess qualified as E
import Wire.NotificationSubsystem (NotificationSubsystem)
import Wire.NotificationSubsystem qualified as NS
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.StoredConversation (StoredConversation, localMemberToOther, remoteMemberToOther)
import Wire.StoredConversation qualified as Data

----------------------------------------------------------------------------
-- API Handlers

createGroupConversationUpToV3 ::
  ( Member ConversationSubsystem.ConversationSubsystem r,
    Member (Error InternalError) r,
    Member (Error UnreachableBackends) r,
    Member P.TinyLog r,
    Member Now r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (FederationAPIAccess FederatorClient) r,
    Member NotificationSubsystem r,
    Member BackendNotificationQueueAccess r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  NewConv ->
  Sem r (ConversationResponse Public.OwnConversation)
createGroupConversationUpToV3 lusr conn newConv = do
  dbConv <- ConversationSubsystem.createGroupConversation lusr conn newConv
  conversationCreated lusr dbConv

createGroupOwnConversation ::
  ( Member ConversationSubsystem.ConversationSubsystem r,
    Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  NewConv ->
  Sem r CreateGroupConversationResponseV9
createGroupOwnConversation lusr conn newConv = do
  dbConv <- ConversationSubsystem.createGroupConversation lusr conn newConv
  conv <- conversationViewV9 lusr dbConv
  pure . GroupConversationCreatedV9 $ CreateGroupOwnConversation conv mempty

createGroupConversation ::
  (Member ConversationSubsystem.ConversationSubsystem r) =>
  Local UserId ->
  Maybe ConnId ->
  NewConv ->
  Sem r CreateGroupConversation
createGroupConversation lusr conn newConv = do
  dbConv <- ConversationSubsystem.createGroupConversation lusr conn newConv
  pure $
    CreateGroupConversation
      { conversation = conversationView (qualifyAs lusr ()) (Just lusr) dbConv,
        failedToAdd = mempty
      }

createProteusSelfConversation ::
  ( Member ConversationSubsystem.ConversationSubsystem r,
    Member (Error InternalError) r,
    Member (Error UnreachableBackends) r,
    Member P.TinyLog r,
    Member Now r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (FederationAPIAccess FederatorClient) r,
    Member NotificationSubsystem r,
    Member BackendNotificationQueueAccess r
  ) =>
  Local UserId ->
  Sem r (ConversationResponse Public.OwnConversation)
createProteusSelfConversation lusr = do
  (c, created) <- ConversationSubsystem.createProteusSelfConversation lusr
  if created
    then conversationCreated lusr c
    else conversationExisted lusr c

createOne2OneConversation ::
  ( Member ConversationSubsystem.ConversationSubsystem r,
    Member (Error InternalError) r,
    Member (Error UnreachableBackends) r,
    Member P.TinyLog r,
    Member Now r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (FederationAPIAccess FederatorClient) r,
    Member NotificationSubsystem r,
    Member BackendNotificationQueueAccess r
  ) =>
  Local UserId ->
  ConnId ->
  NewOne2OneConv ->
  Sem r (ConversationResponse Public.OwnConversation)
createOne2OneConversation lusr zcon j = do
  (c, created) <- ConversationSubsystem.createOne2OneConversation lusr zcon j
  if created
    then conversationCreated lusr c
    else conversationExisted lusr c

----------------------------------------------------------------------------
-- Helpers

conversationCreated ::
  ( Member (Error InternalError) r,
    Member (Error UnreachableBackends) r,
    Member P.TinyLog r,
    Member Now r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (FederationAPIAccess FederatorClient) r,
    Member NotificationSubsystem r,
    Member BackendNotificationQueueAccess r
  ) =>
  Local UserId ->
  StoredConversation ->
  Sem r (ConversationResponse Public.OwnConversation)
conversationCreated lusr cnv = do
  unless (Data.convType cnv == Public.SelfConv) $ do
    notifyCreatedConversation lusr Nothing cnv def
  Created <$> conversationViewV9 lusr cnv

conversationExisted ::
  ( Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  StoredConversation ->
  Sem r (ConversationResponse Public.OwnConversation)
conversationExisted lusr cnv = Existed <$> conversationViewV9 lusr cnv

notifyCreatedConversation ::
  ( Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error UnreachableBackends) r,
    Member (Error InternalError) r,
    Member (FederationAPIAccess FederatorClient) r,
    Member NotificationSubsystem r,
    Member BackendNotificationQueueAccess r,
    Member Now r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  StoredConversation ->
  JoinType ->
  Sem r ()
notifyCreatedConversation lusr conn c joinType = do
  now <- Now.get
  Interpreter.registerRemoteConversationMemberships now lusr (qualifyAs lusr c) joinType
  unless (null c.remoteMembers) $
    unlessM E.isFederationConfigured $
      throw FederationNotConfigured

  NS.pushNotifications =<< mapM (toPush now) c.localMembers
  where
    route
      | Data.convType c == Public.RegularConv = PushV2.RouteAny
      | otherwise = PushV2.RouteDirect
    toPush t m = do
      let remoteOthers = remoteMemberToOther <$> c.remoteMembers
          localOthers = map (localMemberToOther (tDomain lusr)) $ c.localMembers
          lconv = qualifyAs lusr c.id_
      c' <- conversationViewWithCachedOthers remoteOthers localOthers c (qualifyAs lusr m.id_)
      let e = Event (tUntagged lconv) Nothing (EventFromUser (tUntagged lusr)) t Nothing (EdConversation c')
      pure $
        NS.Push
          { NS.origin = Just (tUnqualified lusr),
            NS.json = toJSONObject e,
            NS.recipients = [NS.userRecipient m.id_],
            NS.isCellsEvent = False,
            NS.route = route,
            NS.conn = conn,
            NS.transient = False,
            NS.nativePriority = Nothing,
            NS.apsData = Nothing
          }

createConnectConversation ::
  ( Member ConversationSubsystem.ConversationSubsystem r,
    Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  Connect ->
  Sem r (ConversationResponse Public.OwnConversation)
createConnectConversation lusr conn j = do
  c <- ConversationSubsystem.createConnectConversation lusr conn j
  conversationExisted lusr c
