-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.ConversationSubsystem.Interpreter where

import Data.Default
import Data.Id
import Data.Json.Util (ToJSONObject (toJSONObject))
import Data.Qualified
import Data.Singletons (Sing)
import Galley.Types.Teams (FeatureDefaults)
import Imports
import Network.AMQP qualified as Q
import Polysemy
import Polysemy.Error
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Action
import Wire.API.Conversation.CellsState (CellsState (..))
import Wire.API.Conversation.Protocol (ProtocolTag)
import Wire.API.Event.Conversation
import Wire.API.Federation.API (makeConversationUpdateBundle, sendBundle)
import Wire.API.Federation.API.Galley.Notifications (ConversationUpdate (..))
import Wire.API.Federation.Error (FederationError)
import Wire.API.MLS.Keys (MLSKeysByPurpose, MLSPrivateKeys)
import Wire.API.Team.Feature (LegalholdConfig)
import Wire.BackendNotificationQueueAccess (BackendNotificationQueueAccess, enqueueNotificationsConcurrently)
import Wire.ConversationSubsystem
import Wire.ExternalAccess (ExternalAccess, deliverAsync)
import Wire.NotificationSubsystem as NS
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.StoredConversation

data ConversationSubsystemConfig = ConversationSubsystemConfig
  { mlsKeys :: Maybe (MLSKeysByPurpose MLSPrivateKeys),
    federationProtocols :: Maybe [ProtocolTag],
    legalholdDefaults :: FeatureDefaults LegalholdConfig,
    maxConvSize :: Word16
  }

interpretConversationSubsystem ::
  ( Member (Error FederationError) r,
    Member BackendNotificationQueueAccess r,
    Member NotificationSubsystem r,
    Member ExternalAccess r,
    Member Now r
  ) =>
  Sem (ConversationSubsystem : r) a ->
  Sem r a
interpretConversationSubsystem = interpret $ \case
  NotifyConversationAction tag quid notifyOrigDomain con lconv targetsLocal targetsRemote targetsBots action extraData ->
    notifyConversationActionImpl tag quid notifyOrigDomain con lconv targetsLocal targetsRemote targetsBots action extraData

notifyConversationActionImpl ::
  forall tag r.
  ( Member BackendNotificationQueueAccess r,
    Member ExternalAccess r,
    Member (Error FederationError) r,
    Member Now r,
    Member NotificationSubsystem r
  ) =>
  Sing tag ->
  EventFrom ->
  Bool ->
  Maybe ConnId ->
  Local StoredConversation ->
  Set UserId ->
  Set (Remote UserId) ->
  Set BotMember ->
  ConversationAction (tag :: ConversationActionTag) ->
  ExtraConversationData ->
  Sem r LocalConversationUpdate
notifyConversationActionImpl tag eventFrom notifyOrigDomain con lconv targetsLocal targetsRemote targetsBots action extraData = do
  now <- Now.get
  let lcnv = fmap (.id_) lconv
      conv = tUnqualified lconv
      tid = conv.metadata.cnvmTeam
      e = conversationActionToEvent tag now eventFrom (tUntagged lcnv) extraData Nothing tid action
      quid = eventFromUserId eventFrom
      mkUpdate uids =
        ConversationUpdate
          { time = now,
            origUserId = quid,
            convId = tUnqualified lcnv,
            alreadyPresentUsers = uids,
            action = SomeConversationAction tag action,
            extraConversationData = Just extraData
          }
  update <-
    fmap (fromMaybe (mkUpdate []) . asum . map tUnqualified) $
      enqueueNotificationsConcurrently Q.Persistent (toList targetsRemote) $
        \ruids -> do
          let update = mkUpdate (tUnqualified ruids)
          if notifyOrigDomain || tDomain ruids /= qDomain quid
            then do
              makeConversationUpdateBundle update >>= sendBundle
              pure Nothing
            else pure (Just update)

  pushConversationEvent con conv.metadata.cnvmCellsState e (qualifyAs lcnv targetsLocal) targetsBots

  pure $ LocalConversationUpdate {lcuEvent = e, lcuUpdate = update}

pushConversationEvent ::
  ( Member ExternalAccess r,
    Member NotificationSubsystem r,
    Foldable f
  ) =>
  Maybe ConnId ->
  CellsState ->
  Event ->
  Local (f UserId) ->
  f BotMember ->
  Sem r ()
pushConversationEvent conn st e lusers bots = do
  pushNotifications [(newConversationEventPush (fmap toList lusers)) {conn}]
  deliverAsync (map (,e) (toList bots))
  where
    newConversationEventPush :: Local [UserId] -> Push
    newConversationEventPush users =
      let eventFromUser = eventFromUserId e.evtFrom
          musr = guard (tDomain users == qDomain eventFromUser) $> qUnqualified eventFromUser
       in def
            { origin = musr,
              json = toJSONObject e,
              recipients = map userRecipient (tUnqualified users),
              isCellsEvent = shouldPushToCells st e
            }
