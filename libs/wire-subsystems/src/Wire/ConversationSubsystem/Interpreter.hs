{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

module Wire.ConversationSubsystem.Interpreter
  ( module X,
    interpretConversationSubsystem,
    createConversationImpl,
    sendCellsNotification,
    notifyConversationActionImpl,
    pushConversationEvent,
    toConversationCreated,
    fromConversationCreated,
    registerRemoteConversationMemberships,
    notifyCreatedConversation,
  )
where

import Data.Bifunctor (second)
import Data.Default
import Data.Id
import Data.Json.Util
import Data.List.Extra (nubOrd)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Qualified
import Data.Set qualified as Set
import Data.Singletons (Sing, sing)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Time (UTCTime)
import Galley.Types.Error (InternalError)
import Galley.Types.Error qualified as GalleyError
import Imports
import Network.AMQP qualified as Q
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as P
import System.Logger.Message (msg, val, (+++))
import Wire.API.Component (Component (Brig, Galley))
import Wire.API.Conversation qualified as Public
import Wire.API.Conversation.Action
import Wire.API.Conversation.CellsState
import Wire.API.Conversation.Protocol (Protocol (ProtocolProteus))
import Wire.API.Conversation.Role
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.API (fedClient, makeConversationUpdateBundle, sendBundle)
import Wire.API.Federation.API.Galley (ConversationCreated (..), ccRemoteOrigUserId)
import Wire.API.Federation.API.Galley.Notifications (ConversationUpdate (..))
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error
import Wire.API.Push.V2 qualified as PushV2
import Wire.BackendNotificationQueueAccess (BackendNotificationQueueAccess, enqueueNotificationsConcurrently, enqueueNotificationsConcurrentlyBuckets)
import Wire.ConversationStore (ConversationStore)
import Wire.ConversationStore qualified as ConvStore
import Wire.ConversationSubsystem
import Wire.ConversationSubsystem.Federation (ensureNoUnreachableBackends)
import Wire.ConversationSubsystem.Types as X
import Wire.ConversationSubsystem.View (conversationViewWithCachedOthers)
import Wire.ExternalAccess (ExternalAccess, deliverAsync)
import Wire.FederationAPIAccess (FederationAPIAccess, runFederatedConcurrentlyEither)
import Wire.FederationAPIAccess qualified as E
import Wire.NotificationSubsystem as NS
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.StoredConversation hiding (convTeam, id_, localOne2OneConvId)
import Wire.StoredConversation as Data (LocalMember (..), NewConversation (..), RemoteMember (..), convType)
import Wire.StoredConversation qualified as Data

interpretConversationSubsystem ::
  ( Member (Error FederationError) r,
    Member (Error GalleyError.InternalError) r,
    Member BackendNotificationQueueAccess r,
    Member NotificationSubsystem r,
    Member ExternalAccess r,
    Member Now r,
    Member (Embed IO) r,
    Member ConversationStore r,
    Member (FederationAPIAccess FederatorClient) r,
    Member TinyLog r
  ) =>
  Sem (ConversationSubsystem : r) a ->
  Sem r a
interpretConversationSubsystem = interpret $ \case
  NotifyConversationAction tag quid notifyOrigDomain con lconv targetsLocal targetsRemote targetsBots action extraData ->
    notifyConversationActionImpl tag quid notifyOrigDomain con lconv targetsLocal targetsRemote targetsBots action extraData
  CreateConversation lconv lusr newConv -> do
    res <- runError @UnreachableBackends $ runError @InternalError $ createConversationImpl lconv lusr newConv
    case res of
      Left (unreachable :: UnreachableBackends) -> throw $ FederationUnexpectedError (T.pack $ show unreachable)
      Right (Left (err :: InternalError)) -> throw err
      Right (Right val') -> pure val'

createConversationImpl ::
  ( Member (Error FederationError) r,
    Member (Error UnreachableBackends) r,
    Member (Error InternalError) r,
    Member BackendNotificationQueueAccess r,
    Member NotificationSubsystem r,
    Member Now r,
    Member (Embed IO) r,
    Member ConversationStore r,
    Member (FederationAPIAccess FederatorClient) r,
    Member TinyLog r
  ) =>
  Local ConvId ->
  Local UserId ->
  Data.NewConversation ->
  Sem r StoredConversation
createConversationImpl lconv lusr newConv = do
  storedConv <- ConvStore.upsertConversation lconv newConv
  unless (Data.convType storedConv == Public.SelfConv) $ do
    notifyCreatedConversation lusr Nothing storedConv def
  sendCellsNotification lusr Nothing storedConv
  pure storedConv

sendCellsNotification ::
  ( Member NotificationSubsystem r,
    Member Now r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  StoredConversation ->
  Sem r ()
sendCellsNotification lusr conn conv = do
  now <- Now.get
  let lconv = qualifyAs lusr conv.id_
      event = CellsEvent (tUntagged lconv) (tUntagged lusr) now CellsConvCreateNoData
  when (conv.metadata.cnvmCellsState /= CellsDisabled) $ do
    let push =
          def
            { origin = Just (tUnqualified lusr),
              json = toJSONObject event,
              isCellsEvent = True,
              route = PushV2.RouteAny,
              conn
            }
    NS.pushNotifications [push]

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
  Public.ExtraConversationData ->
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
  NS.pushNotifications [(newConversationEventPush (fmap toList lusers)) {conn}]
  deliverAsync (map (,e) (toList bots))
  where
    newConversationEventPush :: Local [UserId] -> Push
    newConversationEventPush users =
      let eventFromUser = eventFromUserId e.evtFrom
          musr = guard (tDomain users == qDomain eventFromUser) $> qUnqualified eventFromUser
       in def
            { origin = musr,
              json = toJSONObject e,
              recipients = map NS.userRecipient (tUnqualified users),
              isCellsEvent = shouldPushToCells st e
            }

toConversationCreated ::
  UTCTime ->
  Local UserId ->
  StoredConversation ->
  ConversationCreated ConvId
toConversationCreated now lusr StoredConversation {metadata = Public.ConversationMetadata {..}, ..} =
  ConversationCreated
    { time = now,
      origUserId = tUnqualified lusr,
      cnvId = id_,
      cnvType = cnvmType,
      cnvAccess = cnvmAccess,
      cnvAccessRoles = cnvmAccessRoles,
      cnvName = cnvmName,
      nonCreatorMembers = Set.empty,
      messageTimer = cnvmMessageTimer,
      receiptMode = cnvmReceiptMode,
      protocol = protocol,
      groupConvType = cnvmGroupConvType,
      channelAddPermission = cnvmChannelAddPermission
    }

fromConversationCreated ::
  Local x ->
  ConversationCreated (Remote ConvId) ->
  [(Public.Member, Public.OwnConversation)]
fromConversationCreated loc rc@ConversationCreated {..} =
  let membersView = fmap (second Set.toList) . setHoles $ nonCreatorMembers
      creatorOther =
        Public.OtherMember
          (tUntagged (ccRemoteOrigUserId rc))
          Nothing
          roleNameWireAdmin
   in foldMap
        ( \(me, others) ->
            guard (inDomain me) $> let mem = toMember me in (mem, conv mem (creatorOther : others))
        )
        membersView
  where
    inDomain :: Public.OtherMember -> Bool
    inDomain = (== tDomain loc) . qDomain . Public.omQualifiedId
    setHoles :: (Ord a) => Set a -> [(a, Set a)]
    setHoles s = foldMap (\x -> [(x, Set.delete x s)]) s
    toMember :: Public.OtherMember -> Public.Member
    toMember m =
      Public.Member
        { memId = Public.omQualifiedId m,
          memService = Public.omService m,
          memOtrMutedStatus = Nothing,
          memOtrMutedRef = Nothing,
          memOtrArchived = False,
          memOtrArchivedRef = Nothing,
          memHidden = False,
          memHiddenRef = Nothing,
          memConvRoleName = Public.omConvRoleName m
        }
    conv :: Public.Member -> [Public.OtherMember] -> Public.OwnConversation
    conv this others =
      Public.OwnConversation
        (tUntagged cnvId)
        Public.ConversationMetadata
          { cnvmType = cnvType,
            cnvmCreator = Just origUserId,
            cnvmAccess = cnvAccess,
            cnvmAccessRoles = cnvAccessRoles,
            cnvmName = cnvName,
            cnvmTeam = Nothing,
            cnvmMessageTimer = messageTimer,
            cnvmReceiptMode = receiptMode,
            cnvmGroupConvType = groupConvType,
            cnvmChannelAddPermission = channelAddPermission,
            cnvmCellsState = def,
            cnvmParent = Nothing
          }
        (Public.OwnConvMembers this others)
        ProtocolProteus

registerRemoteConversationMemberships ::
  ( Member ConvStore.ConversationStore r,
    Member (Error UnreachableBackends) r,
    Member (Error FederationError) r,
    Member BackendNotificationQueueAccess r,
    Member (FederationAPIAccess FederatorClient) r,
    Member TinyLog r
  ) =>
  UTCTime ->
  Local UserId ->
  Local StoredConversation ->
  JoinType ->
  Sem r ()
registerRemoteConversationMemberships now lusr lc joinType = deleteOnUnreachable $ do
  let c = tUnqualified lc
      rc = toConversationCreated now lusr c
      allRemoteMembers = nubOrd c.remoteMembers
      allRemoteMembersQualified = remoteMemberQualify <$> allRemoteMembers
      allRemoteBuckets :: [Remote [RemoteMember]] = bucketRemote allRemoteMembersQualified

  void . (ensureNoUnreachableBackends =<<) $
    runFederatedConcurrentlyEither allRemoteMembersQualified $ \_ ->
      void $ fedClient @'Brig @"api-version" ()

  void . (ensureNoUnreachableBackends =<<) $
    runFederatedConcurrentlyEither allRemoteMembersQualified $
      \rrms ->
        fedClient @'Galley @"on-conversation-created"
          ( rc
              { nonCreatorMembers =
                  toMembers (tUnqualified rrms)
              }
          )

  let joined :: [Remote [RemoteMember]] = allRemoteBuckets
      joinedCoupled :: [Remote ([RemoteMember], NonEmpty (Remote UserId))]
      joinedCoupled =
        foldMap
          ( \ruids ->
              let nj =
                    foldMap (fmap (.id_) . tUnqualified) $
                      filter (\r -> tDomain r /= tDomain ruids) joined
               in case NE.nonEmpty nj of
                    Nothing -> []
                    Just v -> [fmap (,v) ruids]
          )
          joined

  void $ enqueueNotificationsConcurrentlyBuckets Q.Persistent joinedCoupled $ \z ->
    makeConversationUpdateBundle (convUpdateJoin z) >>= sendBundle
  where
    creator :: Maybe UserId
    creator = Public.cnvmCreator . (.metadata) . tUnqualified $ lc

    localNonCreators :: [Public.OtherMember]
    localNonCreators =
      fmap (localMemberToOther . tDomain $ lc)
        . filter (\lm -> lm.id_ `notElem` creator)
        . (.localMembers)
        . tUnqualified
        $ lc

    toMembers :: [RemoteMember] -> Set Public.OtherMember
    toMembers rs = Set.fromList $ localNonCreators <> fmap remoteMemberToOther rs

    convUpdateJoin :: Remote ([RemoteMember], NonEmpty (Remote UserId)) -> ConversationUpdate
    convUpdateJoin (tUnqualified -> (toNotify, newMembers)) =
      ConversationUpdate
        { time = now,
          origUserId = tUntagged lusr,
          convId = (tUnqualified lc).id_,
          alreadyPresentUsers = fmap (\m -> tUnqualified $ m.id_) toNotify,
          action =
            SomeConversationAction
              (sing @'ConversationJoinTag)
              (Public.ConversationJoin (tUntagged <$> newMembers) roleNameWireMember joinType),
          extraConversationData = def
        }

    deleteOnUnreachable ::
      ( Member ConvStore.ConversationStore r,
        Member (Error UnreachableBackends) r,
        Member TinyLog r
      ) =>
      Sem r a ->
      Sem r a
    deleteOnUnreachable m = catch @UnreachableBackends m $ \e -> do
      P.err . msg $
        val "Unreachable backend when notifying"
          +++ val "error"
          +++ (LT.pack . show $ e)
      ConvStore.deleteConversation (tUnqualified lc).id_
      throw e

notifyCreatedConversation ::
  ( Member ConvStore.ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (Error UnreachableBackends) r,
    Member (FederationAPIAccess FederatorClient) r,
    Member NotificationSubsystem r,
    Member BackendNotificationQueueAccess r,
    Member Now r,
    Member TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  StoredConversation ->
  JoinType ->
  Sem r ()
notifyCreatedConversation lusr conn c joinType = do
  now <- Now.get
  registerRemoteConversationMemberships now lusr (qualifyAs lusr c) joinType
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
        def
          { origin = Just (tUnqualified lusr),
            json = toJSONObject e,
            recipients = [localMemberToRecipient m],
            isCellsEvent = False,
            route,
            conn
          }
