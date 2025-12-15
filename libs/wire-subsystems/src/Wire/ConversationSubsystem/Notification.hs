{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.ConversationSubsystem.Notification where

import Data.Bifunctor
import Data.Default
import Data.Id
import Data.Json.Util
import Data.List.Extra (nubOrd)
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty)
import Data.Qualified
import Data.Set qualified as Set
import Data.Singletons
import Data.Time
import Imports
import Network.AMQP qualified as Q
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog qualified as P
import Wire.API.Component (Component(..))
import Wire.API.Conversation hiding (Member, cnvAccess, cnvAccessRoles, cnvName, cnvType)
import Wire.API.Conversation qualified as Public
import Wire.API.Conversation.Action
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Error.Galley (UnreachableBackends(..))
import Wire.API.Event.Conversation
import Wire.API.Federation.API (fedClient, sendBundle, makeConversationUpdateBundle)
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error
import Wire.API.Push.V2 qualified as PushV2
import Wire.BackendNotificationQueueAccess
import Wire.ConversationStore
import Wire.ConversationSubsystem.View
import Wire.FederationAPIAccess
import Wire.FederationAPIAccess qualified as E
import Wire.NotificationSubsystem
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.StoredConversation as Data

toConversationCreated ::
  UTCTime ->
  Local UserId ->
  StoredConversation ->
  ConversationCreated ConvId
toConversationCreated now lusr StoredConversation {metadata = ConversationMetadata {..}, ..} =
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
        OtherMember
          (tUntagged (ccRemoteOrigUserId rc))
          Nothing
          roleNameWireAdmin
   in foldMap
        ( \(me, others) ->
            guard (inDomain me) $> let mem = toMember me in (mem, conv mem (creatorOther : others))
        )
        membersView
  where
    inDomain :: OtherMember -> Bool
    inDomain = (== tDomain loc) . qDomain . Public.omQualifiedId
    setHoles :: (Ord a) => Set a -> [(a, Set a)]
    setHoles s = foldMap (\x -> [(x, Set.delete x s)]) s
    toMember :: OtherMember -> Public.Member
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
    conv :: Public.Member -> [OtherMember] -> Public.OwnConversation
    conv this others =
      Public.OwnConversation
        (tUntagged cnvId)
        ConversationMetadata
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
        (OwnConvMembers this others)
        ProtocolProteus

ensureNoUnreachableBackends ::
  (Member (Error UnreachableBackends) r) =>
  [Either (Remote e, b) a] ->
  Sem r [a]
ensureNoUnreachableBackends results = do
  let (errors, values) = partitionEithers results
  unless (null errors) $
    throw (UnreachableBackends (map (tDomain . fst) errors))
  pure values

registerRemoteConversationMemberships ::
  ( Member ConversationStore r,
    Member (Error UnreachableBackends) r,
    Member (Error FederationError) r,
    Member BackendNotificationQueueAccess r,
    Member (FederationAPIAccess FederatorClient) r
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
    creator = cnvmCreator . (.metadata) . tUnqualified $ lc

    localNonCreators :: [OtherMember]
    localNonCreators =
      fmap (localMemberToOther . tDomain $ lc)
        . filter (\lm -> lm.id_ `notElem` creator)
        . (.localMembers)
        . tUnqualified
        $ lc

    toMembers :: [RemoteMember] -> Set OtherMember
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
              (ConversationJoin (tUntagged <$> newMembers) roleNameWireMember joinType),
          extraConversationData = def
        }

    deleteOnUnreachable ::
      ( Member ConversationStore r,
        Member (Error UnreachableBackends) r
      ) =>
      Sem r a ->
      Sem r a
    deleteOnUnreachable m = catch @UnreachableBackends m $ \e -> do
      deleteConversation (tUnqualified lc).id_
      throw e

notifyCreatedConversation ::
  ( Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error ViewError) r,
    Member (Error UnreachableBackends) r,
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
  registerRemoteConversationMemberships now lusr (qualifyAs lusr c) joinType
  unless (null c.remoteMembers) $
    unlessM E.isFederationConfigured $
      throw FederationNotConfigured

  pushNotifications =<< mapM (toPush now) c.localMembers
  where
    route
      | Data.convType c == RegularConv = PushV2.RouteAny
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
