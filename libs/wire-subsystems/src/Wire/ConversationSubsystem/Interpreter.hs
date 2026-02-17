-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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
  ( interpretConversationSubsystem,
  )
where

import Control.Error (headMay)
import Control.Lens hiding ((??))
import Data.Default
import Data.Id
import Data.Json.Util (ToJSONObject (toJSONObject))
import Data.Misc (FutureWork (FutureWork))
import Data.Qualified
import Data.Range
import Data.Set qualified as Set
import Data.Singletons (Sing)
import Data.UUID.Tagged qualified as U
import GHC.TypeNats
import Galley.Types.Error (InternalError, InvalidInput (..))
import Imports
import Network.AMQP qualified as Q
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation qualified as Public
import Wire.API.Conversation.Action
import Wire.API.Conversation.CellsState
import Wire.API.Conversation.Config
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.API (makeConversationUpdateBundle, sendBundle)
import Wire.API.Federation.API.Galley.Notifications (ConversationUpdate (..))
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error
import Wire.API.History (History (HistoryPrivate))
import Wire.API.Push.V2 qualified as PushV2
import Wire.API.Team
import Wire.API.Team.Collaborator qualified as CollaboratorPermission
import Wire.API.Team.Feature
import Wire.API.Team.Feature qualified as Conf
import Wire.API.Team.FeatureFlags (notTeamMember)
import Wire.API.Team.LegalHold (LegalholdProtectee (LegalholdPlusFederationNotImplemented))
import Wire.API.Team.Member
import Wire.API.Team.Permission hiding (self)
import Wire.API.User
import Wire.BackendNotificationQueueAccess (BackendNotificationQueueAccess, enqueueNotificationsConcurrently)
import Wire.BrigAPIAccess
import Wire.ConversationStore (ConversationStore)
import Wire.ConversationStore qualified as ConvStore
import Wire.ConversationSubsystem
import Wire.ConversationSubsystem qualified as ConversationSubsystem
import Wire.ConversationSubsystem.One2One
import Wire.ConversationSubsystem.Util
import Wire.ExternalAccess (ExternalAccess)
import Wire.FeaturesConfigSubsystem
import Wire.FederationAPIAccess (FederationAPIAccess)
import Wire.LegalHoldStore (LegalHoldStore)
import Wire.NotificationSubsystem as NS
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.Sem.Random (Random)
import Wire.Sem.Random qualified as Random
import Wire.StoredConversation hiding (convTeam, id_, localOne2OneConvId)
import Wire.StoredConversation as Data (NewConversation (..), convType)
import Wire.StoredConversation qualified as Data
import Wire.TeamCollaboratorsSubsystem
import Wire.TeamStore (TeamStore)
import Wire.TeamStore qualified as TeamStore
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.TeamSubsystem qualified as TeamSubsystem
import Wire.UserList (UserList (UserList), toUserList, ulAddLocal, ulAll, ulFromLocals, ulLocals, ulRemotes)

interpretConversationSubsystem ::
  ( Member (Error FederationError) r,
    Member (Error UnreachableBackends) r,
    Member (Error InternalError) r,
    Member (Error InvalidInput) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'NotConnected) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (ErrorS 'MLSNonEmptyMemberList) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'NonBindingTeam) r,
    Member (ErrorS 'NoBindingTeamMembers) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'ChannelsNotEnabled) r,
    Member (ErrorS 'NotAnMlsConversation) r,
    Member (ErrorS HistoryNotSupported) r,
    Member BackendNotificationQueueAccess r,
    Member NotificationSubsystem r,
    Member ExternalAccess r,
    Member Now r,
    Member ConversationStore r,
    Member (FederationAPIAccess FederatorClient) r,
    Member BrigAPIAccess r,
    Member FeaturesConfigSubsystem r,
    Member TeamCollaboratorsSubsystem r,
    Member Random r,
    Member TeamSubsystem r,
    Member (Input ConversationSubsystemConfig) r,
    Member LegalHoldStore r,
    Member TeamStore r
  ) =>
  Sem (ConversationSubsystem : r) a ->
  Sem r a
interpretConversationSubsystem = interpret $ \case
  NotifyConversationAction tag quid notifyOrigDomain con lconv targetsLocal targetsRemote targetsBots action extraData ->
    notifyConversationActionImpl tag quid notifyOrigDomain con lconv targetsLocal targetsRemote targetsBots action extraData
  ConversationSubsystem.CreateGroupConversation lusr conn newConv ->
    createGroupConversationGeneric lusr conn newConv
  ConversationSubsystem.CreateOne2OneConversation lusr conn newOne2One ->
    createOne2OneConversationLogic lusr conn newOne2One
  ConversationSubsystem.CreateProteusSelfConversation lusr ->
    createProteusSelfConversationLogic lusr
  ConversationSubsystem.CreateConnectConversation lusr conn j ->
    createConnectConversationLogic lusr conn j

createGroupConversationGeneric ::
  forall r.
  ( Member BrigAPIAccess r,
    Member ConversationStore r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (Error InvalidInput) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'NotConnected) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (ErrorS 'MLSNonEmptyMemberList) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'ChannelsNotEnabled) r,
    Member (ErrorS 'NotAnMlsConversation) r,
    Member (ErrorS HistoryNotSupported) r,
    Member (Input ConversationSubsystemConfig) r,
    Member LegalHoldStore r,
    Member TeamStore r,
    Member FeaturesConfigSubsystem r,
    Member TeamCollaboratorsSubsystem r,
    Member Random r,
    Member TeamSubsystem r,
    Member Now r,
    Member NotificationSubsystem r,
    Member (Error FederationError) r,
    Member (Error UnreachableBackends) r,
    Member BackendNotificationQueueAccess r,
    Member (FederationAPIAccess FederatorClient) r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  Public.NewConv ->
  Sem r StoredConversation
createGroupConversationGeneric lusr conn newConv = do
  (nc, fromConvSize -> allUsers) <- newRegularConversation lusr newConv
  checkCreateConvPermissions lusr newConv newConv.newConvTeam allUsers
  ensureNoLegalholdConflicts allUsers
  when (newConv.newConvHistory /= HistoryPrivate && newConv.newConvGroupConvType /= Channel) $
    throwS @HistoryNotSupported

  when (Public.newConvProtocol newConv == BaseProtocolMLSTag) $ do
    assertMLSEnabled

  lcnv <- traverse (const Random.newId) lusr
  storedConv <- createConversationImpl lcnv lusr nc
  notifyConversationCreated lusr conn storedConv def
  sendCellsNotification lusr conn storedConv
  pure storedConv

createOne2OneConversationLogic ::
  ( Member BrigAPIAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error UnreachableBackends) r,
    Member (Error InvalidInput) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'NonBindingTeam) r,
    Member (ErrorS 'NoBindingTeamMembers) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'NotConnected) r,
    Member TeamStore r,
    Member TeamCollaboratorsSubsystem r,
    Member TeamSubsystem r,
    Member Now r,
    Member NotificationSubsystem r,
    Member BackendNotificationQueueAccess r,
    Member (FederationAPIAccess FederatorClient) r
  ) =>
  Local UserId ->
  ConnId ->
  Public.NewOne2OneConv ->
  Sem r (StoredConversation, Bool)
createOne2OneConversationLogic lusr zcon j = do
  let allUsers = newOne2OneConvMembers lusr j
  other <- ensureOne (ulAll lusr allUsers)
  when (tUntagged lusr == other) $
    throwS @'InvalidOperation
  mtid <- case j.team of
    Just ti -> do
      foldQualified
        lusr
        (\lother -> checkBindingTeamPermissions lusr lother (cnvTeamId ti))
        (const (pure Nothing))
        other
    Nothing -> ensureConnected lusr allUsers $> Nothing
  foldQualified
    lusr
    (createLegacyOne2OneConversationUnchecked lusr zcon j.name mtid)
    (createOne2OneConversationUnchecked lusr zcon j.name mtid . tUntagged)
    other

createProteusSelfConversationLogic ::
  (Member ConversationStore r) =>
  Local UserId ->
  Sem r (StoredConversation, Bool)
createProteusSelfConversationLogic lusr = do
  let lcnv = fmap Data.selfConv lusr
  c <- ConvStore.getConversation (tUnqualified lcnv)
  maybe (create lcnv) (\conv -> pure (conv, False)) c
  where
    create lcnv = do
      let nc =
            Data.NewConversation
              { metadata = (defConversationMetadata (Just (tUnqualified lusr))) {cnvmType = Public.SelfConv},
                users = ulFromLocals [toUserRole (tUnqualified lusr)],
                protocol = BaseProtocolProteusTag,
                groupId = Nothing
              }
      conv <- createConversationImpl lcnv lusr nc
      pure (conv, True)

createConversationImpl ::
  (Member ConversationStore r) =>
  Local ConvId ->
  Local UserId ->
  Data.NewConversation ->
  Sem r StoredConversation
createConversationImpl lconv _lusr =
  ConvStore.upsertConversation lconv

createConnectConversationLogic ::
  ( Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (Error InvalidInput) r,
    Member (Error UnreachableBackends) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member NotificationSubsystem r,
    Member BackendNotificationQueueAccess r,
    Member Now r,
    Member (FederationAPIAccess FederatorClient) r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  Connect ->
  Sem r (StoredConversation, Bool)
createConnectConversationLogic lusr conn j = do
  lrecipient <- ensureLocal lusr (cRecipient j)
  n <- rangeCheckedMaybe (cName j)
  let meta =
        (defConversationMetadata (Just (tUnqualified lusr)))
          { cnvmType = Public.ConnectConv,
            cnvmName = fmap fromRange n
          }
  lcnv <- localOne2OneConvId lusr lrecipient
  let nc =
        Data.NewConversation
          { -- We add only one member, second one gets added later,
            -- when the other user accepts the connection request.
            users = ulFromLocals [(toUserRole . tUnqualified) lusr],
            protocol = BaseProtocolProteusTag,
            metadata = meta,
            groupId = Nothing
          }
  mconv <- ConvStore.getConversation (tUnqualified lcnv)
  case mconv of
    Nothing -> do
      conv <- create lcnv nc
      pure (conv, True)
    Just conv -> do
      conv' <- update n conv
      pure (conv', False)
  where
    create lcnv nc = do
      conv <- createConversationImpl lcnv lusr nc
      notifyConversationCreated lusr conn conv def
      notifyConversationUpdated lusr conn j conv
      pure conv
    update n conv = do
      let mems = conv.localMembers
      if tUnqualified lusr `isMember` mems
        then -- we already were in the conversation, maybe also other
          connect n conv
        else do
          let lcid = qualifyAs lusr conv.id_
          mm <- ConvStore.upsertMember lcid lusr
          let conv' =
                conv
                  { localMembers = conv.localMembers <> toList mm
                  }
          if null mems
            then -- the conversation was empty
              connect n conv'
            else do
              -- we were not in the conversation, but someone else
              conv'' <- acceptOne2One lusr conv' conn
              if Data.convType conv'' == Public.ConnectConv
                then connect n conv''
                else pure conv''
    connect n conv
      | Data.convType conv == Public.ConnectConv = do
          n' <- case n of
            Just x -> do
              ConvStore.setConversationName conv.id_ x
              pure . Just $ fromRange x
            Nothing -> pure $ Data.convName conv
          notifyConversationUpdated lusr conn j conv
          pure $ Data.convSetName n' conv
      | otherwise = pure conv

ensureNoLegalholdConflicts ::
  ( Member (ErrorS 'MissingLegalholdConsent) r,
    Member (Input ConversationSubsystemConfig) r,
    Member LegalHoldStore r,
    Member TeamStore r,
    Member TeamSubsystem r
  ) =>
  UserList UserId ->
  Sem r ()
ensureNoLegalholdConflicts (UserList locals remotes) = do
  let FutureWork _remotes = FutureWork @'LegalholdPlusFederationNotImplemented remotes
  whenM (anyLegalholdActivated locals) $
    unlessM (allLegalholdConsentGiven locals) $
      throwS @'MissingLegalholdConsent

checkCreateConvPermissions ::
  ( Member BrigAPIAccess r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'NotConnected) r,
    Member (ErrorS 'ChannelsNotEnabled) r,
    Member (ErrorS 'NotAnMlsConversation) r,
    Member TeamStore r,
    Member FeaturesConfigSubsystem r,
    Member TeamCollaboratorsSubsystem r,
    Member TeamSubsystem r
  ) =>
  Local UserId ->
  Public.NewConv ->
  Maybe ConvTeamInfo ->
  UserList UserId ->
  Sem r ()
checkCreateConvPermissions lusr newConv Nothing allUsers = do
  when (newConv.newConvGroupConvType == Channel) $ throwS @OperationDenied
  activated <- listToMaybe <$> lookupActivatedUsers [tUnqualified lusr]
  void $ noteS @OperationDenied activated
  tm <- getTeamMember (tUnqualified lusr) Nothing
  for_ tm $
    permissionCheck AddRemoveConvMember . Just
  ensureConnected lusr allUsers
checkCreateConvPermissions lusr newConv (Just tinfo) allUsers = do
  let convTeam = cnvTeamId tinfo
  mTeamMember <- getTeamMember (tUnqualified lusr) (Just convTeam)
  teamAssociation <- case mTeamMember of
    Just tm -> pure (Just (Right tm))
    Nothing -> do
      Left <$$> internalGetTeamCollaborator convTeam (tUnqualified lusr)

  case newConv.newConvGroupConvType of
    Channel -> do
      ensureCreateChannelPermissions tinfo.cnvTeamId mTeamMember
    GroupConversation -> do
      void $ permissionCheck CreateConversation teamAssociation
      when (length allUsers > 1 || Public.newConvProtocol newConv == BaseProtocolMLSTag) $ do
        void $ permissionCheck AddRemoveConvMember teamAssociation

  convLocalMemberships <- mapM (flip TeamSubsystem.internalGetTeamMember convTeam) (ulLocals allUsers)
  ensureAccessRole (accessRoles newConv) (zip (ulLocals allUsers) convLocalMemberships)
  ensureConnectedToLocals (tUnqualified lusr) (notTeamMember (ulLocals allUsers) (catMaybes convLocalMemberships))
  ensureConnectedToRemotes lusr (ulRemotes allUsers)
  where
    ensureCreateChannelPermissions ::
      forall r.
      ( Member (ErrorS OperationDenied) r,
        Member FeaturesConfigSubsystem r,
        Member (ErrorS 'NotATeamMember) r,
        Member (ErrorS 'ChannelsNotEnabled) r,
        Member (ErrorS 'NotAnMlsConversation) r
      ) =>
      TeamId ->
      Maybe TeamMember ->
      Sem r ()
    ensureCreateChannelPermissions tid (Just tm) = do
      channelsConf :: LockableFeature ChannelsConfig <- getFeatureForTeam tid
      when (channelsConf.status == FeatureStatusDisabled) $ throwS @'ChannelsNotEnabled
      when (Public.newConvProtocol newConv /= BaseProtocolMLSTag) $ throwS @'NotAnMlsConversation
      case channelsConf.config.allowedToCreateChannels of
        Conf.Everyone -> pure ()
        Conf.TeamMembers -> void $ permissionCheck AddRemoveConvMember $ Just tm
        Conf.Admins -> unless (isAdminOrOwner (tm ^. permissions)) $ throwS @OperationDenied
    ensureCreateChannelPermissions _ Nothing = do
      throwS @'NotATeamMember

getTeamMember :: (Member TeamStore r, Member TeamSubsystem r) => UserId -> Maybe TeamId -> Sem r (Maybe TeamMember)
getTeamMember uid (Just tid) = TeamSubsystem.internalGetTeamMember uid tid
getTeamMember uid Nothing = TeamStore.getUserTeams uid >>= maybe (pure Nothing) (TeamSubsystem.internalGetTeamMember uid) . headMay

createLegacyOne2OneConversationUnchecked ::
  ( Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error UnreachableBackends) r,
    Member (Error InvalidInput) r,
    Member Now r,
    Member NotificationSubsystem r,
    Member BackendNotificationQueueAccess r,
    Member (FederationAPIAccess FederatorClient) r
  ) =>
  Local UserId ->
  ConnId ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Local UserId ->
  Sem r (StoredConversation, Bool)
createLegacyOne2OneConversationUnchecked self zcon name mtid other = do
  lcnv <- localOne2OneConvId self other
  let meta =
        (defConversationMetadata (Just (tUnqualified self)))
          { cnvmType = Public.One2OneConv,
            cnvmTeam = mtid,
            cnvmName = fmap fromRange name
          }
  let nc =
        Data.NewConversation
          { users = ulFromLocals (map (toUserRole . tUnqualified) [self, other]),
            protocol = BaseProtocolProteusTag,
            metadata = meta,
            groupId = Nothing
          }
  mc <- ConvStore.getConversation (tUnqualified lcnv)
  case mc of
    Just c -> pure (c, False)
    Nothing -> do
      conv <- createConversationImpl lcnv self nc
      notifyConversationCreated self (Just zcon) conv def
      pure (conv, True)

createOne2OneConversationUnchecked ::
  ( Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error UnreachableBackends) r,
    Member Now r,
    Member NotificationSubsystem r,
    Member BackendNotificationQueueAccess r,
    Member (FederationAPIAccess FederatorClient) r
  ) =>
  Local UserId ->
  ConnId ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Qualified UserId ->
  Sem r (StoredConversation, Bool)
createOne2OneConversationUnchecked self zcon name mtid other = do
  let create =
        foldQualified
          self
          createOne2OneConversationLocally
          createOne2OneConversationRemotely
  create (one2OneConvId BaseProtocolProteusTag (tUntagged self) other) self zcon name mtid other

createOne2OneConversationLocally ::
  ( Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error UnreachableBackends) r,
    Member Now r,
    Member NotificationSubsystem r,
    Member BackendNotificationQueueAccess r,
    Member (FederationAPIAccess FederatorClient) r
  ) =>
  Local ConvId ->
  Local UserId ->
  ConnId ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Qualified UserId ->
  Sem r (StoredConversation, Bool)
createOne2OneConversationLocally lcnv self zcon name mtid other = do
  mc <- ConvStore.getConversation (tUnqualified lcnv)
  case mc of
    Just c -> pure (c, False)
    Nothing -> do
      let meta =
            (defConversationMetadata (Just (tUnqualified self)))
              { cnvmType = Public.One2OneConv,
                cnvmTeam = mtid,
                cnvmName = fmap fromRange name
              }
      let nc =
            Data.NewConversation
              { metadata = meta,
                users = fmap toUserRole (toUserList lcnv [tUntagged self, other]),
                protocol = BaseProtocolProteusTag,
                groupId = Nothing
              }
      conv <- createConversationImpl lcnv self nc
      notifyConversationCreated self (Just zcon) conv def
      pure (conv, True)

createOne2OneConversationRemotely ::
  (Member (Error FederationError) r) =>
  Remote ConvId ->
  Local UserId ->
  ConnId ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Qualified UserId ->
  Sem r (StoredConversation, Bool)
createOne2OneConversationRemotely _ _ _ _name _mtid _ =
  throw FederationNotImplemented

newRegularConversation ::
  ( Member (ErrorS 'MLSNonEmptyMemberList) r,
    Member (ErrorS OperationDenied) r,
    Member (Error InvalidInput) r,
    Member (Input ConversationSubsystemConfig) r,
    Member ConversationStore r
  ) =>
  Local UserId ->
  Public.NewConv ->
  Sem r (Data.NewConversation, ConvSizeChecked UserList UserId)
newRegularConversation lusr newConv = do
  cfg <- input
  let uncheckedUsers = newConvMembers lusr newConv
  forM_ newConv.newConvParent $ \parent -> do
    mMembership <- ConvStore.getLocalMember parent (tUnqualified lusr)
    when (isNothing mMembership) $
      throwS @OperationDenied
  users <- case Public.newConvProtocol newConv of
    BaseProtocolProteusTag -> checkedConvSize cfg uncheckedUsers
    BaseProtocolMLSTag -> do
      unless (null uncheckedUsers) $ throwS @'MLSNonEmptyMemberList
      pure mempty
  let usersWithoutCreator = (,newConvUsersRole newConv) <$> fromConvSize users
      newConvUsersRoles =
        if newConv.newConvSkipCreator
          then usersWithoutCreator
          else ulAddLocal (toUserRole (tUnqualified lusr)) usersWithoutCreator
  let nc =
        Data.NewConversation
          { metadata =
              Public.ConversationMetadata
                { cnvmType = Public.RegularConv,
                  cnvmCreator = Just (tUnqualified lusr),
                  cnvmAccess = access newConv,
                  cnvmAccessRoles = accessRoles newConv,
                  cnvmName = fmap fromRange newConv.newConvName,
                  cnvmMessageTimer = newConv.newConvMessageTimer,
                  cnvmReceiptMode = case Public.newConvProtocol newConv of
                    BaseProtocolProteusTag -> newConv.newConvReceiptMode
                    BaseProtocolMLSTag -> Just def,
                  cnvmTeam = fmap cnvTeamId newConv.newConvTeam,
                  cnvmGroupConvType = Just newConv.newConvGroupConvType,
                  cnvmChannelAddPermission = if newConv.newConvGroupConvType == Channel then newConv.newConvChannelAddPermission <|> Just def else Nothing,
                  cnvmCellsState =
                    if newConv.newConvCells
                      then CellsPending
                      else CellsDisabled,
                  cnvmParent = newConv.newConvParent,
                  cnvmHistory = newConv.newConvHistory
                },
            users = newConvUsersRoles,
            protocol = Public.newConvProtocol newConv,
            groupId = Nothing
          }
  pure (nc, users)

localOne2OneConvId ::
  (Member (Error InvalidInput) r) =>
  Local UserId ->
  Local UserId ->
  Sem r (Local ConvId)
localOne2OneConvId self other = do
  (x, y) <- toUUIDs (tUnqualified self) (tUnqualified other)
  pure . qualifyAs self $ Data.localOne2OneConvId x y
  where
    toUUIDs ::
      (Member (Error InvalidInput) r) =>
      UserId ->
      UserId ->
      Sem r (U.UUID U.V4, U.UUID U.V4)
    toUUIDs a b = do
      a' <- U.fromUUID (toUUID a) & note InvalidUUID4
      b' <- U.fromUUID (toUUID b) & note InvalidUUID4
      pure (a', b')

accessRoles :: Public.NewConv -> Set AccessRole
accessRoles b = fromMaybe defRole (newConvAccessRoles b)

access :: Public.NewConv -> [Access]
access a = case Set.toList (Public.newConvAccess a) of
  [] -> Data.defRegularConvAccess
  (x : xs) -> x : xs

newConvMembers :: Local x -> Public.NewConv -> UserList UserId
newConvMembers loc body =
  UserList (newConvUsers body) []
    <> toUserList loc (newConvQualifiedUsers body)

newOne2OneConvMembers :: Local x -> Public.NewOne2OneConv -> UserList UserId
newOne2OneConvMembers loc body =
  UserList body.users []
    <> toUserList loc body.qualifiedUsers

ensureOne :: (Member (Error InvalidInput) r) => [a] -> Sem r a
ensureOne [x] = pure x
ensureOne _ = throw (InvalidRange "One-to-one conversations can only have a single invited member")

assertMLSEnabled :: (Member (Input ConversationSubsystemConfig) r, Member (ErrorS 'MLSNotEnabled) r) => Sem r ()
assertMLSEnabled = do
  cfg <- input
  when (null cfg.mlsKeys) $ throwS @'MLSNotEnabled

newtype ConvSizeChecked f a = ConvSizeChecked {fromConvSize :: f a}
  deriving (Functor, Foldable, Traversable)
  deriving newtype (Semigroup, Monoid)

checkedConvSize ::
  (Member (Error InvalidInput) r, Foldable f) =>
  ConversationSubsystemConfig ->
  f a ->
  Sem r (ConvSizeChecked f a)
checkedConvSize cfg x = do
  let minV :: Integer = 0
      limit = cfg.maxConvSize - 1
  if length x <= fromIntegral limit
    then pure (ConvSizeChecked x)
    else throwErr (errorMsg minV limit "")

rangeChecked :: (KnownNat n, KnownNat m, Member (Error InvalidInput) r, Within a n m) => a -> Sem r (Range n m a)
rangeChecked = either throwErr pure . checkedEither
{-# INLINE rangeChecked #-}

rangeCheckedMaybe ::
  (Member (Error InvalidInput) r, KnownNat n, KnownNat m, Within a n m) =>
  Maybe a ->
  Sem r (Maybe (Range n m a))
rangeCheckedMaybe Nothing = pure Nothing
rangeCheckedMaybe (Just a) = Just <$> rangeChecked a
{-# INLINE rangeCheckedMaybe #-}

throwErr :: (Member (Error InvalidInput) r) => String -> Sem r a
throwErr = throw . InvalidRange . fromString

checkBindingTeamPermissions ::
  ( Member (ErrorS 'NoBindingTeamMembers) r,
    Member (ErrorS 'NonBindingTeam) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamCollaboratorsSubsystem r,
    Member TeamStore r,
    Member TeamSubsystem r
  ) =>
  Local UserId ->
  Local UserId ->
  TeamId ->
  Sem r (Maybe TeamId)
checkBindingTeamPermissions lusr lother tid = do
  mTeamCollaborator <- internalGetTeamCollaborator tid (tUnqualified lusr)
  zusrMembership <- TeamSubsystem.internalGetTeamMember (tUnqualified lusr) tid
  case (mTeamCollaborator, zusrMembership) of
    (Just collaborator, Nothing) -> guardPerm CollaboratorPermission.ImplicitConnection collaborator
    (Nothing, mbMember) -> void $ permissionCheck CreateConversation mbMember
    (Just collaborator, Just member) ->
      unless (hasPermission collaborator CollaboratorPermission.ImplicitConnection || hasPermission member CreateConversation) $
        throwS @OperationDenied
  TeamStore.getTeamBinding tid >>= \case
    Just Binding -> do
      when (isJust zusrMembership) $
        verifyMembership tid (tUnqualified lusr)
      mOtherTeamCollaborator <- internalGetTeamCollaborator tid (tUnqualified lother)
      unless (isJust mOtherTeamCollaborator) $
        verifyMembership tid (tUnqualified lother)
      pure (Just tid)
    Just _ -> throwS @'NonBindingTeam
    Nothing -> throwS @'TeamNotFound
  where
    guardPerm p m =
      if m `hasPermission` p
        then pure ()
        else throwS @OperationDenied

verifyMembership ::
  ( Member (ErrorS 'NoBindingTeamMembers) r,
    Member TeamSubsystem r
  ) =>
  TeamId ->
  UserId ->
  Sem r ()
verifyMembership tid u = do
  membership <- TeamSubsystem.internalGetTeamMember u tid
  when (isNothing membership) $
    throwS @'NoBindingTeamMembers

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
