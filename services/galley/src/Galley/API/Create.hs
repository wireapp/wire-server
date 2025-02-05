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

-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.
module Galley.API.Create
  ( createGroupConversationUpToV3,
    createGroupConversation,
    createProteusSelfConversation,
    createOne2OneConversation,
    createConnectConversation,
  )
where

import Control.Error (headMay)
import Control.Lens hiding ((??))
import Data.Default
import Data.Id
import Data.Json.Util
import Data.Misc (FutureWork (FutureWork))
import Data.Qualified
import Data.Range
import Data.Set qualified as Set
import Data.Time
import Data.UUID.Tagged qualified as U
import Galley.API.Action
import Galley.API.Error
import Galley.API.MLS
import Galley.API.Mapping
import Galley.API.One2One
import Galley.API.Teams.Features.Get (getFeatureForTeam)
import Galley.API.Util
import Galley.App (Env)
import Galley.Data.Conversation qualified as Data
import Galley.Data.Conversation.Types
import Galley.Effects
import Galley.Effects.BrigAccess
import Galley.Effects.ConversationStore qualified as E
import Galley.Effects.FederatorAccess qualified as E
import Galley.Effects.MemberStore qualified as E
import Galley.Effects.TeamStore qualified as E
import Galley.Options
import Galley.Types.Conversations.Members
import Galley.Types.Teams (notTeamMember)
import Galley.Types.ToUserRole
import Galley.Types.UserList
import Galley.Validation
import Imports hiding ((\\))
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog qualified as P
import Wire.API.Conversation hiding (Conversation, Member)
import Wire.API.Conversation qualified as Public
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.Error
import Wire.API.FederationStatus
import Wire.API.Push.V2 qualified as PushV2
import Wire.API.Routes.Public.Galley.Conversation
import Wire.API.Routes.Public.Util
import Wire.API.Team
import Wire.API.Team.Feature
import Wire.API.Team.LegalHold (LegalholdProtectee (LegalholdPlusFederationNotImplemented))
import Wire.API.Team.Member
import Wire.API.Team.Permission hiding (self)
import Wire.API.User
import Wire.NotificationSubsystem

----------------------------------------------------------------------------
-- Group conversations

-- | The public-facing endpoint for creating group conversations in the client
-- API up to and including version 3.
createGroupConversationUpToV3 ::
  ( Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (Error InvalidInput) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'NotConnected) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (ErrorS 'MLSNonEmptyMemberList) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS ChannelsNotEnabled) r,
    Member (ErrorS NotAnMlsConversation) r,
    Member (Error UnreachableBackendsLegacy) r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member TeamStore r,
    Member P.TinyLog r,
    Member TeamFeatureStore r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  NewConv ->
  Sem r (ConversationResponse Public.Conversation)
createGroupConversationUpToV3 lusr conn newConv = mapError UnreachableBackendsLegacy $
  do
    conv <-
      createGroupConversationGeneric
        lusr
        conn
        newConv
    conversationCreated lusr conv

-- | The public-facing endpoint for creating group conversations in the client
-- API in version 4 and above.
createGroupConversation ::
  ( Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (Error InvalidInput) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (Error NonFederatingBackends) r,
    Member (ErrorS 'NotConnected) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (ErrorS 'MLSNonEmptyMemberList) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS ChannelsNotEnabled) r,
    Member (ErrorS NotAnMlsConversation) r,
    Member (Error UnreachableBackends) r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member TeamStore r,
    Member P.TinyLog r,
    Member TeamFeatureStore r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  NewConv ->
  Sem r CreateGroupConversationResponse
createGroupConversation lusr conn newConv = do
  let remoteDomains = void <$> snd (partitionQualified lusr $ newConv.newConvQualifiedUsers)
  enforceFederationProtocol (baseProtocolToProtocol newConv.newConvProtocol) remoteDomains
  checkFederationStatus (RemoteDomains $ Set.fromList remoteDomains)
  cnv <-
    createGroupConversationGeneric
      lusr
      conn
      newConv
  conv <- conversationView lusr cnv
  pure . GroupConversationCreated $
    CreateGroupConversation conv mempty

createGroupConversationGeneric ::
  ( Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (Error InvalidInput) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'NotConnected) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (ErrorS 'MLSNonEmptyMemberList) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS ChannelsNotEnabled) r,
    Member (ErrorS NotAnMlsConversation) r,
    Member (Error UnreachableBackends) r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member TeamStore r,
    Member P.TinyLog r,
    Member TeamFeatureStore r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  NewConv ->
  Sem r Conversation
createGroupConversationGeneric lusr conn newConv = do
  (nc, fromConvSize -> allUsers) <- newRegularConversation lusr newConv
  checkCreateConvPermissions lusr newConv newConv.newConvTeam allUsers
  ensureNoLegalholdConflicts allUsers

  when (newConvProtocol newConv == BaseProtocolMLSTag) $ do
    -- Here we fail early in order to notify users of this misconfiguration
    assertMLSEnabled

  lcnv <- traverse (const E.createConversationId) lusr
  conv <- E.createConversation lcnv nc
  -- NOTE: We only send (conversation) events to members of the conversation
  notifyCreatedConversation lusr conn conv
  E.getConversation (tUnqualified lcnv)
    >>= note (BadConvState (tUnqualified lcnv))

ensureNoLegalholdConflicts ::
  ( Member (ErrorS 'MissingLegalholdConsent) r,
    Member (Input Opts) r,
    Member LegalHoldStore r,
    Member TeamStore r
  ) =>
  UserList UserId ->
  Sem r ()
ensureNoLegalholdConflicts (UserList locals remotes) = do
  let FutureWork _remotes = FutureWork @'LegalholdPlusFederationNotImplemented remotes
  whenM (anyLegalholdActivated locals) $
    unlessM (allLegalholdConsentGiven locals) $
      throwS @'MissingLegalholdConsent

checkCreateConvPermissions ::
  ( Member BrigAccess r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'NotConnected) r,
    Member (ErrorS ChannelsNotEnabled) r,
    Member (ErrorS NotAnMlsConversation) r,
    Member TeamStore r,
    Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  Local UserId ->
  NewConv ->
  Maybe ConvTeamInfo ->
  UserList UserId ->
  Sem r ()
checkCreateConvPermissions lusr newConv Nothing allUsers = do
  when (newConv.newConvGroupConvType == Channel) $ throwS @OperationDenied
  activated <- listToMaybe <$> lookupActivatedUsers [tUnqualified lusr]
  void $ noteS @OperationDenied activated
  -- an external partner is not allowed to create group conversations (except 1:1 team conversations that are handled below)
  tm <- getTeamMember (tUnqualified lusr) Nothing
  for_ tm $
    permissionCheck AddRemoveConvMember . Just
  ensureConnected lusr allUsers
checkCreateConvPermissions lusr newConv (Just tinfo) allUsers = do
  let convTeam = cnvTeamId tinfo
  zusrMembership <- getTeamMember (tUnqualified lusr) (Just convTeam)
  case newConv.newConvGroupConvType of
    Channel -> do
      ensureCreateChannelPermissions tinfo.cnvTeamId zusrMembership
    GroupConversation -> do
      void $ permissionCheck CreateConversation zusrMembership
      -- In teams we don't have 1:1 conversations, only regular conversations. We want
      -- users without the 'AddRemoveConvMember' permission to still be able to create
      -- regular conversations, therefore we check for 'AddRemoveConvMember' only if
      -- there are going to be more than two users in the conversation.
      -- FUTUREWORK: We keep this permission around because not doing so will break backwards
      -- compatibility in the sense that the team role 'partners' would be able to create group
      -- conversations (which they should not be able to).
      -- Not sure at the moment how to best solve this but it is unlikely
      -- we can ever get rid of the team permission model anyway - the only thing I can
      -- think of is that 'partners' can create convs but not be admins...
      -- this only applies to proteus conversations, because in MLS we have proper 1:1 conversations,
      -- so we don't allow an external partner to create an MLS group conversation at all
      when (length allUsers > 1 || newConv.newConvProtocol == BaseProtocolMLSTag) $ do
        void $ permissionCheck AddRemoveConvMember zusrMembership

  convLocalMemberships <- mapM (E.getTeamMember convTeam) (ulLocals allUsers)
  ensureAccessRole (accessRoles newConv) (zip (ulLocals allUsers) convLocalMemberships)
  -- Team members are always considered to be connected, so we only check
  -- 'ensureConnected' for non-team-members.
  ensureConnectedToLocals (tUnqualified lusr) (notTeamMember (ulLocals allUsers) (catMaybes convLocalMemberships))
  ensureConnectedToRemotes lusr (ulRemotes allUsers)
  where
    ensureCreateChannelPermissions ::
      forall r.
      ( Member (ErrorS OperationDenied) r,
        Member (Input Opts) r,
        Member TeamFeatureStore r,
        Member (ErrorS NotATeamMember) r,
        Member (ErrorS ChannelsNotEnabled) r,
        Member (ErrorS NotAnMlsConversation) r
      ) =>
      TeamId ->
      Maybe TeamMember ->
      Sem r ()
    ensureCreateChannelPermissions tid (Just tm) = do
      channelsConf <- getFeatureForTeam @ChannelsConfig tid
      when (channelsConf.status == FeatureStatusDisabled) $ throwS @ChannelsNotEnabled
      when (newConv.newConvProtocol /= BaseProtocolMLSTag) $ throwS @NotAnMlsConversation
      case channelsConf.config.allowedToCreateChannels of
        Everyone -> pure ()
        TeamMembers -> void $ permissionCheck AddRemoveConvMember $ Just tm
        Admins -> unless (isAdminOrOwner (tm ^. permissions)) $ throwS @OperationDenied
    ensureCreateChannelPermissions _ Nothing = do
      throwS @NotATeamMember

getTeamMember :: (Member TeamStore r) => UserId -> Maybe TeamId -> Sem r (Maybe TeamMember)
getTeamMember uid (Just tid) = E.getTeamMember tid uid
getTeamMember uid Nothing = E.getUserTeams uid >>= maybe (pure Nothing) (flip E.getTeamMember uid) . headMay

----------------------------------------------------------------------------
-- Other kinds of conversations

createProteusSelfConversation ::
  forall r.
  ( Member ConversationStore r,
    Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Sem r (ConversationResponse Public.Conversation)
createProteusSelfConversation lusr = do
  let lcnv = fmap Data.selfConv lusr
  c <- E.getConversation (tUnqualified lcnv)
  maybe (create lcnv) (conversationExisted lusr) c
  where
    create :: Local ConvId -> Sem r (ConversationResponse Public.Conversation)
    create lcnv = do
      let nc =
            NewConversation
              { ncMetadata = (defConversationMetadata (Just (tUnqualified lusr))) {cnvmType = SelfConv},
                ncUsers = ulFromLocals [toUserRole (tUnqualified lusr)],
                ncProtocol = BaseProtocolProteusTag
              }
      c <- E.createConversation lcnv nc
      conversationCreated lusr c

createOne2OneConversation ::
  ( Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (Error InvalidInput) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'NonBindingTeam) r,
    Member (ErrorS 'NoBindingTeamMembers) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'NotConnected) r,
    Member (Error UnreachableBackendsLegacy) r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  ConnId ->
  NewOne2OneConv ->
  Sem r (ConversationResponse Public.Conversation)
createOne2OneConversation lusr zcon j =
  mapError @UnreachableBackends @UnreachableBackendsLegacy UnreachableBackendsLegacy $ do
    let allUsers = newOne2OneConvMembers lusr j
    other <- ensureOne (ulAll lusr allUsers)
    when (tUntagged lusr == other) $
      throwS @'InvalidOperation
    mtid <- case j.team of
      Just ti -> do
        foldQualified
          lusr
          (\lother -> checkBindingTeamPermissions lother (cnvTeamId ti))
          (const (pure Nothing))
          other
      Nothing -> ensureConnected lusr allUsers $> Nothing
    foldQualified
      lusr
      (createLegacyOne2OneConversationUnchecked lusr zcon j.name mtid)
      (createOne2OneConversationUnchecked lusr zcon j.name mtid . tUntagged)
      other
  where
    verifyMembership ::
      ( Member (ErrorS 'NoBindingTeamMembers) r,
        Member TeamStore r
      ) =>
      TeamId ->
      UserId ->
      Sem r ()
    verifyMembership tid u = do
      membership <- E.getTeamMember tid u
      when (isNothing membership) $
        throwS @'NoBindingTeamMembers
    checkBindingTeamPermissions ::
      ( Member (ErrorS 'NoBindingTeamMembers) r,
        Member (ErrorS 'NonBindingTeam) r,
        Member (ErrorS 'NotATeamMember) r,
        Member (ErrorS OperationDenied) r,
        Member (ErrorS 'TeamNotFound) r,
        Member TeamStore r
      ) =>
      Local UserId ->
      TeamId ->
      Sem r (Maybe TeamId)
    checkBindingTeamPermissions lother tid = do
      zusrMembership <- E.getTeamMember tid (tUnqualified lusr)
      void $ permissionCheck CreateConversation zusrMembership
      E.getTeamBinding tid >>= \case
        Just Binding -> do
          verifyMembership tid (tUnqualified lusr)
          verifyMembership tid (tUnqualified lother)
          pure (Just tid)
        Just _ -> throwS @'NonBindingTeam
        Nothing -> throwS @'TeamNotFound

createLegacyOne2OneConversationUnchecked ::
  ( Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (Error InvalidInput) r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  ConnId ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Local UserId ->
  Sem r (ConversationResponse Public.Conversation)
createLegacyOne2OneConversationUnchecked self zcon name mtid other = do
  lcnv <- localOne2OneConvId self other
  let meta =
        (defConversationMetadata (Just (tUnqualified self)))
          { cnvmType = One2OneConv,
            cnvmTeam = mtid,
            cnvmName = fmap fromRange name
          }
  let nc =
        NewConversation
          { ncUsers = ulFromLocals (map (toUserRole . tUnqualified) [self, other]),
            ncProtocol = BaseProtocolProteusTag,
            ncMetadata = meta
          }
  mc <- E.getConversation (tUnqualified lcnv)
  case mc of
    Just c -> conversationExisted self c
    Nothing -> do
      c <- E.createConversation lcnv nc
      runError @UnreachableBackends (notifyCreatedConversation self (Just zcon) c)
        >>= \case
          Left _ -> do
            throw . InternalErrorWithDescription $
              "A one-to-one conversation on one backend cannot involve unreachable backends"
          Right () -> conversationCreated self c

createOne2OneConversationUnchecked ::
  ( Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (Error UnreachableBackends) r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  ConnId ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Qualified UserId ->
  Sem r (ConversationResponse Public.Conversation)
createOne2OneConversationUnchecked self zcon name mtid other = do
  let create =
        foldQualified
          self
          createOne2OneConversationLocally
          createOne2OneConversationRemotely
  create (one2OneConvId BaseProtocolProteusTag (tUntagged self) other) self zcon name mtid other

createOne2OneConversationLocally ::
  ( Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (Error UnreachableBackends) r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member P.TinyLog r
  ) =>
  Local ConvId ->
  Local UserId ->
  ConnId ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Qualified UserId ->
  Sem r (ConversationResponse Public.Conversation)
createOne2OneConversationLocally lcnv self zcon name mtid other = do
  mc <- E.getConversation (tUnqualified lcnv)
  case mc of
    Just c -> conversationExisted self c
    Nothing -> do
      let meta =
            (defConversationMetadata (Just (tUnqualified self)))
              { cnvmType = One2OneConv,
                cnvmTeam = mtid,
                cnvmName = fmap fromRange name
              }
      let nc =
            NewConversation
              { ncMetadata = meta,
                ncUsers = fmap toUserRole (toUserList lcnv [tUntagged self, other]),
                ncProtocol = BaseProtocolProteusTag
              }
      c <- E.createConversation lcnv nc
      notifyCreatedConversation self (Just zcon) c
      conversationCreated self c

createOne2OneConversationRemotely ::
  (Member (Error FederationError) r) =>
  Remote ConvId ->
  Local UserId ->
  ConnId ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Qualified UserId ->
  Sem r (ConversationResponse Public.Conversation)
createOne2OneConversationRemotely _ _ _ _ _ _ =
  throw FederationNotImplemented

createConnectConversation ::
  ( Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (Error InvalidInput) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (Error UnreachableBackends) r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  Connect ->
  Sem r (ConversationResponse Public.Conversation)
createConnectConversation lusr conn j = do
  lrecipient <- ensureLocal lusr (cRecipient j)
  n <- rangeCheckedMaybe (cName j)
  let meta =
        (defConversationMetadata (Just (tUnqualified lusr)))
          { cnvmType = ConnectConv,
            cnvmName = fmap fromRange n
          }
  lcnv <- localOne2OneConvId lusr lrecipient
  let nc =
        NewConversation
          { -- We add only one member, second one gets added later,
            -- when the other user accepts the connection request.
            ncUsers = ulFromLocals ([(toUserRole . tUnqualified) lusr]),
            ncProtocol = BaseProtocolProteusTag,
            ncMetadata = meta
          }
  E.getConversation (tUnqualified lcnv)
    >>= maybe (create lcnv nc) (update n)
  where
    create lcnv nc = do
      c <- E.createConversation lcnv nc
      now <- input
      let e = Event (tUntagged lcnv) Nothing (tUntagged lusr) now (EdConnect j)
      notifyCreatedConversation lusr conn c
      pushNotifications
        [ newPushLocal (tUnqualified lusr) (toJSONObject e) (localMemberToRecipient <$> Data.convLocalMembers c) (isCellsEvent $ evtType e)
            & pushRoute .~ PushV2.RouteDirect
            & pushConn .~ conn
        ]
      conversationCreated lusr c
    update n conv = do
      let mems = Data.convLocalMembers conv
       in conversationExisted lusr
            =<< if tUnqualified lusr `isMember` mems
              then -- we already were in the conversation, maybe also other
                connect n conv
              else do
                let lcid = qualifyAs lusr (Data.convId conv)
                mm <- E.createMember lcid lusr
                let conv' =
                      conv
                        { Data.convLocalMembers = Data.convLocalMembers conv <> toList mm
                        }
                if null mems
                  then do
                    -- the conversation was empty
                    connect n conv'
                  else do
                    -- we were not in the conversation, but someone else
                    conv'' <- acceptOne2One lusr conv' conn
                    if Data.convType conv'' == ConnectConv
                      then connect n conv''
                      else pure conv''
    connect n conv
      | Data.convType conv == ConnectConv = do
          let lcnv = qualifyAs lusr (Data.convId conv)
          n' <- case n of
            Just x -> do
              E.setConversationName (Data.convId conv) x
              pure . Just $ fromRange x
            Nothing -> pure $ Data.convName conv
          t <- input
          let e = Event (tUntagged lcnv) Nothing (tUntagged lusr) t (EdConnect j)
          pushNotifications
            [ newPushLocal (tUnqualified lusr) (toJSONObject e) (localMemberToRecipient <$> Data.convLocalMembers conv) (isCellsEvent $ evtType e)
                & pushRoute .~ PushV2.RouteDirect
                & pushConn .~ conn
            ]
          pure $ Data.convSetName n' conv
      | otherwise = pure conv

--------------------------------------------------------------------------------
-- Conversation creation records

-- | Return a 'NewConversation' record suitable for creating a group conversation.
newRegularConversation ::
  ( Member (ErrorS 'MLSNonEmptyMemberList) r,
    Member (Error InvalidInput) r,
    Member (Input Opts) r
  ) =>
  Local UserId ->
  NewConv ->
  Sem r (NewConversation, ConvSizeChecked UserList UserId)
newRegularConversation lusr newConv = do
  o <- input
  let uncheckedUsers = newConvMembers lusr newConv
  users <- case newConvProtocol newConv of
    BaseProtocolProteusTag -> checkedConvSize o uncheckedUsers
    BaseProtocolMLSTag -> do
      unless (null uncheckedUsers) $ throwS @'MLSNonEmptyMemberList
      pure mempty
  let nc =
        NewConversation
          { ncMetadata =
              ConversationMetadata
                { cnvmType = RegularConv,
                  cnvmCreator = Just (tUnqualified lusr),
                  cnvmAccess = access newConv,
                  cnvmAccessRoles = accessRoles newConv,
                  cnvmName = fmap fromRange (newConvName newConv),
                  cnvmMessageTimer = newConvMessageTimer newConv,
                  cnvmReceiptMode = newConvReceiptMode newConv,
                  cnvmTeam = fmap cnvTeamId (newConvTeam newConv),
                  cnvmGroupConvType = Just newConv.newConvGroupConvType,
                  cnvmCellsState = def
                },
            ncUsers = ulAddLocal (toUserRole (tUnqualified lusr)) (fmap (,newConvUsersRole newConv) (fromConvSize users)),
            ncProtocol = newConvProtocol newConv
          }
  pure (nc, users)

-------------------------------------------------------------------------------
-- Helpers

conversationCreated ::
  ( Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Data.Conversation ->
  Sem r (ConversationResponse Public.Conversation)
conversationCreated lusr cnv = Created <$> conversationView lusr cnv

-- | The return set contains all the remote users that could not be contacted.
-- Consequently, the unreachable users are not added to the member list. This
-- behavior might be changed later on when a message/event queue per remote
-- backend is implemented.
notifyCreatedConversation ::
  ( Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (Error UnreachableBackends) r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member BackendNotificationQueueAccess r,
    Member (Input UTCTime) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  Data.Conversation ->
  Sem r ()
notifyCreatedConversation lusr conn c = do
  now <- input
  -- Ask remote servers to store conversation membership and notify remote users
  -- of being added to a conversation
  registerRemoteConversationMemberships now lusr (qualifyAs lusr c)
  unless (null (Data.convRemoteMembers c)) $
    unlessM E.isFederationConfigured $
      throw FederationNotConfigured

  -- Notify local users
  pushNotifications =<< mapM (toPush now) (Data.convLocalMembers c)
  where
    route
      | Data.convType c == RegularConv = PushV2.RouteAny
      | otherwise = PushV2.RouteDirect
    toPush t m = do
      let remoteOthers = remoteMemberToOther <$> Data.convRemoteMembers c
          localOthers = map (localMemberToOther (tDomain lusr)) $ Data.convLocalMembers c
          lconv = qualifyAs lusr (Data.convId c)
      c' <- conversationViewWithCachedOthers remoteOthers localOthers c (qualifyAs lusr (lmId m))
      let e = Event (tUntagged lconv) Nothing (tUntagged lusr) t (EdConversation c')
      pure $
        newPushLocal (tUnqualified lusr) (toJSONObject e) [localMemberToRecipient m] (isCellsEvent $ evtType e)
          & pushConn .~ conn
          & pushRoute .~ route

localOne2OneConvId ::
  (Member (Error InvalidInput) r) =>
  Local UserId ->
  Local UserId ->
  Sem r (Local ConvId)
localOne2OneConvId self other = do
  (x, y) <- toUUIDs (tUnqualified self) (tUnqualified other)
  pure . qualifyAs self $ Data.localOne2OneConvId x y

toUUIDs ::
  (Member (Error InvalidInput) r) =>
  UserId ->
  UserId ->
  Sem r (U.UUID U.V4, U.UUID U.V4)
toUUIDs a b = do
  a' <- U.fromUUID (toUUID a) & note InvalidUUID4
  b' <- U.fromUUID (toUUID b) & note InvalidUUID4
  pure (a', b')

accessRoles :: NewConv -> Set AccessRole
accessRoles b = fromMaybe Data.defRole (newConvAccessRoles b)

access :: NewConv -> [Access]
access a = case Set.toList (newConvAccess a) of
  [] -> Data.defRegularConvAccess
  (x : xs) -> x : xs

newConvMembers :: Local x -> NewConv -> UserList UserId
newConvMembers loc body =
  UserList (newConvUsers body) []
    <> toUserList loc (newConvQualifiedUsers body)

newOne2OneConvMembers :: Local x -> NewOne2OneConv -> UserList UserId
newOne2OneConvMembers loc body =
  UserList body.users []
    <> toUserList loc body.qualifiedUsers

ensureOne :: (Member (Error InvalidInput) r) => [a] -> Sem r a
ensureOne [x] = pure x
ensureOne _ = throw (InvalidRange "One-to-one conversations can only have a single invited member")
