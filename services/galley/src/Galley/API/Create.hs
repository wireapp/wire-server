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
  ( createGroupConversation,
    createProteusSelfConversation,
    createOne2OneConversation,
    createConnectConversation,
  )
where

import Control.Lens hiding ((??))
import Data.Id
import Data.List1 (list1)
import Data.Misc (FutureWork (FutureWork))
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Data.Time
import qualified Data.UUID.Tagged as U
import Galley.API.Error
import Galley.API.MLS
import Galley.API.MLS.KeyPackage (nullKeyPackageRef)
import Galley.API.MLS.Keys (getMLSRemovalKey)
import Galley.API.Mapping
import Galley.API.One2One
import Galley.API.Util
import Galley.App (Env)
import qualified Galley.Data.Conversation as Data
import Galley.Data.Conversation.Types
import Galley.Effects
import qualified Galley.Effects.ConversationStore as E
import qualified Galley.Effects.GundeckAccess as E
import qualified Galley.Effects.MemberStore as E
import qualified Galley.Effects.TeamStore as E
import Galley.Intra.Push
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
import qualified Polysemy.TinyLog as P
import Wire.API.Conversation hiding (Conversation, Member)
import Wire.API.Conversation.Protocol
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.Error
import Wire.API.Routes.Public.Galley.Conversation
import Wire.API.Routes.Public.Util
import Wire.API.Team
import Wire.API.Team.LegalHold (LegalholdProtectee (LegalholdPlusFederationNotImplemented))
import Wire.API.Team.Member
import Wire.API.Team.Permission hiding (self)

----------------------------------------------------------------------------
-- Group conversations

-- | The public-facing endpoint for creating group conversations.
createGroupConversation ::
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member MemberStore r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (Error InternalError) r,
    Member (Error InvalidInput) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'NotConnected) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (ErrorS 'MLSNonEmptyMemberList) r,
    Member (ErrorS 'MLSMissingSenderClient) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input Env) r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Maybe ClientId ->
  ConnId ->
  NewConv ->
  Sem r ConversationResponse
createGroupConversation lusr mCreatorClient conn newConv = do
  (nc, fromConvSize -> allUsers) <- newRegularConversation lusr newConv
  let tinfo = newConvTeam newConv
  checkCreateConvPermissions lusr newConv tinfo allUsers
  ensureNoLegalholdConflicts allUsers

  case newConvProtocol newConv of
    ProtocolMLSTag -> do
      -- Here we fail early in order to notify users of this misconfiguration
      assertMLSEnabled
      unlessM (isJust <$> getMLSRemovalKey) $
        throw (InternalErrorWithDescription "No backend removal key is configured (See 'mlsPrivateKeyPaths' in galley's config). Refusing to create MLS conversation.")
    ProtocolProteusTag -> pure ()

  lcnv <- traverse (const E.createConversationId) lusr
  -- FUTUREWORK: Invoke the creating a conversation action only once
  -- protocol-specific validation is successful. Otherwise we might write the
  -- conversation to the database, and throw a validation error when the
  -- conversation is already in the database.
  conv <- E.createConversation lcnv nc

  -- set creator client for MLS conversations
  case (convProtocol conv, mCreatorClient) of
    (ProtocolProteus, _) -> pure ()
    (ProtocolMLS mlsMeta, Just c) ->
      E.addMLSClients (cnvmlsGroupId mlsMeta) (tUntagged lusr) (Set.singleton (c, nullKeyPackageRef))
    (ProtocolMLS _mlsMeta, Nothing) -> throwS @'MLSMissingSenderClient

  now <- input
  -- NOTE: We only send (conversation) events to members of the conversation
  notifyCreatedConversation (Just now) lusr (Just conn) conv
  conversationCreated lusr conv

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
    Member TeamStore r
  ) =>
  Local UserId ->
  NewConv ->
  Maybe ConvTeamInfo ->
  UserList UserId ->
  Sem r ()
checkCreateConvPermissions lusr _newConv Nothing allUsers =
  ensureConnected lusr allUsers
checkCreateConvPermissions lusr newConv (Just tinfo) allUsers = do
  let convTeam = cnvTeamId tinfo
  zusrMembership <- E.getTeamMember convTeam (tUnqualified lusr)
  void $ permissionCheck CreateConversation zusrMembership
  convLocalMemberships <- mapM (E.getTeamMember convTeam) (ulLocals allUsers)
  ensureAccessRole (accessRoles newConv) (zip (ulLocals allUsers) convLocalMemberships)
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
  when (length allUsers > 1) $ do
    void $ permissionCheck DoNotUseDeprecatedAddRemoveConvMember zusrMembership

  -- Team members are always considered to be connected, so we only check
  -- 'ensureConnected' for non-team-members.
  ensureConnectedToLocals (tUnqualified lusr) (notTeamMember (ulLocals allUsers) (catMaybes convLocalMemberships))
  ensureConnectedToRemotes lusr (ulRemotes allUsers)

----------------------------------------------------------------------------
-- Other kinds of conversations

createProteusSelfConversation ::
  forall r.
  ( Member ConversationStore r,
    Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Sem r ConversationResponse
createProteusSelfConversation lusr = do
  let lcnv = fmap Data.selfConv lusr
  c <- E.getConversation (tUnqualified lcnv)
  maybe (create lcnv) (conversationExisted lusr) c
  where
    create :: Local ConvId -> Sem r ConversationResponse
    create lcnv = do
      let nc =
            NewConversation
              { ncMetadata = (defConversationMetadata (tUnqualified lusr)) {cnvmType = SelfConv},
                ncUsers = ulFromLocals [toUserRole (tUnqualified lusr)],
                ncProtocol = ProtocolProteusTag
              }
      c <- E.createConversation lcnv nc
      conversationCreated lusr c

createOne2OneConversation ::
  forall r.
  ( Member BrigAccess r,
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
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input UTCTime) r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  ConnId ->
  NewConv ->
  Sem r ConversationResponse
createOne2OneConversation lusr zcon j = do
  let allUsers = newConvMembers lusr j
  other <- ensureOne (ulAll lusr allUsers)
  when (tUntagged lusr == other) $
    throwS @'InvalidOperation
  mtid <- case newConvTeam j of
    Just ti -> do
      foldQualified
        lusr
        (\lother -> checkBindingTeamPermissions lother (cnvTeamId ti))
        (const (pure Nothing))
        other
    Nothing -> ensureConnected lusr allUsers $> Nothing
  foldQualified
    lusr
    (createLegacyOne2OneConversationUnchecked lusr zcon (newConvName j) mtid)
    (createOne2OneConversationUnchecked lusr zcon (newConvName j) mtid . tUntagged)
    other
  where
    verifyMembership :: TeamId -> UserId -> Sem r ()
    verifyMembership tid u = do
      membership <- E.getTeamMember tid u
      when (isNothing membership) $
        throwS @'NoBindingTeamMembers
    checkBindingTeamPermissions ::
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
  ( Member ConversationStore r,
    Member (Error InternalError) r,
    Member (Error InvalidInput) r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input UTCTime) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  ConnId ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Local UserId ->
  Sem r ConversationResponse
createLegacyOne2OneConversationUnchecked self zcon name mtid other = do
  lcnv <- localOne2OneConvId self other
  let meta =
        (defConversationMetadata (tUnqualified self))
          { cnvmType = One2OneConv,
            cnvmTeam = mtid,
            cnvmName = fmap fromRange name
          }
  let nc =
        NewConversation
          { ncUsers = ulFromLocals (map (toUserRole . tUnqualified) [self, other]),
            ncProtocol = ProtocolProteusTag,
            ncMetadata = meta
          }
  mc <- E.getConversation (tUnqualified lcnv)
  case mc of
    Just c -> conversationExisted self c
    Nothing -> do
      c <- E.createConversation lcnv nc
      notifyCreatedConversation Nothing self (Just zcon) c
      conversationCreated self c

createOne2OneConversationUnchecked ::
  ( Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input UTCTime) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  ConnId ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Qualified UserId ->
  Sem r ConversationResponse
createOne2OneConversationUnchecked self zcon name mtid other = do
  let create =
        foldQualified
          self
          createOne2OneConversationLocally
          createOne2OneConversationRemotely
  create (one2OneConvId (tUntagged self) other) self zcon name mtid other

createOne2OneConversationLocally ::
  ( Member ConversationStore r,
    Member (Error InternalError) r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input UTCTime) r,
    Member P.TinyLog r
  ) =>
  Local ConvId ->
  Local UserId ->
  ConnId ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Qualified UserId ->
  Sem r ConversationResponse
createOne2OneConversationLocally lcnv self zcon name mtid other = do
  mc <- E.getConversation (tUnqualified lcnv)
  case mc of
    Just c -> conversationExisted self c
    Nothing -> do
      let meta =
            (defConversationMetadata (tUnqualified self))
              { cnvmType = One2OneConv,
                cnvmTeam = mtid,
                cnvmName = fmap fromRange name
              }
      let nc =
            NewConversation
              { ncMetadata = meta,
                ncUsers = fmap toUserRole (toUserList lcnv [tUntagged self, other]),
                ncProtocol = ProtocolProteusTag
              }
      c <- E.createConversation lcnv nc
      notifyCreatedConversation Nothing self (Just zcon) c
      conversationCreated self c

createOne2OneConversationRemotely ::
  Member (Error FederationError) r =>
  Remote ConvId ->
  Local UserId ->
  ConnId ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Qualified UserId ->
  Sem r ConversationResponse
createOne2OneConversationRemotely _ _ _ _ _ _ =
  throw FederationNotImplemented

createConnectConversation ::
  ( Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (Error InvalidInput) r,
    Member (ErrorS 'InvalidOperation) r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  Connect ->
  Sem r ConversationResponse
createConnectConversation lusr conn j = do
  lrecipient <- ensureLocal lusr (cRecipient j)
  n <- rangeCheckedMaybe (cName j)
  let meta =
        (defConversationMetadata (tUnqualified lusr))
          { cnvmType = ConnectConv,
            cnvmName = fmap fromRange n
          }
  lcnv <- localOne2OneConvId lusr lrecipient
  let nc =
        NewConversation
          { -- We add only one member, second one gets added later,
            -- when the other user accepts the connection request.
            ncUsers = ulFromLocals (map (toUserRole . tUnqualified) [lusr]),
            ncProtocol = ProtocolProteusTag,
            ncMetadata = meta
          }
  E.getConversation (tUnqualified lcnv)
    >>= maybe (create lcnv nc) (update n)
  where
    create lcnv nc = do
      c <- E.createConversation lcnv nc
      now <- input
      let e = Event (tUntagged lcnv) Nothing (tUntagged lusr) now (EdConnect j)
      notifyCreatedConversation Nothing lusr conn c
      for_ (newPushLocal ListComplete (tUnqualified lusr) (ConvEvent e) (recipient <$> Data.convLocalMembers c)) $ \p ->
        E.push1 $
          p
            & pushRoute .~ RouteDirect
            & pushConn .~ conn
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
          for_ (newPushLocal ListComplete (tUnqualified lusr) (ConvEvent e) (recipient <$> Data.convLocalMembers conv)) $ \p ->
            E.push1 $
              p
                & pushRoute .~ RouteDirect
                & pushConn .~ conn
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
    ProtocolProteusTag -> checkedConvSize o uncheckedUsers
    ProtocolMLSTag -> do
      unless (null uncheckedUsers) $ throwS @'MLSNonEmptyMemberList
      pure mempty
  let nc =
        NewConversation
          { ncMetadata =
              ConversationMetadata
                { cnvmType = RegularConv,
                  cnvmCreator = tUnqualified lusr,
                  cnvmAccess = access newConv,
                  cnvmAccessRoles = accessRoles newConv,
                  cnvmName = fmap fromRange (newConvName newConv),
                  cnvmMessageTimer = newConvMessageTimer newConv,
                  cnvmReceiptMode = newConvReceiptMode newConv,
                  cnvmTeam = fmap cnvTeamId (newConvTeam newConv)
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
  Sem r ConversationResponse
conversationCreated lusr cnv = Created <$> conversationView lusr cnv

notifyCreatedConversation ::
  ( Member (Error InternalError) r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input UTCTime) r,
    Member P.TinyLog r
  ) =>
  Maybe UTCTime ->
  Local UserId ->
  Maybe ConnId ->
  Data.Conversation ->
  Sem r ()
notifyCreatedConversation dtime lusr conn c = do
  now <- maybe input pure dtime
  -- FUTUREWORK: Handle failures in notifying so it does not abort half way
  -- through (either when notifying remotes or locals)
  --
  -- Ask remote server to store conversation membership and notify remote users
  -- of being added to a conversation
  registerRemoteConversationMemberships now (tDomain lusr) c
  -- Notify local users
  let remoteOthers = map remoteMemberToOther $ Data.convRemoteMembers c
      localOthers = map (localMemberToOther (tDomain lusr)) $ Data.convLocalMembers c
  E.push =<< mapM (toPush now remoteOthers localOthers) (Data.convLocalMembers c)
  where
    route
      | Data.convType c == RegularConv = RouteAny
      | otherwise = RouteDirect
    toPush t remoteOthers localOthers m = do
      let lconv = qualifyAs lusr (Data.convId c)
      c' <- conversationViewWithCachedOthers remoteOthers localOthers c (qualifyAs lusr (lmId m))
      let e = Event (tUntagged lconv) Nothing (tUntagged lusr) t (EdConversation c')
      pure $
        newPushLocal1 ListComplete (tUnqualified lusr) (ConvEvent e) (list1 (recipient m) [])
          & pushConn .~ conn
          & pushRoute .~ route

localOne2OneConvId ::
  Member (Error InvalidInput) r =>
  Local UserId ->
  Local UserId ->
  Sem r (Local ConvId)
localOne2OneConvId self other = do
  (x, y) <- toUUIDs (tUnqualified self) (tUnqualified other)
  pure . qualifyAs self $ Data.localOne2OneConvId x y

toUUIDs ::
  Member (Error InvalidInput) r =>
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

ensureOne :: Member (Error InvalidInput) r => [a] -> Sem r a
ensureOne [x] = pure x
ensureOne _ = throw (InvalidRange "One-to-one conversations can only have a single invited member")
