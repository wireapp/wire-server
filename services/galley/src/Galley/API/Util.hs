{-# LANGUAGE RecordWildCards #-}

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

module Galley.API.Util where

import Control.Lens (to, view, (^.))
import Control.Monad.Extra (allM, anyM)
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import Data.Code qualified as Code
import Data.Default
import Data.Domain (Domain)
import Data.Id as Id
import Data.Json.Util
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus)
import Data.List.Extra (chunksOf, nubOrd)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Misc (PlainTextPassword6, PlainTextPassword8)
import Data.Qualified
import Data.Set qualified as Set
import Data.Singletons
import Data.Text qualified as T
import Data.Time
import Galley.API.Cells
import Galley.API.Error
import Galley.API.Mapping
import Galley.Data.Conversation qualified as Data
import Galley.Data.Services (BotMember, newBotMember)
import Galley.Data.Types qualified as DataTypes
import Galley.Effects
import Galley.Effects.BackendNotificationQueueAccess
import Galley.Effects.BrigAccess
import Galley.Effects.ClientStore
import Galley.Effects.CodeStore
import Galley.Effects.ConversationStore
import Galley.Effects.ExternalAccess
import Galley.Effects.FederatorAccess
import Galley.Effects.LegalHoldStore
import Galley.Effects.MemberStore
import Galley.Effects.TeamStore
import Galley.Options
import Galley.Types.Clients (Clients, fromUserClients)
import Galley.Types.Conversations.Members
import Galley.Types.Conversations.Roles
import Galley.Types.Teams
import Galley.Types.UserList
import Imports hiding (forkIO)
import Network.AMQP qualified as Q
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog qualified as P
import Wire.API.Connection
import Wire.API.Conversation hiding (Member, cnvAccess, cnvAccessRoles, cnvName, cnvType)
import Wire.API.Conversation qualified as Public
import Wire.API.Conversation.Action
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.Federation.Version
import Wire.API.MLS.Group.Serialisation
import Wire.API.Push.V2 qualified as PushV2
import Wire.API.Routes.Public.Galley.Conversation
import Wire.API.Routes.Public.Util
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.API.Team.Member qualified as Mem
import Wire.API.Team.Role
import Wire.API.User hiding (userId)
import Wire.API.User.Auth.ReAuth
import Wire.API.VersionInfo
import Wire.HashPassword (HashPassword)
import Wire.HashPassword qualified as HashPassword
import Wire.NotificationSubsystem
import Wire.RateLimit

data NoChanges = NoChanges

ensureAccessRole ::
  ( Member BrigAccess r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'ConvAccessDenied) r
  ) =>
  Set Public.AccessRole ->
  [(UserId, Maybe TeamMember)] ->
  Sem r ()
ensureAccessRole roles users = do
  when (Set.null roles) $ throwS @'ConvAccessDenied
  unless (NonTeamMemberAccessRole `Set.member` roles) $
    when (any (isNothing . snd) users) $
      throwS @'NotATeamMember
  unless (Set.fromList [GuestAccessRole, ServiceAccessRole] `Set.isSubsetOf` roles) $ do
    activated <- lookupActivatedUsers (fst <$> users)
    let guestsExist = length activated /= length users
    unless (not guestsExist || GuestAccessRole `Set.member` roles) $ throwS @'ConvAccessDenied
    let botsExist = any (isJust . userService) activated
    unless (not botsExist || ServiceAccessRole `Set.member` roles) $ throwS @'ConvAccessDenied

-- | Check that the given user is either part of the same team as the other
-- users OR that there is a connection.
ensureConnectedOrSameTeam ::
  ( Member BrigAccess r,
    Member (ErrorS 'NotConnected) r,
    Member TeamStore r
  ) =>
  Local UserId ->
  [Qualified UserId] ->
  Sem r ()
ensureConnectedOrSameTeam lusr others = do
  let (locals, remotes) = partitionQualified lusr others
  ensureConnectedToLocalsOrSameTeam lusr locals
  ensureConnectedToRemotes lusr remotes

-- | Check that the given user is either part of the same team(s) as the other
-- users OR that there is a connection.
--
-- Team members are always considered connected, so we only check 'ensureConnected'
-- for non-team-members of the _given_ user
ensureConnectedToLocalsOrSameTeam ::
  ( Member BrigAccess r,
    Member (ErrorS 'NotConnected) r,
    Member TeamStore r
  ) =>
  Local UserId ->
  [UserId] ->
  Sem r ()
ensureConnectedToLocalsOrSameTeam _ [] = pure ()
ensureConnectedToLocalsOrSameTeam (tUnqualified -> u) uids = do
  uTeams <- getUserTeams u
  -- We collect all the relevant uids from same teams as the origin user
  sameTeamUids <- forM uTeams $ \team ->
    fmap (view Mem.userId) <$> selectTeamMembers team uids
  -- Do not check connections for users that are on the same team
  ensureConnectedToLocals u (uids \\ join sameTeamUids)

-- | Check that the user is connected to everybody else.
--
-- The connection has to be bidirectional (e.g. if A connects to B and later
-- B blocks A, the status of A-to-B is still 'Accepted' but it doesn't mean
-- that they are connected).
ensureConnected ::
  ( Member BrigAccess r,
    Member (ErrorS 'NotConnected) r
  ) =>
  Local UserId ->
  UserList UserId ->
  Sem r ()
ensureConnected self others = do
  ensureConnectedToLocals (tUnqualified self) (ulLocals others)
  ensureConnectedToRemotes self (ulRemotes others)

ensureConnectedToLocals ::
  ( Member (ErrorS 'NotConnected) r,
    Member BrigAccess r
  ) =>
  UserId ->
  [UserId] ->
  Sem r ()
ensureConnectedToLocals _ [] = pure ()
ensureConnectedToLocals u uids = do
  (connsFrom, connsTo) <-
    getConnectionsUnqualifiedBidi [u] uids (Just Accepted) (Just Accepted)
  unless (length connsFrom == length uids && length connsTo == length uids) $
    throwS @'NotConnected

ensureConnectedToRemotes ::
  ( Member BrigAccess r,
    Member (ErrorS 'NotConnected) r
  ) =>
  Local UserId ->
  [Remote UserId] ->
  Sem r ()
ensureConnectedToRemotes _ [] = pure ()
ensureConnectedToRemotes u remotes = do
  acceptedConns <- getConnections [tUnqualified u] (Just $ map tUntagged remotes) (Just Accepted)
  when (length acceptedConns /= length remotes) $
    throwS @'NotConnected

ensureReAuthorised ::
  ( Member BrigAccess r,
    Member (Error AuthenticationError) r
  ) =>
  UserId ->
  Maybe PlainTextPassword6 ->
  Maybe Code.Value ->
  Maybe VerificationAction ->
  Sem r ()
ensureReAuthorised u secret mbAction mbCode =
  reauthUser u (ReAuthUser secret mbAction mbCode) >>= fromEither

ensureChannelAndTeamAdmin :: (Member (ErrorS 'ConvNotFound) r) => Data.Conversation -> TeamMember -> Sem r ()
ensureChannelAndTeamAdmin conv tm = do
  unless (conv.convMetadata.cnvmGroupConvType == Just Channel && isAdminOrOwner (tm ^. permissions)) $
    throwS @'ConvNotFound

-- | Given a member in a conversation, check if the given action
-- is permitted. If the user does not have the given permission, or if it has a
-- custom role, throw 'ActionDenied'.
ensureActionAllowed ::
  forall (action :: Action) mem r.
  (IsConvMember mem, Member (ErrorS ('ActionDenied action)) r) =>
  Sing action ->
  mem ->
  Sem r ()
ensureActionAllowed action self = case isActionAllowed (fromSing action) (convMemberRole self) of
  Just True -> pure ()
  Just False -> throwS @('ActionDenied action)
  -- Actually, this will "never" happen due to the
  -- fact that there can be no custom roles at the moment
  Nothing -> throwS @('ActionDenied action)

-- | Ensure that the conversation is a group conversation which includes channels
ensureGroupConversation :: (Member (ErrorS 'InvalidOperation) r) => Data.Conversation -> Sem r ()
ensureGroupConversation conv = do
  let ty = Data.convType conv
  when (ty /= RegularConv) $ throwS @'InvalidOperation

-- | Ensure that the set of actions provided are not "greater" than the user's
--   own. This is used to ensure users cannot "elevate" allowed actions
--   This function needs to be review when custom roles are introduced since only
--   custom roles can cause `roleNameToActions` to return a Nothing
ensureConvRoleNotElevated ::
  (IsConvMember mem, Member (ErrorS 'InvalidAction) r) =>
  mem ->
  RoleName ->
  Sem r ()
ensureConvRoleNotElevated origMember targetRole = do
  case (roleNameToActions targetRole, roleNameToActions (convMemberRole origMember)) of
    (Just targetActions, Just memberActions) ->
      unless (Set.isSubsetOf targetActions memberActions) $
        throwS @'InvalidAction
    (_, _) ->
      -- custom roles not supported
      throwS @'InvalidAction

checkGroupIdSupport ::
  ( Member (ErrorS GroupIdVersionNotSupported) r,
    Member FederatorAccess r
  ) =>
  Local x ->
  Data.Conversation ->
  ConversationJoin ->
  Sem r ()
checkGroupIdSupport loc conv joinAction = void $ runMaybeT $ do
  -- if it is an MLS conversation
  d <- MaybeT (pure (getMLSData conv))

  -- if the group ID version is not 1
  (v, _) <-
    either (\_ -> lift (throwS @GroupIdVersionNotSupported)) pure $
      groupIdToConv
        d.cnvmlsGroupId
  guard $ v /= Just GroupIdVersion1

  -- check that each remote backend is compatible with group ID version >= 2
  let (_, remoteUsers) = partitionQualified loc joinAction.users
  lift
    . (failOnFirstError <=< runFederatedConcurrentlyEither @_ @Brig remoteUsers)
    $ \_ -> do
      guardVersion $ \fedV -> fedV >= groupIdFedVersion GroupIdVersion2
  where
    failOnFirstError :: (Member (ErrorS GroupIdVersionNotSupported) r) => [Either e x] -> Sem r ()
    failOnFirstError = traverse_ $ either (\_ -> throwS @GroupIdVersionNotSupported) pure

getMLSData :: Data.Conversation -> Maybe ConversationMLSData
getMLSData conv = case Data.convProtocol conv of
  ProtocolMLS d -> Just d
  ProtocolMixed d -> Just d
  ProtocolProteus -> Nothing

-- | Same as 'permissionCheck', but for a statically known permission.
permissionCheckS ::
  forall perm (p :: perm) r.
  ( SingKind perm,
    IsPerm (Demote perm),
    ( Member (ErrorS (PermError p)) r,
      Member (ErrorS 'NotATeamMember) r
    )
  ) =>
  Sing p ->
  Maybe TeamMember ->
  Sem r TeamMember
permissionCheckS p =
  \case
    Just m -> do
      if m `hasPermission` fromSing p
        then pure m
        else throwS @(PermError p)
    -- FUTUREWORK: factor `noteS` out of this function.
    Nothing -> throwS @'NotATeamMember

-- | If a team member is not given throw 'notATeamMember'; if the given team
-- member does not have the given permission, throw 'operationDenied'.
-- Otherwise, return the team member.
permissionCheck ::
  ( IsPerm perm,
    ( Member (ErrorS OperationDenied) r,
      Member (ErrorS 'NotATeamMember) r
    )
  ) =>
  perm ->
  Maybe TeamMember ->
  Sem r TeamMember
-- FUTUREWORK: factor `noteS` out of this function.
permissionCheck p = \case
  Just m -> do
    if m `hasPermission` p
      then pure m
      else throwS @OperationDenied
  -- FUTUREWORK: factor `noteS` out of this function.
  Nothing -> throwS @'NotATeamMember

assertTeamExists ::
  ( Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r
  ) =>
  TeamId ->
  Sem r ()
assertTeamExists tid = do
  teamExists <- isJust <$> getTeam tid
  if teamExists
    then pure ()
    else throwS @'TeamNotFound

assertOnTeam ::
  ( Member (ErrorS 'NotATeamMember) r,
    Member TeamStore r
  ) =>
  UserId ->
  TeamId ->
  Sem r ()
assertOnTeam uid tid =
  getTeamMember tid uid >>= \case
    Nothing -> throwS @'NotATeamMember
    Just _ -> pure ()

-- | Try to accept a 1-1 conversation, promoting connect conversations as appropriate.
acceptOne2One ::
  ( Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (Error InternalError) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member NotificationSubsystem r
  ) =>
  Local UserId ->
  Data.Conversation ->
  Maybe ConnId ->
  Sem r Data.Conversation
acceptOne2One lusr conv conn = do
  let lcid = qualifyAs lusr cid
  case Data.convType conv of
    One2OneConv ->
      if tUnqualified lusr `isMember` mems
        then pure conv
        else do
          mm <- createMember lcid lusr
          pure conv {Data.convLocalMembers = mems <> toList mm}
    ConnectConv -> case mems of
      [_, _] | tUnqualified lusr `isMember` mems -> promote
      [_, _] -> throwS @'ConvNotFound
      _ -> do
        when (length mems > 2) $
          throw . BadConvState $
            cid
        now <- input
        mm <- createMember lcid lusr
        let e = memberJoinEvent lusr (tUntagged lcid) now mm []
        conv' <- if isJust (find ((tUnqualified lusr /=) . lmId) mems) then promote else pure conv
        let mems' = mems <> toList mm
            p =
              def
                { origin = Just (tUnqualified lusr),
                  json = toJSONObject e,
                  recipients = localMemberToRecipient <$> mems'
                }
        pushNotifications [p {conn, route = PushV2.RouteDirect}]
        pure conv' {Data.convLocalMembers = mems'}
    _ -> throwS @'InvalidOperation
  where
    cid = Data.convId conv
    mems = Data.convLocalMembers conv
    promote = do
      acceptConnectConversation cid
      pure $ Data.convSetType One2OneConv conv

localMemberToRecipient :: LocalMember -> Recipient
localMemberToRecipient = userRecipient . lmId

userRecipient :: UserId -> Recipient
userRecipient u = Recipient u PushV2.RecipientClientsAll

memberJoinEvent ::
  Local UserId ->
  Qualified ConvId ->
  UTCTime ->
  [LocalMember] ->
  [RemoteMember] ->
  Event
memberJoinEvent lorig qconv t lmems rmems =
  Event qconv Nothing (tUntagged lorig) t Nothing $
    EdMembersJoin (MembersJoin (map localToSimple lmems <> map remoteToSimple rmems) InternalAdd)
  where
    localToSimple u = SimpleMember (tUntagged (qualifyAs lorig (lmId u))) (lmConvRoleName u)
    remoteToSimple u = SimpleMember (tUntagged (rmId u)) (rmConvRoleName u)

convDeleteMembers ::
  (Member MemberStore r) =>
  UserList UserId ->
  Data.Conversation ->
  Sem r Data.Conversation
convDeleteMembers ul conv = do
  deleteMembers (Data.convId conv) ul
  let locals = Set.fromList (ulLocals ul)
      remotes = Set.fromList (ulRemotes ul)
  -- update in-memory view of the conversation
  pure $
    conv
      { Data.convLocalMembers =
          filter (\lm -> Set.notMember (lmId lm) locals) (Data.convLocalMembers conv),
        Data.convRemoteMembers =
          filter (\rm -> Set.notMember (rmId rm) remotes) (Data.convRemoteMembers conv)
      }

isMember :: (Foldable m) => UserId -> m LocalMember -> Bool
isMember u = isJust . find ((u ==) . lmId)

isRemoteMember :: (Foldable m) => Remote UserId -> m RemoteMember -> Bool
isRemoteMember u = isJust . find ((u ==) . rmId)

class (IsConvMember mem) => IsConvMemberId uid mem | uid -> mem where
  getConvMember :: Local x -> Data.Conversation -> uid -> Maybe mem

  isConvMember :: Local x -> Data.Conversation -> uid -> Bool
  isConvMember loc conv = isJust . getConvMember loc conv

  notIsConvMember :: Local x -> Data.Conversation -> uid -> Bool
  notIsConvMember loc conv = not . isConvMember loc conv

isConvMemberL :: (IsConvMemberId uid mem) => Local Data.Conversation -> uid -> Bool
isConvMemberL lconv = isConvMember lconv (tUnqualified lconv)

instance IsConvMemberId UserId LocalMember where
  getConvMember _ conv u = find ((u ==) . lmId) (Data.convLocalMembers conv)

instance IsConvMemberId (Local UserId) LocalMember where
  getConvMember loc conv = getConvMember loc conv . tUnqualified

instance IsConvMemberId (Remote UserId) RemoteMember where
  getConvMember _ conv u = find ((u ==) . rmId) (Data.convRemoteMembers conv)

instance IsConvMemberId (Qualified UserId) (Either LocalMember RemoteMember) where
  getConvMember loc conv =
    foldQualified
      loc
      (fmap Left . getConvMember loc conv)
      (fmap Right . getConvMember loc conv)

instance IsConvMemberId LocalConvMember (Either LocalMember RemoteMember) where
  getConvMember loc conv (ConvMemberNoTeam quid) =
    getConvMember loc conv quid
  getConvMember loc conv (ConvMemberTeam tm) =
    Left . updateChannelPermissions <$> getConvMember loc conv (tm ^. Mem.userId)
    where
      updateChannelPermissions :: LocalMember -> LocalMember
      updateChannelPermissions lm =
        if hasChannelAdminPermissions
          then lm {lmConvRoleName = roleNameWireAdmin}
          else lm

      hasChannelAdminPermissions :: Bool
      hasChannelAdminPermissions =
        conv.convMetadata.cnvmGroupConvType == Just Channel
          && isAdminOrOwner (tm ^. permissions)

data LocalConvMember = ConvMemberNoTeam (Qualified UserId) | ConvMemberTeam TeamMember

-- | This type indicates whether a member is a conversation member or a team member.
-- This is a simplification because a user could be both a conversation member and a team member.
data ConvOrTeamMember mem where
  ConvMember :: (IsConvMember mem) => mem -> ConvOrTeamMember mem
  TeamMember :: TeamMember -> ConvOrTeamMember mem

class IsConvMember mem where
  convMemberRole :: mem -> RoleName
  convMemberId :: Local x -> mem -> Qualified UserId

instance IsConvMember LocalMember where
  convMemberRole = lmConvRoleName
  convMemberId loc mem = tUntagged (qualifyAs loc (lmId mem))

instance IsConvMember RemoteMember where
  convMemberRole = rmConvRoleName
  convMemberId _ = tUntagged . rmId

instance IsConvMember (Either LocalMember RemoteMember) where
  convMemberRole = either convMemberRole convMemberRole
  convMemberId loc = either (convMemberId loc) (convMemberId loc)

-- | Remove users that are already present in the conversation.
ulNewMembers :: Local x -> Data.Conversation -> UserList UserId -> UserList UserId
ulNewMembers loc conv (UserList locals remotes) =
  UserList
    (filter (notIsConvMember loc conv) locals)
    (filter (notIsConvMember loc conv) remotes)

-- | This is an ad-hoc class to update notification targets based on the type
-- of the user id. Local user IDs get added to the local targets, remote user IDs
-- to remote targets, and qualified user IDs get added to the appropriate list
-- according to whether they are local or remote, by making a runtime check.
class IsBotOrMember uid where
  bmAdd :: Local x -> uid -> BotsAndMembers -> BotsAndMembers

data BotsAndMembers = BotsAndMembers
  { bmLocals :: Set UserId,
    bmRemotes :: Set (Remote UserId),
    bmBots :: Set BotMember
  }
  deriving (Show)

bmQualifiedMembers :: Local x -> BotsAndMembers -> [Qualified UserId]
bmQualifiedMembers loc bm =
  map (tUntagged . qualifyAs loc) (toList (bmLocals bm))
    <> map tUntagged (toList (bmRemotes bm))

instance Semigroup BotsAndMembers where
  BotsAndMembers locals1 remotes1 bots1
    <> BotsAndMembers locals2 remotes2 bots2 =
      BotsAndMembers
        (locals1 <> locals2)
        (remotes1 <> remotes2)
        (bots1 <> bots2)

instance Monoid BotsAndMembers where
  mempty = BotsAndMembers mempty mempty mempty

instance IsBotOrMember (Local UserId) where
  bmAdd _ luid bm =
    bm {bmLocals = Set.insert (tUnqualified luid) (bmLocals bm)}

instance IsBotOrMember (Remote UserId) where
  bmAdd _ ruid bm = bm {bmRemotes = Set.insert ruid (bmRemotes bm)}

instance IsBotOrMember (Qualified UserId) where
  bmAdd loc = foldQualified loc (bmAdd loc) (bmAdd loc)

bmDiff :: BotsAndMembers -> BotsAndMembers -> BotsAndMembers
bmDiff bm1 bm2 =
  BotsAndMembers
    { bmLocals = Set.difference (bmLocals bm1) (bmLocals bm2),
      bmRemotes = Set.difference (bmRemotes bm1) (bmRemotes bm2),
      bmBots = Set.difference (bmBots bm1) (bmBots bm2)
    }

bmFromMembers :: [LocalMember] -> [RemoteMember] -> BotsAndMembers
bmFromMembers lmems rusers = case localBotsAndUsers lmems of
  (bots, lusers) ->
    BotsAndMembers
      { bmLocals = Set.fromList (map lmId lusers),
        bmRemotes = Set.fromList (map rmId rusers),
        bmBots = Set.fromList bots
      }

convBotsAndMembers :: Data.Conversation -> BotsAndMembers
convBotsAndMembers conv = bmFromMembers (Data.convLocalMembers conv) (Data.convRemoteMembers conv)

localBotsAndUsers :: (Foldable f) => f LocalMember -> ([BotMember], [LocalMember])
localBotsAndUsers = foldMap botOrUser
  where
    botOrUser m = case lmService m of
      -- we drop invalid bots here, which shouldn't happen
      Just _ -> (toList (newBotMember m), [])
      Nothing -> ([], [m])

nonTeamMembers :: [LocalMember] -> [TeamMember] -> [LocalMember]
nonTeamMembers cm tm = filter (not . isMemberOfTeam . lmId) cm
  where
    -- FUTUREWORK: remote members: teams and their members are always on the same backend
    isMemberOfTeam = \case
      uid -> isTeamMember uid tm

membersToRecipients :: Maybe UserId -> [TeamMember] -> [Recipient]
membersToRecipients Nothing = map (userRecipient . view Mem.userId)
membersToRecipients (Just u) = map userRecipient . filter (/= u) . map (view Mem.userId)

getSelfMemberFromLocals ::
  (Foldable t, Member (ErrorS 'ConvNotFound) r) =>
  UserId ->
  t LocalMember ->
  Sem r LocalMember
getSelfMemberFromLocals = getMember @'ConvNotFound lmId

-- | Throw 'ConvMemberNotFound' if the given user is not part of a
-- conversation (either locally or remotely).
ensureOtherMember ::
  (Member (ErrorS 'ConvMemberNotFound) r) =>
  Local a ->
  Qualified UserId ->
  Data.Conversation ->
  Sem r (Either LocalMember RemoteMember)
ensureOtherMember loc quid conv =
  noteS @'ConvMemberNotFound $
    Left
      <$> find ((== quid) . tUntagged . qualifyAs loc . lmId) (Data.convLocalMembers conv)
        <|> Right
      <$> find ((== quid) . tUntagged . rmId) (Data.convRemoteMembers conv)

getMember ::
  forall e mem t userId r.
  (Foldable t, Eq userId, Member (ErrorS e) r) =>
  -- | A projection from a member type to its user ID
  (mem -> userId) ->
  -- | The member to be found by its user ID
  userId ->
  -- | A list of members to search
  t mem ->
  Sem r mem
getMember p u = noteS @e . find ((u ==) . p)

getConversationAndCheckMembership ::
  ( Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'ConvAccessDenied) r
  ) =>
  Qualified UserId ->
  Local ConvId ->
  Sem r Data.Conversation
getConversationAndCheckMembership quid lcnv = do
  foldQualified
    lcnv
    ( \lusr -> do
        (conv, _) <-
          getConversationAndMemberWithError
            @'ConvAccessDenied
            (tUnqualified lusr)
            lcnv
        pure conv
    )
    ( \rusr -> do
        (conv, _) <-
          getConversationAndMemberWithError
            @'ConvNotFound
            rusr
            lcnv
        pure conv
    )
    quid

getConversationWithError ::
  ( Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r
  ) =>
  Local ConvId ->
  Sem r Data.Conversation
getConversationWithError lcnv =
  getConversation (tUnqualified lcnv) >>= noteS @'ConvNotFound

getConversationAndMemberWithError ::
  forall e uid mem r.
  ( Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS e) r,
    IsConvMemberId uid mem
  ) =>
  uid ->
  Local ConvId ->
  Sem r (Data.Conversation, mem)
getConversationAndMemberWithError usr lcnv = do
  c <- getConversationWithError lcnv
  member <- noteS @e $ getConvMember lcnv c usr
  pure (c, member)

-- | Deletion requires a permission check, but also a 'Role' comparison:
-- Owners can only be deleted by another owner (and not themselves).
--
-- FUTUREWORK: do not do this with 'Role', but introduce permissions "can delete owner", "can
-- delete admin", etc.
canDeleteMember :: TeamMember -> TeamMember -> Bool
canDeleteMember deleter deletee
  | getRole deletee == RoleOwner =
      getRole deleter == RoleOwner -- owners can only be deleted by another owner
        && (deleter ^. Mem.userId /= deletee ^. Mem.userId) -- owner cannot delete itself
  | otherwise =
      True
  where
    -- (team members having no role is an internal error, but we don't want to deal with that
    -- here, so we pick a reasonable default.)
    getRole mem = fromMaybe RoleMember $ permissionsRole $ mem ^. permissions

-- | Send an event to local users and bots
pushConversationEvent ::
  ( Member ExternalAccess r,
    Member NotificationSubsystem r,
    Foldable f,
    HasCellsState a
  ) =>
  Maybe ConnId ->
  a ->
  Event ->
  Local (f UserId) ->
  f BotMember ->
  Sem r ()
pushConversationEvent conn st e lusers bots = do
  pushNotifications
    [(newConversationEventPush st e (fmap toList lusers)) {conn}]
  deliverAsync (map (,e) (toList bots))

newConversationEventPush :: (HasCellsState a) => a -> Event -> Local [UserId] -> Push
newConversationEventPush st e users =
  let musr = guard (tDomain users == qDomain (evtFrom e)) $> qUnqualified (evtFrom e)
   in def
        { origin = musr,
          json = toJSONObject e,
          recipients = map userRecipient (tUnqualified users),
          isCellsEvent = shouldPushToCells st e
        }

verifyReusableCode ::
  ( Member CodeStore r,
    Member (ErrorS 'CodeNotFound) r,
    Member (ErrorS 'InvalidConversationPassword) r,
    Member HashPassword r,
    Member RateLimit r
  ) =>
  RateLimitKey ->
  Bool ->
  Maybe PlainTextPassword8 ->
  ConversationCode ->
  Sem r DataTypes.Code
verifyReusableCode rateLimitKey checkPw mPtpw convCode = do
  (c, mPw) <-
    getCode (conversationKey convCode) DataTypes.ReusableCode
      >>= noteS @'CodeNotFound
  unless (DataTypes.codeValue c == conversationCode convCode) $
    throwS @'CodeNotFound
  case (checkPw, mPtpw, mPw) of
    (True, Just ptpw, Just pw) -> do
      unlessM (HashPassword.verifyPassword rateLimitKey ptpw pw) $ throwS @'InvalidConversationPassword
    (True, Nothing, Just _) ->
      throwS @'InvalidConversationPassword
    (_, _, _) -> pure ()
  pure c

ensureConversationAccess ::
  ( Member BrigAccess r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'NotATeamMember) r,
    Member TeamStore r
  ) =>
  UserId ->
  Data.Conversation ->
  Access ->
  Sem r ()
ensureConversationAccess zusr conv access = do
  ensureAccess conv access
  zusrMembership <- maybe (pure Nothing) (`getTeamMember` zusr) (Data.convTeam conv)
  ensureAccessRole (Data.convAccessRoles conv) [(zusr, zusrMembership)]

ensureAccess ::
  (Member (ErrorS 'ConvAccessDenied) r) =>
  Data.Conversation ->
  Access ->
  Sem r ()
ensureAccess conv access =
  unless (access `elem` Data.convAccess conv) $
    throwS @'ConvAccessDenied

ensureLocal :: (Member (Error FederationError) r) => Local x -> Qualified a -> Sem r (Local a)
ensureLocal loc = foldQualified loc pure (\_ -> throw FederationNotImplemented)

--------------------------------------------------------------------------------
-- Federation

qualifyLocal :: (Member (Input (Local ())) r) => a -> Sem r (Local a)
qualifyLocal a = toLocalUnsafe <$> fmap getDomain input <*> pure a
  where
    getDomain :: Local () -> Domain
    getDomain = tDomain

runLocalInput :: Local x -> Sem (Input (Local ()) ': r) a -> Sem r a
runLocalInput = runInputConst . void

-- | Convert an internal conversation representation 'Data.Conversation' to
-- 'ConversationCreated' to be sent over the wire to a remote backend that will
-- reconstruct this into multiple public-facing
-- 'Wire.API.Conversation.Conversation' values, one per user from that remote
-- backend.
--
-- FUTUREWORK: Include the team ID as well once it becomes qualified.
toConversationCreated ::
  -- | The time stamp the conversation was created at
  UTCTime ->
  -- | The user that created the conversation
  Local UserId ->
  -- | The conversation to convert for sending to a remote Galley
  Data.Conversation ->
  -- | The resulting information to be sent to a remote Galley
  ConversationCreated ConvId
toConversationCreated now lusr Data.Conversation {convMetadata = ConversationMetadata {..}, ..} =
  ConversationCreated
    { time = now,
      origUserId = tUnqualified lusr,
      cnvId = convId,
      cnvType = cnvmType,
      cnvAccess = cnvmAccess,
      cnvAccessRoles = cnvmAccessRoles,
      cnvName = cnvmName,
      -- non-creator members are a function of the remote backend and will be
      -- overridden when fanning out the notification to remote backends.
      nonCreatorMembers = Set.empty,
      messageTimer = cnvmMessageTimer,
      receiptMode = cnvmReceiptMode,
      protocol = convProtocol,
      groupConvType = cnvmGroupConvType,
      channelAddPermission = cnvmChannelAddPermission
    }

-- | The function converts a 'ConversationCreated' value to a
-- 'Wire.API.Conversation.Conversation' value for each user that is on the given
-- domain/backend. The obtained value can be used in e.g. creating an 'Event' to
-- be sent out to users informing them that they were added to a new
-- conversation.
fromConversationCreated ::
  Local x ->
  ConversationCreated (Remote ConvId) ->
  [(Public.Member, Public.ConversationV9)]
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
    -- Currently this function creates a Member with default conversation attributes
    -- FUTUREWORK(federation): retrieve member's conversation attributes (muted, archived, etc) here once supported by the database schema.
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
    conv :: Public.Member -> [OtherMember] -> Public.ConversationV9
    conv this others =
      Public.ConversationV9
        (tUntagged cnvId)
        ConversationMetadata
          { cnvmType = cnvType,
            -- FUTUREWORK: Document this is the same domain as the conversation
            -- domain
            cnvmCreator = Just origUserId,
            cnvmAccess = cnvAccess,
            cnvmAccessRoles = cnvAccessRoles,
            cnvmName = cnvName,
            -- FUTUREWORK: Document this is the same domain as the conversation
            -- domain.
            cnvmTeam = Nothing,
            cnvmMessageTimer = messageTimer,
            cnvmReceiptMode = receiptMode,
            cnvmGroupConvType = groupConvType,
            cnvmChannelAddPermission = channelAddPermission,
            cnvmCellsState = def
          }
        (ConvMembers this others)
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

-- | Notify remote users of being added to a new conversation.
registerRemoteConversationMemberships ::
  ( Member ConversationStore r,
    Member (Error UnreachableBackends) r,
    Member (Error FederationError) r,
    Member BackendNotificationQueueAccess r,
    Member FederatorAccess r
  ) =>
  -- | The time stamp when the conversation was created
  UTCTime ->
  Local UserId ->
  Local Data.Conversation ->
  JoinType ->
  Sem r ()
registerRemoteConversationMemberships now lusr lc joinType = deleteOnUnreachable $ do
  let c = tUnqualified lc
      rc = toConversationCreated now lusr c
      allRemoteMembers = nubOrd {- (but why would there be duplicates?) -} (Data.convRemoteMembers c)
      allRemoteMembersQualified = remoteMemberQualify <$> allRemoteMembers
      allRemoteBuckets :: [Remote [RemoteMember]] = bucketRemote allRemoteMembersQualified

  -- ping involved remote backends
  void . (ensureNoUnreachableBackends =<<) $
    runFederatedConcurrentlyEither allRemoteMembersQualified $ \_ ->
      void $ fedClient @'Brig @"api-version" ()

  void . (ensureNoUnreachableBackends =<<) $
    -- let remote backends know about a subset of new joiners
    runFederatedConcurrentlyEither allRemoteMembersQualified $
      \rrms ->
        fedClient @'Galley @"on-conversation-created"
          ( rc
              { nonCreatorMembers =
                  toMembers (tUnqualified rrms)
              }
          )

  -- reachable members in buckets per remote domain
  let joined :: [Remote [RemoteMember]] = allRemoteBuckets
      joinedCoupled :: [Remote ([RemoteMember], NonEmpty (Remote UserId))]
      joinedCoupled =
        foldMap
          ( \ruids ->
              let nj =
                    foldMap (fmap rmId . tUnqualified) $
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
    creator = cnvmCreator . DataTypes.convMetadata . tUnqualified $ lc

    localNonCreators :: [OtherMember]
    localNonCreators =
      fmap (localMemberToOther . tDomain $ lc)
        . filter (\lm -> lmId lm `notElem` creator)
        . Data.convLocalMembers
        . tUnqualified
        $ lc

    -- Total set of members living on one remote backend (rs) or the hosting backend.
    toMembers :: [RemoteMember] -> Set OtherMember
    toMembers rs = Set.fromList $ localNonCreators <> fmap remoteMemberToOther rs

    convUpdateJoin :: Remote ([RemoteMember], NonEmpty (Remote UserId)) -> ConversationUpdate
    convUpdateJoin (tUnqualified -> (toNotify, newMembers)) =
      ConversationUpdate
        { time = now,
          origUserId = tUntagged lusr,
          convId = DataTypes.convId (tUnqualified lc),
          alreadyPresentUsers = fmap (tUnqualified . rmId) toNotify,
          action =
            SomeConversationAction
              (sing @'ConversationJoinTag)
              -- FUTUREWORK(md): replace the member role with whatever is provided in
              -- the NewConv input
              (ConversationJoin (tUntagged <$> newMembers) roleNameWireMember joinType)
        }

    deleteOnUnreachable ::
      ( Member ConversationStore r,
        Member (Error UnreachableBackends) r
      ) =>
      Sem r a ->
      Sem r a
    deleteOnUnreachable m = catch @UnreachableBackends m $ \e -> do
      deleteConversation (DataTypes.convId (tUnqualified lc))
      throw e

--------------------------------------------------------------------------------
-- Legalhold

userLHEnabled :: UserLegalHoldStatus -> Bool
userLHEnabled = \case
  UserLegalHoldEnabled -> True
  UserLegalHoldPending -> False
  UserLegalHoldDisabled -> False
  UserLegalHoldNoConsent -> False

data ConsentGiven = ConsentGiven | ConsentNotGiven
  deriving (Eq, Ord, Show)

consentGiven :: UserLegalHoldStatus -> ConsentGiven
consentGiven = \case
  UserLegalHoldDisabled -> ConsentGiven
  UserLegalHoldPending -> ConsentGiven
  UserLegalHoldEnabled -> ConsentGiven
  UserLegalHoldNoConsent -> ConsentNotGiven

checkConsent ::
  (Member TeamStore r) =>
  Map UserId TeamId ->
  UserId ->
  Sem r ConsentGiven
checkConsent teamsOfUsers other = do
  consentGiven <$> getLHStatus (Map.lookup other teamsOfUsers) other

-- Get legalhold status of user. Defaults to 'defUserLegalHoldStatus' if user
-- doesn't belong to a team.
getLHStatus ::
  (Member TeamStore r) =>
  Maybe TeamId ->
  UserId ->
  Sem r UserLegalHoldStatus
getLHStatus teamOfUser other = do
  case teamOfUser of
    Nothing -> pure defUserLegalHoldStatus
    Just team -> do
      mMember <- getTeamMember team other
      pure $ maybe defUserLegalHoldStatus (view legalHoldStatus) mMember

anyLegalholdActivated ::
  ( Member (Input Opts) r,
    Member TeamStore r
  ) =>
  [UserId] ->
  Sem r Bool
anyLegalholdActivated uids = do
  opts <- input
  case view (settings . featureFlags . to npProject) opts of
    FeatureLegalHoldDisabledPermanently -> pure False
    FeatureLegalHoldDisabledByDefault -> check
    FeatureLegalHoldWhitelistTeamsAndImplicitConsent -> check
  where
    check = do
      flip anyM (chunksOf 32 uids) $ \uidsPage -> do
        teamsOfUsers <- getUsersTeams uidsPage
        anyM (\uid -> userLHEnabled <$> getLHStatus (Map.lookup uid teamsOfUsers) uid) uidsPage

allLegalholdConsentGiven ::
  ( Member (Input Opts) r,
    Member LegalHoldStore r,
    Member TeamStore r
  ) =>
  [UserId] ->
  Sem r Bool
allLegalholdConsentGiven uids = do
  opts <- input
  case view (settings . featureFlags . to npProject) opts of
    FeatureLegalHoldDisabledPermanently -> pure False
    FeatureLegalHoldDisabledByDefault -> do
      flip allM (chunksOf 32 uids) $ \uidsPage -> do
        teamsOfUsers <- getUsersTeams uidsPage
        allM (\uid -> (== ConsentGiven) . consentGiven <$> getLHStatus (Map.lookup uid teamsOfUsers) uid) uidsPage
    FeatureLegalHoldWhitelistTeamsAndImplicitConsent -> do
      -- For this feature the implementation is more efficient. Being part of
      -- a whitelisted team is equivalent to have given consent to be in a
      -- conversation with user under legalhold.
      flip allM (chunksOf 32 uids) $ \uidsPage -> do
        teamsPage <- getUsersTeams uidsPage
        allM (eitherTeamMemberAndLHAllowedOrDefLHStatus teamsPage) uidsPage
      where
        eitherTeamMemberAndLHAllowedOrDefLHStatus teamsPage uid = do
          fromMaybe (consentGiven defUserLegalHoldStatus == ConsentGiven) <$> (for (Map.lookup uid teamsPage) isTeamLegalholdWhitelisted)

-- | Add to every uid the legalhold status
getLHStatusForUsers ::
  (Member TeamStore r) =>
  [UserId] ->
  Sem r [(UserId, UserLegalHoldStatus)]
getLHStatusForUsers uids =
  mconcat
    <$> for
      (chunksOf 32 uids)
      ( \uidsChunk -> do
          teamsOfUsers <- getUsersTeams uidsChunk
          for uidsChunk $ \uid -> do
            (uid,) <$> getLHStatus (Map.lookup uid teamsOfUsers) uid
      )

getTeamMembersForFanout :: (Member TeamStore r) => TeamId -> Sem r TeamMemberList
getTeamMembersForFanout tid = do
  lim <- fanoutLimit
  getTeamMembersWithLimit tid lim

ensureMemberLimit ::
  ( Foldable f,
    ( Member (ErrorS 'TooManyMembers) r,
      Member (Input Opts) r
    )
  ) =>
  ProtocolTag ->
  [LocalMember] ->
  f a ->
  Sem r ()
ensureMemberLimit ProtocolMLSTag _ _ = pure ()
ensureMemberLimit _ old new = do
  o <- input
  let maxSize = fromIntegral (o ^. settings . maxConvSize)
  when (length old + length new > maxSize) $
    throwS @'TooManyMembers

conversationExisted ::
  ( Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Data.Conversation ->
  Sem r (ConversationResponse ConversationV9)
conversationExisted lusr cnv = Existed <$> conversationViewV9 lusr cnv

getLocalUsers :: Domain -> NonEmpty (Qualified UserId) -> [UserId]
getLocalUsers localDomain = map qUnqualified . filter ((== localDomain) . qDomain) . toList

getBrigClients ::
  ( Member BrigAccess r,
    Member ClientStore r
  ) =>
  [UserId] ->
  Sem r Clients
getBrigClients users = do
  isInternal <- useIntraClientListing
  if isInternal
    then fromUserClients <$> lookupClients users
    else getClients users

getUpdateResult :: Sem (Error NoChanges ': r) a -> Sem r (UpdateResult a)
getUpdateResult = fmap (either (const Unchanged) Updated) . runError

--------------------------------------------------------------------------------
-- Handling remote errors

class RethrowErrors (effs :: EffectRow) r where
  rethrowErrors :: GalleyError -> Sem r a

instance (Member (Error FederationError) r) => RethrowErrors '[] r where
  rethrowErrors :: GalleyError -> Sem r a
  rethrowErrors err' = throw (FederationUnexpectedError (T.pack . show $ err'))

instance
  ( SingI (e :: GalleyError),
    Member (ErrorS e) r,
    RethrowErrors effs r
  ) =>
  RethrowErrors (ErrorS e ': effs) r
  where
  rethrowErrors :: GalleyError -> Sem r a
  rethrowErrors err' =
    if err' == demote @e
      then throwS @e
      else rethrowErrors @effs @r err'
