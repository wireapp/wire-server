{-# LANGUAGE RecordWildCards #-}

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

module Wire.UserGroupSubsystem.Interpreter where

import Control.Error (MaybeT (..))
import Control.Lens ((^.))
import Data.Default
import Data.Id
import Data.Json.Util
import Data.Qualified
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input (Input, input)
import Wire.API.BackgroundJobs
import Wire.API.Conversation qualified as Conversation
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.Pagination
import Wire.API.Push.V2 (RecipientClients (RecipientClientsAll))
import Wire.API.Routes.Internal.Brig
import Wire.API.Team.Member
import Wire.API.Team.Member qualified as TM
import Wire.API.User
import Wire.API.UserEvent
import Wire.API.UserGroup
import Wire.API.UserGroup.Pagination
import Wire.BackgroundJobsPublisher
import Wire.Error
import Wire.GalleyAPIAccess (GalleyAPIAccess, internalGetConversation)
import Wire.NotificationSubsystem
import Wire.PaginationState
import Wire.Sem.Random qualified as Random
import Wire.TeamSubsystem
import Wire.UserGroupStore (UserGroupPageRequest (..))
import Wire.UserGroupStore qualified as Store
import Wire.UserGroupSubsystem (GroupSearch (..), UserGroupSubsystem (..))
import Wire.UserSubsystem (UserSubsystem, getLocalUserProfiles, getUserTeam)

interpretUserGroupSubsystem ::
  ( Member Random.Random r,
    Member UserSubsystem r,
    Member (Error UserGroupSubsystemError) r,
    Member Store.UserGroupStore r,
    Member (Input (Local ())) r,
    Member NotificationSubsystem r,
    Member TeamSubsystem r,
    Member GalleyAPIAccess r,
    Member BackgroundJobsPublisher r
  ) =>
  InterpreterFor UserGroupSubsystem r
interpretUserGroupSubsystem = interpret $ \case
  CreateGroup creator newGroup -> createUserGroup creator newGroup
  GetGroup getter gid includeChannels -> getUserGroup getter gid includeChannels
  GetGroups getter search -> getUserGroups getter search
  UpdateGroup updater groupId groupUpdate -> updateGroup updater groupId groupUpdate
  DeleteGroup deleter groupId -> deleteGroup deleter groupId
  DeleteGroupManaged managedBy team groupId -> deleteGroupManagedImpl managedBy team groupId
  AddUser adder groupId addeeId -> addUser adder groupId addeeId
  AddUsers adder groupId addeeIds -> addUsers adder groupId addeeIds
  UpdateUsers updater groupId uids -> updateUsers updater groupId uids
  RemoveUser remover groupId removeeId -> removeUser remover groupId removeeId
  RemoveUserFromAllGroups uid tid -> removeUserFromAllGroups uid tid
  AddChannels performer groupId channelIds -> updateChannels True performer groupId channelIds
  UpdateChannels performer groupId channelIds -> updateChannels False performer groupId channelIds
  -- Internal API handlers
  CreateGroupInternal managedBy team mbCreator newGroup -> createUserGroupFullImpl managedBy team mbCreator newGroup
  GetGroupInternal tid gid includeChannels -> getUserGroupInternal tid gid includeChannels
  GetGroupsInternal tid displayNameSubstring -> getUserGroupsInternal tid displayNameSubstring
  ResetUserGroupInternal req -> resetUserGroupInternal req

data UserGroupSubsystemError
  = UserGroupNotATeamAdmin
  | UserGroupMemberIsNotInTheSameTeam
  | UserGroupNotFound
  | UserGroupChannelNotFound
  | UserGroupManagedByMismatch
  deriving (Show, Eq)

userGroupSubsystemErrorToHttpError :: UserGroupSubsystemError -> HttpError
userGroupSubsystemErrorToHttpError =
  StdError . \case
    UserGroupNotATeamAdmin -> errorToWai @E.UserGroupNotATeamAdmin
    UserGroupMemberIsNotInTheSameTeam -> errorToWai @E.UserGroupMemberIsNotInTheSameTeam
    UserGroupNotFound -> errorToWai @E.UserGroupNotFound
    UserGroupChannelNotFound -> errorToWai @E.UserGroupChannelNotFound
    UserGroupManagedByMismatch -> errorToWai @E.UserGroupManagedByMismatch

createUserGroup ::
  ( Member Random.Random r,
    Member UserSubsystem r,
    Member (Error UserGroupSubsystemError) r,
    Member Store.UserGroupStore r,
    Member (Input (Local ())) r,
    Member NotificationSubsystem r,
    Member TeamSubsystem r,
    Member BackgroundJobsPublisher r
  ) =>
  UserId ->
  NewUserGroup ->
  Sem r UserGroup
createUserGroup creator newGroup = do
  team <- getTeamAsAdmin creator >>= note UserGroupNotATeamAdmin
  createUserGroupFullImpl ManagedByWire team (Just creator) newGroup

createUserGroupFullImpl ::
  forall r.
  ( Member UserSubsystem r,
    Member (Error UserGroupSubsystemError) r,
    Member Store.UserGroupStore r,
    Member (Input (Local ())) r,
    Member NotificationSubsystem r,
    Member TeamSubsystem r,
    Member Random.Random r,
    Member BackgroundJobsPublisher r
  ) =>
  ManagedBy ->
  TeamId {- home team of the user group.-} ->
  Maybe UserId {- creator of the user group (just needed for exclusion from event; this is not
                checked for consistency with TeamId. -} ->
  NewUserGroup ->
  Sem r UserGroup
createUserGroupFullImpl managedBy team mbCreator newGroup = do
  guardMembersInTeam
  ug <- Store.createUserGroup team newGroup managedBy
  notifyAdmins ug
  triggerSyncUserGroup team mbCreator ug.id_
  pure ug
  where
    guardMembersInTeam :: Sem r ()
    guardMembersInTeam = do
      groupMembersFound :: [UserProfile] <- getLocalUserProfiles =<< qualifyLocal (toList newGroup.members)
      let groupMemberIdsRequested :: [UserId] = toList newGroup.members
          groupMemberIdsFound :: [UserId] = qUnqualified . profileQualifiedId <$> groupMembersFound
          nobodyMissing = Set.fromList groupMemberIdsRequested == Set.fromList groupMemberIdsFound

          allTeams :: [Maybe TeamId] = nub $ profileTeam <$> groupMembersFound
          nobodyFromOtherTeam = allTeams == [Just team] || null (toList newGroup.members)

      unless (nobodyMissing && nobodyFromOtherTeam) do
        throw UserGroupMemberIsNotInTheSameTeam

    notifyAdmins :: UserGroup -> Sem r ()
    notifyAdmins ug = do
      admins <- fmap (^. TM.userId) . (^. teamMembers) <$> internalGetTeamAdmins team
      pushNotifications
        [ mmkEvent mbCreator (UserGroupCreated ug.id_) admins
        ]

getTeamAsAdmin ::
  ( Member UserSubsystem r,
    Member TeamSubsystem r
  ) =>
  UserId ->
  Sem r (Maybe TeamId)
getTeamAsAdmin user = runMaybeT do
  (team, member) <- MaybeT $ getTeamAsMember user
  guard (isAdminOrOwner (member ^. permissions))
  pure team

getTeamAsMember ::
  ( Member UserSubsystem r,
    Member TeamSubsystem r
  ) =>
  UserId ->
  Sem r (Maybe (TeamId, TeamMember))
getTeamAsMember memberId = runMaybeT do
  team <- MaybeT $ getUserTeam memberId
  mbr <- MaybeT $ internalGetTeamMember memberId team
  pure (team, mbr)

mmkEvent :: Maybe UserId -> UserGroupEvent -> [UserId] -> Push
mmkEvent mAuthor evt recipients =
  def
    { origin = mAuthor,
      json = toJSONObject $ UserGroupEvent evt,
      recipients = (\uid -> Recipient {recipientUserId = uid, recipientClients = RecipientClientsAll}) <$> recipients,
      transient = True
    }

mkEvent :: UserId -> UserGroupEvent -> [UserId] -> Push
mkEvent = mmkEvent . Just

qualifyLocal :: (Member (Input (Local ())) r) => a -> Sem r (Local a)
qualifyLocal a = do
  l <- input
  pure $ qualifyAs l a

getUserGroup ::
  ( Member UserSubsystem r,
    Member Store.UserGroupStore r,
    Member TeamSubsystem r
  ) =>
  UserId ->
  UserGroupId ->
  Bool ->
  Sem r (Maybe UserGroup)
getUserGroup getter gid includeChannels = runMaybeT $ do
  team <- MaybeT $ getUserTeam getter
  getterCanSeeAll <- mkGetterCanSeeAll getter team
  userGroup <- MaybeT $ getUserGroupInternal team gid includeChannels
  if getterCanSeeAll || getter `elem` (toList (runIdentity userGroup.members))
    then pure userGroup
    else MaybeT $ pure Nothing

getUserGroupInternal ::
  (Member Store.UserGroupStore r) =>
  TeamId ->
  UserGroupId ->
  Bool ->
  Sem r (Maybe UserGroup)
getUserGroupInternal tid gid includeChannels = runMaybeT $ do
  MaybeT $ Store.getUserGroup tid gid includeChannels

mkGetterCanSeeAll ::
  forall r.
  (Member TeamSubsystem r) =>
  UserId ->
  TeamId ->
  MaybeT (Sem r) Bool
mkGetterCanSeeAll getter team = do
  creatorTeamMember <- MaybeT $ internalGetTeamMember getter team
  pure . isAdminOrOwner $ creatorTeamMember ^. permissions

getUserGroups ::
  forall r.
  ( Member UserSubsystem r,
    Member TeamSubsystem r,
    Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r
  ) =>
  UserId ->
  GroupSearch ->
  Sem r UserGroupPage
getUserGroups getter search = do
  team :: TeamId <- getUserTeam getter >>= ifNothing UserGroupNotATeamAdmin
  getterCanSeeAll :: Bool <- fromMaybe False <$> runMaybeT (mkGetterCanSeeAll getter team)
  unless getterCanSeeAll (throw UserGroupNotATeamAdmin)
  let pageReq =
        UserGroupPageRequest
          { pageSize = fromMaybe def search.pageSize,
            sortOrder = fromMaybe Desc search.sortOrder,
            paginationState = case fromMaybe def search.sortBy of
              SortByName -> PaginationSortByName $ (,) <$> search.lastName <*> search.lastId
              SortByCreatedAt -> PaginationSortByCreatedAt $ (,) <$> search.lastCreatedAt <*> search.lastId,
            team = team,
            searchString = search.query,
            includeMemberCount = search.includeMemberCount,
            includeChannels = search.includeChannels
          }
  Store.getUserGroups pageReq
  where
    ifNothing :: UserGroupSubsystemError -> Maybe a -> Sem r a
    ifNothing e = maybe (throw e) pure

getUserGroupsInternal ::
  forall r.
  ( Member Store.UserGroupStore r
  ) =>
  TeamId ->
  Maybe Text ->
  Sem r UserGroupPageWithMembers
getUserGroupsInternal team displayNameSubstring = do
  let -- hscim doesn't support pagination at the time of writing this,
      -- so we better fit all groups into one page!
      pageSize = pageSizeFromIntUnsafe 500
      pageReq =
        UserGroupPageRequest
          { pageSize = pageSize,
            sortOrder = Asc,
            paginationState = PaginationSortByName Nothing,
            team = team,
            searchString = displayNameSubstring,
            includeMemberCount = True,
            includeChannels = False
          }
  Store.getUserGroupsWithMembers pageReq

updateGroup ::
  ( Member UserSubsystem r,
    Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member NotificationSubsystem r,
    Member TeamSubsystem r
  ) =>
  UserId ->
  UserGroupId ->
  UserGroupUpdate ->
  Sem r ()
updateGroup updater groupId groupUpdate = do
  team <- getTeamAsAdmin updater >>= note UserGroupNotATeamAdmin
  updateGroupNoAccessControl team (Just updater) groupId groupUpdate

updateGroupNoAccessControl ::
  ( Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member NotificationSubsystem r,
    Member TeamSubsystem r
  ) =>
  TeamId ->
  Maybe UserId ->
  UserGroupId ->
  UserGroupUpdate ->
  Sem r ()
updateGroupNoAccessControl teamId mbUpdater groupId groupUpdate = do
  found <- isJust <$> Store.updateUserGroup teamId groupId groupUpdate
  if found
    then do
      admins <- fmap (^. TM.userId) . (^. teamMembers) <$> internalGetTeamAdmins teamId
      pushNotifications [mmkEvent mbUpdater (UserGroupUpdated groupId) admins]
    else throw UserGroupNotFound

deleteGroup ::
  ( Member UserSubsystem r,
    Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member NotificationSubsystem r,
    Member TeamSubsystem r
  ) =>
  UserId ->
  UserGroupId ->
  Sem r ()
deleteGroup deleter groupId =
  getTeamAsMember deleter >>= \case
    Nothing -> throw UserGroupNotFound
    Just (team, member) -> do
      unless (isAdminOrOwner (member ^. permissions)) $
        throw UserGroupNotATeamAdmin

      Store.deleteUserGroup team groupId >>= note UserGroupNotFound

      admins <- fmap (^. TM.userId) . (^. teamMembers) <$> internalGetTeamAdmins team
      pushNotifications [mkEvent deleter (UserGroupDeleted groupId) admins]

deleteGroupManagedImpl ::
  ( Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member NotificationSubsystem r,
    Member TeamSubsystem r
  ) =>
  ManagedBy ->
  TeamId ->
  UserGroupId ->
  Sem r ()
deleteGroupManagedImpl managedBy team groupId = do
  -- First, get the group to verify it exists and check its managedBy field
  maybeGroup <- Store.getUserGroup team groupId False
  case maybeGroup of
    Nothing -> pure () -- idempotency
    Just group' ->
      if group'.managedBy == managedBy
        then do
          -- Delete the group
          mUserGroup <- Store.deleteUserGroup team groupId

          for_ mUserGroup $ \_ -> do
            -- Send notifications to team admins
            admins <- fmap (^. TM.userId) . (^. teamMembers) <$> internalGetTeamAdmins team
            pushNotifications [mmkEvent Nothing (UserGroupDeleted groupId) admins]
        else
          throw UserGroupManagedByMismatch

addUser ::
  ( Member Random.Random r,
    Member UserSubsystem r,
    Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member NotificationSubsystem r,
    Member TeamSubsystem r,
    Member BackgroundJobsPublisher r
  ) =>
  UserId ->
  UserGroupId ->
  UserId ->
  Sem r ()
addUser adder groupId addeeId = do
  ug <- getUserGroup adder groupId False >>= note UserGroupNotFound
  team <- getTeamAsAdmin adder >>= note UserGroupNotATeamAdmin
  void $ internalGetTeamMember addeeId team >>= note UserGroupMemberIsNotInTheSameTeam
  unless (addeeId `elem` runIdentity ug.members) $ do
    Store.addUser groupId addeeId
    admins <- fmap (^. TM.userId) . (^. teamMembers) <$> internalGetTeamAdmins team
    pushNotifications
      [ mkEvent adder (UserGroupUpdated groupId) admins
      ]
    triggerSyncUserGroup team (Just adder) groupId

addUsers ::
  ( Member Random.Random r,
    Member UserSubsystem r,
    Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member NotificationSubsystem r,
    Member TeamSubsystem r,
    Member BackgroundJobsPublisher r
  ) =>
  UserId ->
  UserGroupId ->
  Vector UserId ->
  Sem r ()
addUsers adder groupId addeeIds = do
  ug <- getUserGroup adder groupId False >>= note UserGroupNotFound
  team <- getTeamAsAdmin adder >>= note UserGroupNotATeamAdmin
  forM_ addeeIds $ \addeeId ->
    internalGetTeamMember addeeId team >>= note UserGroupMemberIsNotInTheSameTeam

  let missingAddeeIds = toList addeeIds \\ toList (runIdentity ug.members)
  unless (null missingAddeeIds) $ do
    mapM_ (Store.addUser groupId) missingAddeeIds
    admins <- fmap (^. TM.userId) . (^. teamMembers) <$> internalGetTeamAdmins team
    pushNotifications
      [ mkEvent adder (UserGroupUpdated groupId) admins
      ]

  triggerSyncUserGroup team (Just adder) groupId

updateUsers ::
  ( Member Random.Random r,
    Member UserSubsystem r,
    Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member NotificationSubsystem r,
    Member TeamSubsystem r,
    Member BackgroundJobsPublisher r
  ) =>
  UserId ->
  UserGroupId ->
  Vector UserId ->
  Sem r ()
updateUsers updater groupId uids = do
  team <- getTeamAsAdmin updater >>= note UserGroupNotATeamAdmin
  updateUsersNoAccessControl team (Just updater) groupId uids

updateUsersNoAccessControl ::
  ( Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member NotificationSubsystem r,
    Member TeamSubsystem r,
    Member Random.Random r,
    Member BackgroundJobsPublisher r
  ) =>
  TeamId ->
  Maybe UserId ->
  UserGroupId ->
  Vector UserId ->
  Sem r ()
updateUsersNoAccessControl teamId mbUpdater groupId uids = do
  void $ getUserGroupInternal teamId groupId False >>= note UserGroupNotFound
  forM_ uids $ \uid ->
    internalGetTeamMember uid teamId >>= note UserGroupMemberIsNotInTheSameTeam
  Store.updateUsers groupId uids
  admins <- fmap (^. TM.userId) . (^. teamMembers) <$> internalGetTeamAdmins teamId
  pushNotifications
    [ mmkEvent mbUpdater (UserGroupUpdated groupId) admins
    ]
  triggerSyncUserGroup teamId mbUpdater groupId

removeUser ::
  ( Member Random.Random r,
    Member UserSubsystem r,
    Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member NotificationSubsystem r,
    Member TeamSubsystem r,
    Member BackgroundJobsPublisher r
  ) =>
  UserId ->
  UserGroupId ->
  UserId ->
  Sem r ()
removeUser remover groupId removeeId = do
  ug <- getUserGroup remover groupId False >>= note UserGroupNotFound
  team <- getTeamAsAdmin remover >>= note UserGroupNotATeamAdmin
  void $ internalGetTeamMember removeeId team >>= note UserGroupMemberIsNotInTheSameTeam
  when (removeeId `elem` runIdentity ug.members) $ do
    Store.removeUser groupId removeeId
    admins <- fmap (^. TM.userId) . (^. teamMembers) <$> internalGetTeamAdmins team
    pushNotifications
      [ mkEvent remover (UserGroupUpdated groupId) admins
      ]
    triggerSyncUserGroup team (Just remover) groupId

removeUserFromAllGroups ::
  ( Member Store.UserGroupStore r,
    Member TeamSubsystem r
  ) =>
  UserId ->
  TeamId ->
  Sem r ()
removeUserFromAllGroups uid tid = do
  internalGetTeamMember uid tid >>= \case
    Just _ -> nextPage Nothing >>= go
    Nothing -> pure ()
  where
    go (ug : ugs) = do
      Store.removeUser ug.id_ uid
      -- when we get to the last item, get a new page
      ugs' <- case ugs of
        [] -> nextPage (Just ug)
        _ -> pure ugs
      go ugs'
    -- no more items, terminate
    go [] = pure ()

    nextPage mug =
      fmap (.page) . Store.getUserGroups $
        UserGroupPageRequest
          { pageSize = def,
            sortOrder = Desc,
            paginationState =
              PaginationSortByCreatedAt $
                fmap Store.userGroupCreatedAtPaginationState mug,
            team = tid,
            searchString = Nothing,
            includeMemberCount = False,
            includeChannels = False
          }

updateChannels ::
  ( Member Random.Random r,
    Member UserSubsystem r,
    Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member TeamSubsystem r,
    Member NotificationSubsystem r,
    Member GalleyAPIAccess r,
    Member BackgroundJobsPublisher r
  ) =>
  Bool ->
  UserId ->
  UserGroupId ->
  Vector ConvId ->
  Sem r ()
updateChannels appendOnly performer groupId channelIds = do
  void $ getUserGroup performer groupId False >>= note UserGroupNotFound
  teamId <- getTeamAsAdmin performer >>= note UserGroupNotATeamAdmin
  for_ channelIds $ \channelId -> do
    conv <- internalGetConversation channelId >>= note UserGroupChannelNotFound
    let meta = conv.metadata
    unless (meta.cnvmTeam == Just teamId && meta.cnvmGroupConvType == Just Conversation.Channel) $
      throw UserGroupChannelNotFound

  if appendOnly
    then Store.addUserGroupChannels groupId channelIds
    else Store.updateUserGroupChannels groupId channelIds

  triggerSyncUserGroup teamId (Just performer) groupId

  admins <- fmap (^. TM.userId) . (^. teamMembers) <$> internalGetTeamAdmins teamId
  pushNotifications
    [ mkEvent performer (UserGroupUpdated groupId) admins
    ]

triggerSyncUserGroup ::
  ( Member Random.Random r,
    Member BackgroundJobsPublisher r
  ) =>
  TeamId ->
  Maybe UserId ->
  UserGroupId ->
  Sem r ()
triggerSyncUserGroup teamId actor userGroupId = do
  jobId <- Random.newId
  publishJob jobId $ JobSyncUserGroup SyncUserGroup {..}

resetUserGroupInternal ::
  ( Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member TeamSubsystem r,
    Member NotificationSubsystem r,
    Member Random.Random r,
    Member BackgroundJobsPublisher r
  ) =>
  UpdateGroupInternalRequest ->
  Sem r ()
resetUserGroupInternal UpdateGroupInternalRequest {..} = do
  forM_ name $ updateGroupNoAccessControl teamId Nothing groupId . UserGroupUpdate
  forM_ members $ updateUsersNoAccessControl teamId Nothing groupId . V.fromList
