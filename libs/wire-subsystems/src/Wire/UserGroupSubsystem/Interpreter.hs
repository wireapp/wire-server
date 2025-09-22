module Wire.UserGroupSubsystem.Interpreter where

import Control.Error (MaybeT (..))
import Control.Lens ((^.))
import Data.Default
import Data.Id
import Data.Json.Util
import Data.Qualified (Local, Qualified (qUnqualified), qualifyAs)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input (Input, input)
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.Pagination
import Wire.API.Push.V2 (RecipientClients (RecipientClientsAll))
import Wire.API.Team.Member
import Wire.API.Team.Member qualified as TM
import Wire.API.User
import Wire.API.UserEvent
import Wire.API.UserGroup
import Wire.API.UserGroup.Pagination
import Wire.Error
import Wire.GalleyAPIAccess (GalleyAPIAccess, getTeamConv)
import Wire.NotificationSubsystem
import Wire.TeamSubsystem
import Wire.UserGroupStore (PaginationState (..), UserGroupPageRequest (..))
import Wire.UserGroupStore qualified as Store
import Wire.UserGroupSubsystem (UserGroupSubsystem (..))
import Wire.UserSubsystem (UserSubsystem, getLocalUserProfiles, getUserTeam)

interpretUserGroupSubsystem ::
  ( Member UserSubsystem r,
    Member (Error UserGroupSubsystemError) r,
    Member Store.UserGroupStore r,
    Member (Input (Local ())) r,
    Member NotificationSubsystem r,
    Member TeamSubsystem r,
    Member GalleyAPIAccess r
  ) =>
  InterpreterFor UserGroupSubsystem r
interpretUserGroupSubsystem = interpret $ \case
  CreateGroup creator newGroup -> createUserGroup creator newGroup
  GetGroup getter gid -> getUserGroup getter gid
  GetGroups getter q sortByKeys sortOrder pSize mLastGroupName mLastCreatedAt mLastGroupId includeMemberCount ->
    getUserGroups getter q sortByKeys sortOrder pSize mLastGroupName mLastCreatedAt mLastGroupId includeMemberCount
  UpdateGroup updater groupId groupUpdate -> updateGroup updater groupId groupUpdate
  DeleteGroup deleter groupId -> deleteGroup deleter groupId
  AddUser adder groupId addeeId -> addUser adder groupId addeeId
  AddUsers adder groupId addeeIds -> addUsers adder groupId addeeIds
  UpdateUsers updater groupId uids -> updateUsers updater groupId uids
  RemoveUser remover groupId removeeId -> removeUser remover groupId removeeId
  UpdateChannels remover groupId channelIds -> updateChannels remover groupId channelIds

data UserGroupSubsystemError
  = UserGroupNotATeamAdmin
  | UserGroupMemberIsNotInTheSameTeam
  | UserGroupNotFound
  | UserGroupChannelNotFound
  deriving (Show, Eq)

userGroupSubsystemErrorToHttpError :: UserGroupSubsystemError -> HttpError
userGroupSubsystemErrorToHttpError =
  StdError . \case
    UserGroupNotATeamAdmin -> errorToWai @E.UserGroupNotATeamAdmin
    UserGroupMemberIsNotInTheSameTeam -> errorToWai @E.UserGroupMemberIsNotInTheSameTeam
    UserGroupNotFound -> errorToWai @E.UserGroupNotFound
    UserGroupChannelNotFound -> errorToWai @E.UserGroupChannelNotFound

createUserGroup ::
  ( Member UserSubsystem r,
    Member (Error UserGroupSubsystemError) r,
    Member Store.UserGroupStore r,
    Member (Input (Local ())) r,
    Member NotificationSubsystem r,
    Member TeamSubsystem r
  ) =>
  UserId ->
  NewUserGroup ->
  Sem r UserGroup
createUserGroup creator newGroup = do
  let managedBy = ManagedByWire
  team <- getTeamAsAdmin creator >>= note UserGroupNotATeamAdmin
  luids <- qualifyLocal $ toList newGroup.members
  profiles <- getLocalUserProfiles luids
  let existingIds = Set.fromList $ fmap (qUnqualified . profileQualifiedId) profiles
  let actualIds = Set.fromList $ toList newGroup.members
  let allInSameTeam = all (\p -> p.profileTeam == Just team) profiles
  when (existingIds /= actualIds || not allInSameTeam) $
    throw $
      UserGroupMemberIsNotInTheSameTeam
  ug <- Store.createUserGroup team newGroup managedBy
  admins <- fmap (^. TM.userId) . (^. teamMembers) <$> internalGetTeamAdmins team
  pushNotifications
    [ mkEvent creator (UserGroupCreated ug.id_) admins
    ]
  pure ug

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

mkEvent :: UserId -> UserGroupEvent -> [UserId] -> Push
mkEvent author evt recipients =
  def
    { origin = Just author,
      json = toJSONObject $ UserGroupEvent evt,
      recipients = (\uid -> Recipient {recipientUserId = uid, recipientClients = RecipientClientsAll}) <$> recipients,
      transient = True
    }

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
  Sem r (Maybe UserGroup)
getUserGroup getter gid = runMaybeT $ do
  team <- MaybeT $ getUserTeam getter
  getterCanSeeAll <- mkGetterCanSeeAll getter team
  userGroup <- MaybeT $ Store.getUserGroup team gid
  if getterCanSeeAll || getter `elem` (toList (runIdentity userGroup.members))
    then pure userGroup
    else MaybeT $ pure Nothing

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
  Maybe Text ->
  Maybe SortBy ->
  Maybe SortOrder ->
  Maybe PageSize ->
  Maybe UserGroupName ->
  Maybe UTCTimeMillis ->
  Maybe UserGroupId ->
  Bool ->
  Sem r UserGroupPage
getUserGroups getter searchString sortBy' sortOrder' mPageSize mLastGroupName mLastCreatedAt mLastGroupId includeMemberCount' = do
  team :: TeamId <- getUserTeam getter >>= ifNothing UserGroupNotATeamAdmin
  getterCanSeeAll :: Bool <- fromMaybe False <$> runMaybeT (mkGetterCanSeeAll getter team)
  unless getterCanSeeAll (throw UserGroupNotATeamAdmin)
  let pageReq =
        UserGroupPageRequest
          { pageSize = fromMaybe def mPageSize,
            sortOrder = fromMaybe Desc sortOrder',
            paginationState = case (fromMaybe def sortBy') of
              SortByName -> PaginationSortByName $ (,) <$> mLastGroupName <*> mLastGroupId
              SortByCreatedAt -> PaginationSortByCreatedAt $ (,) <$> mLastCreatedAt <*> mLastGroupId,
            team = team,
            searchString = searchString,
            includeMemberCount = includeMemberCount'
          }
  Store.getUserGroups pageReq
  where
    ifNothing :: UserGroupSubsystemError -> Maybe a -> Sem r a
    ifNothing e = maybe (throw e) pure

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
  found <- isJust <$> Store.updateUserGroup team groupId groupUpdate
  if found
    then do
      admins <- fmap (^. TM.userId) . (^. teamMembers) <$> internalGetTeamAdmins team
      pushNotifications [mkEvent updater (UserGroupUpdated groupId) admins]
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
      if isAdminOrOwner (member ^. permissions)
        then do
          found <- isJust <$> Store.deleteUserGroup team groupId
          if found
            then do
              admins <- fmap (^. TM.userId) . (^. teamMembers) <$> internalGetTeamAdmins team
              pushNotifications [mkEvent deleter (UserGroupDeleted groupId) admins]
            else
              throw UserGroupNotFound
        else do
          throw UserGroupNotATeamAdmin

addUser ::
  ( Member UserSubsystem r,
    Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member NotificationSubsystem r,
    Member TeamSubsystem r
  ) =>
  UserId ->
  UserGroupId ->
  UserId ->
  Sem r ()
addUser adder groupId addeeId = do
  ug <- getUserGroup adder groupId >>= note UserGroupNotFound
  team <- getTeamAsAdmin adder >>= note UserGroupNotATeamAdmin
  void $ internalGetTeamMember addeeId team >>= note UserGroupMemberIsNotInTheSameTeam
  unless (addeeId `elem` runIdentity ug.members) $ do
    Store.addUser groupId addeeId
    admins <- fmap (^. TM.userId) . (^. teamMembers) <$> internalGetTeamAdmins team
    pushNotifications
      [ mkEvent adder (UserGroupUpdated groupId) admins
      ]

addUsers ::
  ( Member UserSubsystem r,
    Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member NotificationSubsystem r,
    Member TeamSubsystem r
  ) =>
  UserId ->
  UserGroupId ->
  Vector UserId ->
  Sem r ()
addUsers adder groupId addeeIds = do
  ug <- getUserGroup adder groupId >>= note UserGroupNotFound
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

updateUsers ::
  ( Member UserSubsystem r,
    Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member NotificationSubsystem r,
    Member TeamSubsystem r
  ) =>
  UserId ->
  UserGroupId ->
  Vector UserId ->
  Sem r ()
updateUsers updater groupId uids = do
  void $ getUserGroup updater groupId >>= note UserGroupNotFound
  team <- getTeamAsAdmin updater >>= note UserGroupNotATeamAdmin
  forM_ uids $ \uid ->
    internalGetTeamMember uid team >>= note UserGroupMemberIsNotInTheSameTeam
  Store.updateUsers groupId uids
  admins <- fmap (^. TM.userId) . (^. teamMembers) <$> internalGetTeamAdmins team
  pushNotifications
    [ mkEvent updater (UserGroupUpdated groupId) admins
    ]

removeUser ::
  ( Member UserSubsystem r,
    Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member NotificationSubsystem r,
    Member TeamSubsystem r
  ) =>
  UserId ->
  UserGroupId ->
  UserId ->
  Sem r ()
removeUser remover groupId removeeId = do
  ug <- getUserGroup remover groupId >>= note UserGroupNotFound
  team <- getTeamAsAdmin remover >>= note UserGroupNotATeamAdmin
  void $ internalGetTeamMember removeeId team >>= note UserGroupMemberIsNotInTheSameTeam
  when (removeeId `elem` runIdentity ug.members) $ do
    Store.removeUser groupId removeeId
    admins <- fmap (^. TM.userId) . (^. teamMembers) <$> internalGetTeamAdmins team
    pushNotifications
      [ mkEvent remover (UserGroupUpdated groupId) admins
      ]

updateChannels ::
  ( Member UserSubsystem r,
    Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member TeamSubsystem r,
    Member GalleyAPIAccess r
  ) =>
  UserId ->
  UserGroupId ->
  Vector ConvId ->
  Sem r ()
updateChannels performer groupId channelIds = do
  void $ getUserGroup performer groupId >>= note UserGroupNotFound
  teamId <- getTeamAsAdmin performer >>= note UserGroupNotATeamAdmin
  traverse_ (getTeamConv performer teamId >=> note UserGroupChannelNotFound) channelIds
  Store.updateUserGroupChannels teamId groupId channelIds
