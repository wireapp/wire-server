module Wire.UserGroupSubsystem.Interpreter where

import Control.Error (MaybeT (..))
import Control.Lens ((^.))
import Data.Default
import Data.Id
import Data.Json.Util
import Data.Qualified (Local, Qualified (qUnqualified), qualifyAs)
import Data.Set qualified as Set
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input (Input, input)
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.Push.V2 (RecipientClients (RecipientClientsAll))
import Wire.API.Team.Member
import Wire.API.Team.Member qualified as TM
import Wire.API.User
import Wire.API.UserEvent
import Wire.API.UserGroup
import Wire.API.UserGroup.Pagination
import Wire.Error
import Wire.NotificationSubsystem
import Wire.TeamSubsystem
import Wire.UserGroupStore (PaginationState (..), UserGroupPageRequest (..))
import Wire.UserGroupStore qualified as Store
import Wire.UserGroupSubsystem
import Wire.UserSubsystem (UserSubsystem, getLocalUserProfiles, getUserTeam)

interpretUserGroupSubsystem ::
  ( Member UserSubsystem r,
    Member (Error UserGroupSubsystemError) r,
    Member Store.UserGroupStore r,
    Member (Input (Local ())) r,
    Member NotificationSubsystem r,
    Member TeamSubsystem r
  ) =>
  InterpreterFor UserGroupSubsystem r
interpretUserGroupSubsystem = interpret $ \case
  CreateGroup creator newGroup -> createUserGroupImpl creator newGroup
  GetGroup getter gid -> getUserGroupImpl getter gid
  GetGroups getter q sortByKeys sortOrder pSize mLastGroupName mLastCreatedAt mLastGroupId ->
    getUserGroupsImpl getter q sortByKeys sortOrder pSize mLastGroupName mLastCreatedAt mLastGroupId
  UpdateGroup updater groupId groupUpdate -> updateGroupImpl updater groupId groupUpdate
  DeleteGroup deleter groupId -> deleteGroupImpl deleter groupId
  AddUser adder groupId addeeId -> addUserImpl adder groupId addeeId
  RemoveUser remover groupId removeeId -> removeUserImpl remover groupId removeeId

data UserGroupSubsystemError
  = UserGroupNotATeamAdmin
  | UserGroupMemberIsNotInTheSameTeam
  | UserGroupNotFound
  deriving (Show, Eq)

userGroupSubsystemErrorToHttpError :: UserGroupSubsystemError -> HttpError
userGroupSubsystemErrorToHttpError =
  StdError . \case
    UserGroupNotATeamAdmin -> errorToWai @E.UserGroupNotATeamAdmin
    UserGroupMemberIsNotInTheSameTeam -> errorToWai @E.UserGroupMemberIsNotInTheSameTeam
    UserGroupNotFound -> errorToWai @E.UserGroupNotFound

createUserGroupImpl ::
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
createUserGroupImpl creator newGroup = do
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

getUserGroupImpl ::
  ( Member UserSubsystem r,
    Member Store.UserGroupStore r,
    Member TeamSubsystem r
  ) =>
  UserId ->
  UserGroupId ->
  Sem r (Maybe UserGroup)
getUserGroupImpl getter gid = runMaybeT $ do
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

getUserGroupsImpl ::
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
  Sem r UserGroupPage
getUserGroupsImpl getter searchString sortBy' sortOrder' mPageSize mLastGroupName mLastCreatedAt mLastGroupId = do
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
            searchString = searchString
          }
  UserGroupPage <$> Store.getUserGroups pageReq
  where
    ifNothing :: UserGroupSubsystemError -> Maybe a -> Sem r a
    ifNothing e = maybe (throw e) pure

updateGroupImpl ::
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
updateGroupImpl updater groupId groupUpdate = do
  team <- getTeamAsAdmin updater >>= note UserGroupNotATeamAdmin
  found <- isJust <$> Store.updateUserGroup team groupId groupUpdate
  if found
    then do
      admins <- fmap (^. TM.userId) . (^. teamMembers) <$> internalGetTeamAdmins team
      pushNotifications [mkEvent updater (UserGroupUpdated groupId) admins]
    else throw UserGroupNotFound

deleteGroupImpl ::
  ( Member UserSubsystem r,
    Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member NotificationSubsystem r,
    Member TeamSubsystem r
  ) =>
  UserId ->
  UserGroupId ->
  Sem r ()
deleteGroupImpl deleter groupId =
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

addUserImpl ::
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
addUserImpl adder groupId addeeId = do
  ug <- getUserGroupImpl adder groupId >>= note UserGroupNotFound
  team <- getTeamAsAdmin adder >>= note UserGroupNotATeamAdmin
  void $ internalGetTeamMember addeeId team >>= note UserGroupMemberIsNotInTheSameTeam
  unless (addeeId `elem` runIdentity ug.members) $ do
    Store.addUser groupId addeeId
    admins <- fmap (^. TM.userId) . (^. teamMembers) <$> internalGetTeamAdmins team
    pushNotifications
      [ mkEvent adder (UserGroupUpdated groupId) admins
      ]

removeUserImpl ::
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
removeUserImpl remover groupId removeeId = do
  ug <- getUserGroupImpl remover groupId >>= note UserGroupNotFound
  team <- getTeamAsAdmin remover >>= note UserGroupNotATeamAdmin
  void $ internalGetTeamMember removeeId team >>= note UserGroupMemberIsNotInTheSameTeam
  when (removeeId `elem` runIdentity ug.members) $ do
    Store.removeUser groupId removeeId
    admins <- fmap (^. TM.userId) . (^. teamMembers) <$> internalGetTeamAdmins team
    pushNotifications
      [ mkEvent remover (UserGroupUpdated groupId) admins
      ]
