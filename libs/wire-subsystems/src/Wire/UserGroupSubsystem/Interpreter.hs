module Wire.UserGroupSubsystem.Interpreter where

import Control.Error (MaybeT (..))
import Control.Lens ((^.))
import Data.Default
import Data.Id
import Data.Json.Util (ToJSONObject (toJSONObject))
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
import Wire.Error
import Wire.GalleyAPIAccess
import Wire.NotificationSubsystem
import Wire.UserGroupStore qualified as Store
import Wire.UserGroupSubsystem
import Wire.UserSubsystem (UserSubsystem, getLocalUserProfiles, getUserTeam)

interpretUserGroupSubsystem ::
  ( Member UserSubsystem r,
    Member (Error UserGroupSubsystemError) r,
    Member Store.UserGroupStore r,
    Member GalleyAPIAccess r,
    Member (Input (Local ())) r,
    Member NotificationSubsystem r
  ) =>
  InterpreterFor UserGroupSubsystem r
interpretUserGroupSubsystem = interpret $ \case
  CreateGroup creator newGroup -> createUserGroupImpl creator newGroup
  GetGroup getter gid -> getUserGroupImpl getter gid
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
    Member GalleyAPIAccess r,
    Member (Input (Local ())) r,
    Member NotificationSubsystem r
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
  admins <- getTeamAdmins team
  pushNotifications [mkEvent creator (UserGroupCreated ug.id_) admins]
  pure ug

getTeamAsAdmin ::
  ( Member UserSubsystem r,
    Member GalleyAPIAccess r
  ) =>
  UserId ->
  Sem r (Maybe TeamId)
getTeamAsAdmin user = runMaybeT do
  (team, member) <- MaybeT $ getTeamAsMember user
  guard (isAdminOrOwner (member ^. permissions))
  pure team

getTeamAsMember ::
  ( Member UserSubsystem r,
    Member GalleyAPIAccess r
  ) =>
  UserId ->
  Sem r (Maybe (TeamId, TeamMember))
getTeamAsMember memberId = runMaybeT do
  team <- MaybeT $ getUserTeam memberId
  mbr <- MaybeT $ getTeamMember memberId team
  pure (team, mbr)

mkEvent :: UserId -> UserGroupEvent -> TeamMemberList -> Push
mkEvent author evt recipients =
  def
    { origin = Just author,
      json = toJSONObject $ UserGroupEvent evt,
      recipients = (\tm -> Recipient (tm ^. TM.userId) RecipientClientsAll) <$> recipients ^. teamMembers,
      transient = True
    }

qualifyLocal :: (Member (Input (Local ())) r) => a -> Sem r (Local a)
qualifyLocal a = do
  l <- input
  pure $ qualifyAs l a

getUserGroupImpl ::
  ( Member UserSubsystem r,
    Member Store.UserGroupStore r,
    Member GalleyAPIAccess r
  ) =>
  UserId ->
  UserGroupId ->
  Sem r (Maybe UserGroup)
getUserGroupImpl getter gid = runMaybeT $ do
  team <- MaybeT $ getUserTeam getter
  getterCanSeeAll <- do
    creatorTeamMember <- MaybeT $ getTeamMember getter team
    pure . isAdminOrOwner $ creatorTeamMember ^. permissions
  userGroup <- MaybeT $ Store.getUserGroup team gid
  if getterCanSeeAll || getter `elem` (toList userGroup.members)
    then pure userGroup
    else MaybeT $ pure Nothing

updateGroupImpl ::
  ( Member UserSubsystem r,
    Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member NotificationSubsystem r,
    Member GalleyAPIAccess r
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
      admins <- getTeamAdmins team
      pushNotifications [mkEvent updater (UserGroupUpdated groupId) admins]
    else throw UserGroupNotFound

deleteGroupImpl ::
  ( Member UserSubsystem r,
    Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member NotificationSubsystem r,
    Member GalleyAPIAccess r
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
              admins <- getTeamAdmins team
              pushNotifications [mkEvent deleter (UserGroupDeleted groupId) admins]
            else
              throw UserGroupNotFound
        else do
          throw UserGroupNotATeamAdmin

addUserImpl ::
  ( Member UserSubsystem r,
    Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member GalleyAPIAccess r
  ) =>
  UserId ->
  UserGroupId ->
  UserId ->
  Sem r ()
addUserImpl adder groupId addeeId = do
  void $ getUserGroupImpl adder groupId >>= note UserGroupNotFound
  team <- getTeamAsAdmin adder >>= note UserGroupNotATeamAdmin
  void $ getTeamMember addeeId team >>= note UserGroupMemberIsNotInTheSameTeam
  Store.addUser groupId addeeId

removeUserImpl ::
  ( Member UserSubsystem r,
    Member Store.UserGroupStore r,
    Member (Error UserGroupSubsystemError) r,
    Member GalleyAPIAccess r
  ) =>
  UserId ->
  UserGroupId ->
  UserId ->
  Sem r ()
removeUserImpl remover groupId removeeId = do
  void $ getUserGroupImpl remover groupId >>= note UserGroupNotFound
  team <- getTeamAsAdmin remover >>= note UserGroupNotATeamAdmin
  void $ getTeamMember removeeId team >>= note UserGroupMemberIsNotInTheSameTeam
  Store.removeUser groupId removeeId
