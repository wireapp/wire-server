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

data UserGroupSubsystemError
  = UserGroupCreatorIsNotATeamAdmin
  | UserGroupMemberIsNotInTheSameTeam
  deriving (Show, Eq)

userGroupSubsystemErrorToHttpError :: UserGroupSubsystemError -> HttpError
userGroupSubsystemErrorToHttpError =
  StdError . \case
    UserGroupCreatorIsNotATeamAdmin -> errorToWai @E.UserGroupCreatorIsNotATeamAdmin
    UserGroupMemberIsNotInTheSameTeam -> errorToWai @E.UserGroupMemberIsNotInTheSameTeam

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
  team <-
    note UserGroupCreatorIsNotATeamAdmin =<< runMaybeT do
      team <- MaybeT $ getUserTeam creator
      creatorTeamMember <- MaybeT $ getTeamMember creator team
      guard (isAdminOrOwner (creatorTeamMember ^. permissions))
      pure team
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
  let push =
        def
          { origin = Just creator,
            json = toJSONObject $ UserGroupEvent $ UserGroupCreated ug.id_,
            recipients = (\tm -> Recipient (tm ^. TM.userId) RecipientClientsAll) <$> admins ^. teamMembers,
            transient = True
          }
  pushNotifications [push]
  pure ug

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
