{-# LANGUAGE RecordWildCards #-}

module Wire.UserGroupSubsystem.Interpreter where

import Control.Error (MaybeT (..))
import Control.Lens ((^.))
import Data.Id
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.Team.Member
import Wire.API.User.Profile
import Wire.API.UserGroup
import Wire.Error
import Wire.GalleyAPIAccess
import Wire.UserGroupStore qualified as Store
import Wire.UserGroupSubsystem
import Wire.UserSubsystem (UserSubsystem, getUserTeam)

interpretUserGroupSubsystem ::
  ( Member UserSubsystem r,
    Member (Error UserGroupSubsystemError) r,
    Member Store.UserGroupStore r,
    Member GalleyAPIAccess r
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
    Member GalleyAPIAccess r
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
  for_ newGroup.members \member -> do
    memberTeam <- getUserTeam member -- TODO: pull all users in one db request.
    when (memberTeam /= Just team) $
      throw UserGroupMemberIsNotInTheSameTeam
  Store.createUserGroup team newGroup managedBy

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
