{-# LANGUAGE RecordWildCards #-}

module Wire.UserGroupSubsystem.Interpreter where

import Control.Error (MaybeT (..))
import Control.Lens ((^.))
import Data.Id
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.Team.Member
import Wire.API.User.Profile
import Wire.API.UserGroup
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
  GetGroup getter gid -> undefined getter gid

data UserGroupSubsystemError
  = UserGroupCreatorIsNotATeamAdmin
  | UserGroupMemberIsNotInTheSameTeam
  deriving (Show, Eq)

-- TODO: check that creator is admin and all members are part of the team
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
    memberTeam <- getUserTeam member
    when (memberTeam /= Just team) $
      throw UserGroupMemberIsNotInTheSameTeam
  id_ <- Store.createUserGroup team newGroup managedBy
  pure $
    UserGroup
      { name = newGroup.name,
        members = newGroup.members,
        ..
      }

-- getUserGroupImpl :: UserId -> GroupId -> Sem r UserGroup
-- getUserGroupImpl getter gid =
--   team <- getUserTeam getter
--    >>= note
