{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns -Wno-incomplete-patterns -Wno-orphans #-}

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

module Wire.ScimSubsystem.InterpreterSpec (spec) where

import Data.Id
import Data.Json.Util
import Data.Text qualified as Text
import Imports
import Network.URI
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal.Kind
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Web.Scim.Class.Group qualified as Group
import Web.Scim.Class.Group qualified as SCG
import Web.Scim.Schema.Common qualified as Common
import Web.Scim.Schema.Meta qualified as Common
import Wire.API.Routes.Internal.Brig (GetBy (..))
import Wire.API.Team.Member as TM
import Wire.API.User as User
import Wire.API.User.Scim
import Wire.API.UserGroup
import Wire.BrigAPIAccess (BrigAPIAccess (..))
import Wire.ScimSubsystem
import Wire.ScimSubsystem.Interpreter
import Wire.UserGroupSubsystem qualified as UGS
import Wire.UserGroupSubsystem.Interpreter qualified as UGS
import Wire.UserGroupSubsystem.InterpreterSpec qualified as UGS

type AllDependencies =
  [ ScimSubsystem,
    Input ScimSubsystemConfig,
    Error ScimSubsystemError,
    BrigAPIAccess,
    UGS.UserGroupSubsystem
  ]
    `Append` UGS.AllDependencies

runDependencies ::
  forall a.
  (HasCallStack) =>
  [User] ->
  Map TeamId [TeamMember] ->
  Sem AllDependencies a ->
  Either ScimSubsystemError a
runDependencies initialUsers initialTeams =
  either (error . show) id . runDependenciesSafe initialUsers initialTeams

runDependenciesSafe ::
  forall a.
  (HasCallStack) =>
  [User] ->
  Map TeamId [TeamMember] ->
  Sem AllDependencies a ->
  Either UGS.UserGroupSubsystemError (Either ScimSubsystemError a)
runDependenciesSafe initialUsers initialTeams =
  run
    . runError
    . UGS.interpretDependencies initialUsers initialTeams
    . UGS.interpretUserGroupSubsystem
    . mockBrigAPIAccess initialUsers
    . runError
    . runInputConst (ScimSubsystemConfig scimBaseUri)
    . interpretScimSubsystem
  where
    scimBaseUri :: Common.URI
    scimBaseUri = Common.URI . fromJust . parseURI $ "http://nowhere.net/scim/v2"

    -- Mock BrigAPIAccess interpreter for tests
    mockBrigAPIAccess :: (Member UGS.UserGroupSubsystem r) => [User] -> InterpreterFor BrigAPIAccess r
    mockBrigAPIAccess users = interpret $ \case
      CreateGroupInternal managedBy teamId creatorUserId newGroup -> do
        Right <$> UGS.createGroupInternal managedBy teamId creatorUserId newGroup
      GetAccountsBy getBy -> do
        pure $ filter (\u -> User.userId u `elem` getBy.getByUserId) users
      GetGroupInternal tid gid False -> do
        UGS.getGroupInternal tid gid False
      DeleteGroupInternal managedBy teamId groupId ->
        UGS.deleteGroupManaged managedBy teamId groupId $> Right ()
      _ -> error "Unimplemented BrigAPIAccess operation in mock"

instance Arbitrary Group.Group where
  arbitrary = do
    name <- Text.pack . take 4000 <$> ((:) <$> arbitrary <*> arbitrary)
    pure
      Group.Group
        { schemas = ["urn:ietf:params:scim:schemas:core:2.0:Group"],
          displayName = name,
          members = []
        }

mkScimGroupMember :: User -> Group.Member
mkScimGroupMember (idToText . User.userId -> value) =
  let typ = "User"
      ref = "$schema://$host.$domain/scim/vs/Users/$uuid" -- not a real URI, just a string for testing.
   in Group.Member {..}

spec :: Spec
spec = UGS.timeoutHook $ describe "ScimSubsystem.Interpreter" $ do
  describe "scimCreateUserGroup, scimGetUserGroup" $ do
    prop "creates a group returns it" $ \(team :: UGS.ArbitraryTeam) (newScimGroup_ :: Group.Group) ->
      let newScimGroup =
            newScimGroup_
              { Group.members =
                  let scimMembers = filter (\u -> u.userManagedBy == ManagedByScim) (UGS.allUsers team)
                   in mkScimGroupMember <$> scimMembers
              }
          resultOrError = do
            runDependencies (UGS.allUsers team) (UGS.galleyTeam team) $ do
              createdGroup :: Group.StoredGroup SparTag <- scimCreateUserGroup team.tid newScimGroup
              retrievedGroupScimAPI :: SCG.StoredGroup SparTag <- scimGetUserGroup team.tid createdGroup.thing.id
              retrievedGroupPublicAPI :: Maybe UserGroup <- UGS.getGroup (UGS.ownerId team) createdGroup.thing.id False
              pure (createdGroup, retrievedGroupScimAPI, retrievedGroupPublicAPI)
       in case resultOrError of
            Left err -> counterexample ("Left: " ++ show err) False
            Right (createdGroup, retrievedGroupScimAPI, retrievedGroupPublicAPI) ->
              createdGroup === retrievedGroupScimAPI
                .&&. case retrievedGroupPublicAPI of
                  Nothing -> counterexample "*** group not found over public api" False
                  Just grp ->
                    createdGroup.thing.id === grp.id_
                      .&&. createdGroup.thing.value.displayName === userGroupNameToText grp.name
                      .&&. Nothing === grp.channels
                      .&&. Nothing === grp.membersCount
                      .&&. Nothing === grp.channelsCount
                      .&&. ManagedByScim === grp.managedBy
                      .&&. toUTCTimeMillis createdGroup.meta.created === grp.createdAt
                      .&&. ( nub ((.typ) <$> createdGroup.thing.value.members)
                               === ["User" | not $ null createdGroup.thing.value.members]
                           )
                      .&&. ( sort ((.value) <$> createdGroup.thing.value.members)
                               === sort (toList $ idToText <$> runIdentity grp.members)
                           )

    prop "does not allow non-scim members" $ \team newScimGroup_ -> do
      let newScimGroup = newScimGroup_ {Group.members = mkScimGroupMember <$> groupMembers}
          groupMembers = take 2 (UGS.allUsers team)
          have =
            runDependencies (UGS.allUsers team) (UGS.galleyTeam team) $ do
              scimCreateUserGroup team.tid newScimGroup

          want =
            if all (\u -> u.userManagedBy == ManagedByScim) groupMembers
              then isRight
              else isLeft
      unless (want have) do
        expectationFailure . show $ ((.userManagedBy) <$> UGS.allUsers team)

  describe "scimDeleteUserGroup" $ do
    prop "deletes a SCIM-managed group" $ \(team :: UGS.ArbitraryTeam) (newScimGroup_ :: Group.Group) ->
      let newScimGroup =
            newScimGroup_
              { Group.members =
                  let scimUsers = filter (\u -> u.userManagedBy == ManagedByScim) (UGS.allUsers team)
                   in mkScimGroupMember <$> scimUsers
              }
          resultOrError = do
            runDependencies (UGS.allUsers team) (UGS.galleyTeam team) $ do
              -- Create a SCIM group
              createdGroup :: Group.StoredGroup SparTag <- scimCreateUserGroup team.tid newScimGroup
              -- Delete it
              scimDeleteUserGroup team.tid createdGroup.thing.id
              -- Verify it's gone
              retrievedGroup :: Maybe UserGroup <- UGS.getGroup (UGS.ownerId team) createdGroup.thing.id False
              pure retrievedGroup
       in case resultOrError of
            Left err -> counterexample ("Left: " ++ show err) False
            Right retrievedGroup -> retrievedGroup === Nothing

    it "fails to delete non-SCIM-managed groups" $ do
      team :: UGS.ArbitraryTeam <- generate arbitrary
      let ugName = either (error . show) id $ userGroupNameFromText "test-group"
      let newGroup = NewUserGroup {name = ugName, members = mempty}
      let have =
            runDependenciesSafe (UGS.allUsers team) (UGS.galleyTeam team) $ do
              -- Create non-SCIM group
              group' <- UGS.createGroup (UGS.ownerId team) newGroup
              -- Try to delete via SCIM (should fail)
              scimDeleteUserGroup team.tid group'.id_
      have `shouldSatisfy` isLeft

    it "works to delete non-existent groups" $ do
      team :: UGS.ArbitraryTeam <- generate arbitrary
      randomGroupId <- generate arbitrary
      let have =
            runDependenciesSafe (UGS.allUsers team) (UGS.galleyTeam team) $ do
              scimDeleteUserGroup team.tid randomGroupId
      have `shouldSatisfy` isRight
