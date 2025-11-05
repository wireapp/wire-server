{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns -Wno-incomplete-patterns -Wno-orphans #-}

module Wire.ScimSubsystem.InterpreterSpec (spec) where

import Data.Id
import Data.Json.Util (toUTCTimeMillis)
import Data.Qualified
import Data.Text qualified as Text
import Data.UUID qualified as UUID
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
  [User] ->
  Map TeamId [TeamMember] ->
  Sem AllDependencies a ->
  Either ScimSubsystemError a
runDependencies initialUsers initialTeams =
  run
    . lowerLevelStuff
    . UGS.interpretUserGroupSubsystem
    . mockBrigAPIAccess initialUsers
    . runError
    . runInputConst (ScimSubsystemConfig scimBaseUri)
    . interpretScimSubsystem
  where
    scimBaseUri :: Common.URI
    scimBaseUri = Common.URI . fromJust . parseURI $ "http://nowhere.net/scim/v2"

    lowerLevelStuff :: InterpretersFor UGS.AllDependencies r
    lowerLevelStuff = crashOnLowerErrors . UGS.interpretDependencies initialUsers initialTeams
      where
        crashOnLowerErrors = fmap (either (error . show) id) . runError

    -- Mock BrigAPIAccess interpreter for tests
    mockBrigAPIAccess :: [User] -> InterpreterFor BrigAPIAccess r
    mockBrigAPIAccess users = interpret $ \case
      GetAccountsBy localGetBy -> do
        let getBy = tUnqualified localGetBy
        pure $ filter (\u -> User.userId u `elem` getBy.getByUserId) users
      CreateGroupFull managedBy _teamId _creatorUserId newGroup -> do
        -- For tests, just create a minimal UserGroup
        let gid = Id UUID.nil -- Using nil UUID for tests
        pure $
          UserGroup_
            { id_ = gid,
              name = newGroup.name,
              members = Identity newGroup.members,
              membersCount = Nothing,
              channels = Nothing,
              channelsCount = Nothing,
              managedBy = managedBy,
              createdAt = toUTCTimeMillis (read "2024-01-01 00:00:00 UTC")
            }
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
  describe "scimCreateUserGroup" $ do
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
              retrievedGroup :: Maybe UserGroup <- UGS.getGroup (UGS.ownerId team) createdGroup.thing.id False
              pure (createdGroup, retrievedGroup)
       in case resultOrError of
            Left err -> counterexample ("Left: " ++ show err) False
            Right (createdGroup, retrievedGroup) ->
              Just createdGroup.thing.id === ((.id_) <$> retrievedGroup)

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

  describe "getScimGroup" $ do
    it "retrieves metadata intact" $ do
      pendingWith "we actually haven't implemented metadata storage in store, because it was weird to test it without get."
