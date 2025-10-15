{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns -Wno-incomplete-patterns -Wno-orphans #-}

module Wire.ScimSubsystem.InterpreterSpec (spec) where

import Data.Id
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
import Wire.API.Team.Member as TM
import Wire.API.User as User
import Wire.API.UserGroup
import Wire.ScimSubsystem
import Wire.ScimSubsystem.Interpreter
import Wire.UserGroupSubsystem qualified as UGS
import Wire.UserGroupSubsystem.Interpreter qualified as UGS
import Wire.UserGroupSubsystem.InterpreterSpec qualified as UGS

type AllDependencies =
  [ ScimSubsystem,
    Input ScimSubsystemConfig,
    Error ScimSubsystemError,
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

instance Arbitrary Group.Group where
  arbitrary = do
    mems <- listOf1 arbitrary
    pure
      Group.Group
        { schemas = ["urn:ietf:params:scim:schemas:core:2.0:Group"],
          displayName = "some scim user group",
          members = mems
        }

instance Arbitrary Group.Member where
  arbitrary = do
    value <- arbitrary
    let typ = "User"
    let ref = "http://.../scim/vs/Users/463c0b40-a9d9-11f0-b091-181dea96dd7"
    pure Group.Member {..}

spec :: Spec
spec = UGS.timeoutHook $ describe "ScimSubsystem.Interpreter" $ do
  focus . describe "createScimGroup" $ do
    prop "creates a group returns it" $ \team newUserGroup ->
      let resultOrError =
            runDependencies (UGS.allUsers team) (UGS.galleyTeam team) $ do
              createdGroup <- createScimGroup team.tid newUserGroup
              retrievedGroup :: Maybe UserGroup <- UGS.getGroup (UGS.ownerId team) createdGroup.thing.id False
              pure (createdGroup, retrievedGroup)
       in case resultOrError of
            Left err -> counterexample ("Left: " ++ show err) False
            Right (createdGroup, retrievedGroup) ->
              Just createdGroup.thing.id === ((.id_) <$> retrievedGroup)

{-
  let rndShuffle xs gen = map fst $ sortOn snd $ zip xs (Rand.randoms gen :: [Int])
      -- ~half the users
      n = length (allUsers team) `div` 2 + 1
      members = take n $ rndShuffle (allUsers team) (Rand.mkStdGen seed)
      resultOrError =
        runDependencies (allUsers team) (galleyTeam team) scimBaseUrl
          . interpretUserGroupSubsystem
          $ do
            let newUserGroup =
                  NewUserGroup
                    { name = newUserGroupName,
                      members = User.userId <$> V.fromList members
                    }
            createdGroup <- createGroup (ownerId team) newUserGroup
            retrievedGroup <- getGroup (ownerId team) createdGroup.id_ False
            now <- toUTCTimeMillis <$> get
            let assert =
                  createdGroup.name
                    === newUserGroupName
                      .&&. createdGroup.members
                    === Identity newUserGroup.members
                      .&&. createdGroup.managedBy
                    === ManagedByWire
                      .&&. createdGroup.createdAt
                    === now
                      .&&. Just createdGroup
                    === retrievedGroup
            pure (createdGroup, assert)

      assertPushEvents :: UserGroup -> [Push] -> Property
      assertPushEvents ug pushes =
        foldl
          ( \acc push ->
              acc .&&. case A.fromJSON @Event (A.Object push.json) of
                A.Success (UserGroupEvent (UserGroupCreated ugid)) ->
                  ugid
                    === ug.id_
                      .&&. push.origin
                    === Just (ownerId team)
                _ -> counterexample ("Failed to decode push: " <> show push) False
          )
          (length pushes === 1)
          pushes
   in case resultOrError of
          Left err -> counterexample ("Unexpected error: " <> show err) False
          Right (pushes, (ug, propertyCheck)) -> assertPushEvents ug pushes .&&. propertyCheck
        prop
        "created groups can be found in team-management, with managed_by == scim"
        $ \(_ :: Bool) -> True
          prop
          "refuses to create scim groups with non-scim users"
          $ \(_ :: Bool) -> True
-}
