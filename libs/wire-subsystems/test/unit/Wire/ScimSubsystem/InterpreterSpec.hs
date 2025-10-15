{-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns -Wno-incomplete-patterns -Wno-unused-imports #-}

module Wire.ScimSubsystem.InterpreterSpec (spec) where

import Data.Default
import Data.Id
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal.Kind
import Polysemy.State
import Test.Hspec
import Test.Hspec.QuickCheck
import Wire.API.Team.Member as TM
import Wire.API.User as User
import Wire.GalleyAPIAccess
import Wire.ScimSubsystem
import Wire.ScimSubsystem.Interpreter
import Wire.TeamSubsystem
import Wire.UserGroupStore
import Wire.UserGroupSubsystem qualified as UGS
import Wire.UserGroupSubsystem.Interpreter qualified as UGS
import Wire.UserGroupSubsystem.InterpreterSpec qualified as UGS
import Wire.UserSubsystem qualified as US
import Wire.UserSubsystem.Interpreter qualified as US

type AllDependencies =
  [ ScimSubsystem,
    Input ScimSubsystemConfig,
    Error ScimSubsystemError,
    UGS.UserGroupSubsystem
  ]
    `Append` UGS.AllDependencies

_runDependenciesFailOnError :: (HasCallStack) => [User] -> Map TeamId [TeamMember] -> Sem AllDependencies (IO ()) -> IO ()
_runDependenciesFailOnError usrs team = either (error . ("no assertion: " <>) . show) id . runDependencies usrs team

runDependencies ::
  forall a.
  [User] ->
  Map TeamId [TeamMember] ->
  Sem AllDependencies a ->
  Either ScimSubsystemError a
runDependencies initialUsers initialTeams =
  -- Input ScimSubsystemConfig
  -- Error Scim...
  -- UGS.interpretUserGroupSubsystem
  -- run UGS.AllDepencies

  -- run . err . () . go
  run . lowerLevelStuff . UGS.interpretUserGroupSubsystem . runError . runInputConst def . interpretScimSubsystem
  where
    lowerLevelStuff :: InterpretersFor UGS.AllDependencies r
    lowerLevelStuff = fmap (fromRight todo) . runError . UGS.interpretDependencies initialUsers initialTeams

spec :: Spec
spec = UGS.timeoutHook $ describe "ScimSubsystem.Interpreter" $ \team newUserGroupName seed ->
  let rndShuffle xs gen = map fst $ sortOn snd $ zip xs (Rand.randoms gen :: [Int])
      -- ~half the users
      n = length (allUsers team) `div` 2 + 1
      members = take n $ rndShuffle (allUsers team) (Rand.mkStdGen seed)
      resultOrError =
        runDependenciesWithReturnState (allUsers team) (galleyTeam team)
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
