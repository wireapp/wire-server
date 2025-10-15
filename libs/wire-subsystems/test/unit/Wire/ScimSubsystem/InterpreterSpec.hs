{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns -Wno-incomplete-patterns -Wno-orphans #-}

module Wire.ScimSubsystem.InterpreterSpec (spec) where

import Data.Id
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
import Web.Scim.Schema.Common qualified as Common
import Web.Scim.Schema.Meta qualified as Common
import Wire.API.Team.Member as TM
import Wire.API.User as User
import Wire.API.User.Scim
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
  describe "createScimGroup" $ do
    prop "creates a group returns it" $ \(team :: UGS.ArbitraryTeam) (newScimGroup_ :: Group.Group) ->
      let newScimGroup =
            newScimGroup_
              { Group.members =
                  let all_ = UGS.allUsers team
                      nonscim_ = filter (\u -> u.userManagedBy /= ManagedByScim) all_
                   in mkScimGroupMember <$> nonscim_
              }
          resultOrError = do
            runDependencies (UGS.allUsers team) (UGS.galleyTeam team) $ do
              createdGroup :: Group.StoredGroup SparTag <- createScimGroup team.tid newScimGroup
              retrievedGroup :: Maybe UserGroup <- UGS.getGroup (UGS.ownerId team) createdGroup.thing.id False
              pure (createdGroup, retrievedGroup)
       in case resultOrError of
            Left err -> counterexample ("Left: " ++ show err) False
            Right (createdGroup, retrievedGroup) ->
              Just createdGroup.thing.id === ((.id_) <$> retrievedGroup)
