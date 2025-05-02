{-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns #-}

module Wire.UserGroupSubsystem.InterpreterSpec (spec) where

import Control.Lens ((.~), (^.))
import Data.Bifunctor (first)
import Data.Default
import Data.Id
import Data.List.Extra
import Data.Map qualified as Map
import Data.Vector qualified as V
import Imports
import Polysemy
import Polysemy.Error
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.Team.Member
import Wire.API.Team.Member qualified as TeamMember
import Wire.API.Team.Role
import Wire.API.User as User
import Wire.API.UserGroup
import Wire.Arbitrary
import Wire.GalleyAPIAccess
import Wire.MockInterpreters
import Wire.UserGroupStore (UserGroupStore)
import Wire.UserGroupSubsystem
import Wire.UserGroupSubsystem.Interpreter
import Wire.UserSubsystem (UserSubsystem)

runDependencies :: [User] -> Map TeamId [TeamMember] -> Sem '[UserSubsystem, GalleyAPIAccess, UserGroupStore, Error UserGroupSubsystemError] a -> Either UserGroupSubsystemError a
runDependencies initialUsers teams =
  run
    . runError
    . runInMemoryUserGroupStore
    . miniGalleyAPIAccess teams def
    . userSubsystemTestInterpreter initialUsers

expectRight :: (Show err) => Either err Property -> Property
expectRight = \case
  Left err -> counterexample ("Unexpected error: " <> show err) False
  Right p -> p

expectLeft :: (Show err, Eq err) => err -> Either err Property -> Property
expectLeft expectedErr = \case
  Left err -> err === expectedErr
  Right _ -> counterexample ("Expected error, but it didn't happen") False

unexpected :: Sem r Property
unexpected =
  pure $ counterexample "An error was expected to have occured by now" False

spec :: Spec
spec = describe "UserGroupSubsystem.Interpreter" do
  prop "team admins should be able to create and get groups" $ \team newUserGroupName ->
    expectRight
      . runDependencies (allUsers team) (galleyTeam team)
      . interpretUserGroupSubsystem
      $ do
        let newUserGroup =
              NewUserGroup
                { name = newUserGroupName,
                  members = User.userId <$> V.fromList (allUsers team)
                }
        createdGroup <- createGroup (ownerId team) newUserGroup
        retrievedGroup <- getGroup (ownerId team) createdGroup.id_
        pure $
          -- TODO: test createdAt when it's back.
          createdGroup.name === newUserGroupName
            .&&. createdGroup.members === newUserGroup.members
            .&&. createdGroup.managedBy === ManagedByWire
            .&&. Just createdGroup === retrievedGroup

  prop "only team admins should be able to create a group" $
    \((WithMods team) :: WithMods '[AtLeastOneNonAdmin] ArbitraryTeam) newUserGroupName ->
      expectLeft UserGroupCreatorIsNotATeamAdmin
        . runDependencies (allUsers team) (galleyTeam team)
        . interpretUserGroupSubsystem
        $ do
          let newUserGroup =
                NewUserGroup
                  { name = newUserGroupName,
                    members = User.userId <$> V.fromList (allUsers team)
                  }
              Just (nonAdminUser, _) = find (\(_, mem) -> not $ isAdminOrOwner (mem ^. permissions)) team.others
          void $ createGroup (User.userId nonAdminUser) newUserGroup
          unexpected

  prop "only team members are allowed in the group" $ \team otherUsers newUserGroupName ->
    let othersWithoutTeamMembers = filter (\u -> u.userTeam /= Just team.tid) otherUsers
     in notNull othersWithoutTeamMembers
          ==> expectLeft UserGroupMemberIsNotInTheSameTeam
            . runDependencies (allUsers team <> otherUsers) (galleyTeam team)
            . interpretUserGroupSubsystem
          $ do
            let newUserGroup =
                  NewUserGroup
                    { name = newUserGroupName,
                      members = User.userId <$> V.fromList otherUsers
                    }
            void $ createGroup (ownerId team) newUserGroup
            unexpected

  prop "team admins can get all groups in their team; outsiders can see nothing" $ \team otherTeam userGroupName ->
    expectRight
      . runDependencies (allUsers team) (galleyTeam team)
      . interpretUserGroupSubsystem
      $ do
        let newUserGroup =
              NewUserGroup
                { name = userGroupName,
                  members = V.empty
                }
        group1 <- createGroup (ownerId team) newUserGroup
        view1admin <- getGroup (ownerId team) group1.id_
        view1outsider <- getGroup (ownerId otherTeam) group1.id_
        pure $
          ((.id_) <$> view1admin) === Just group1.id_
            .&&. ((.id_) <$> view1outsider) === Nothing

  prop "team members can only get their own groups" $ \team userGroupName1 userGroupName2 ->
    let (memSet1, memSet2) = splitAt (length team.others `div` 2) (User.userId . fst <$> team.others)
     in all notNull [memSet1, memSet2]
          ==> expectRight
            . runDependencies (allUsers team) (galleyTeam team)
            . interpretUserGroupSubsystem
          $ do
            let newUserGroup1 =
                  NewUserGroup
                    { name = userGroupName1,
                      members = V.fromList memSet1
                    }
                newUserGroup2 =
                  NewUserGroup
                    { name = userGroupName2,
                      members = V.fromList memSet2
                    }
            group1 <- createGroup (ownerId team) newUserGroup1
            group2 <- createGroup (ownerId team) newUserGroup2
            view1memSet1 <- getGroup (head memSet1) group1.id_
            view2memSet1 <- getGroup (head memSet1) group2.id_
            pure $
              ((.id_) <$> view1memSet1) === Just group1.id_
                .&&. ((.id_) <$> view2memSet1) === Nothing

data TeamGenMod = AtLeastOneMember | AtLeastOneNonAdmin

class KnownTeamGenMod a where
  teamGenMod :: TeamGenMod

instance KnownTeamGenMod 'AtLeastOneMember where
  teamGenMod = AtLeastOneMember

instance KnownTeamGenMod 'AtLeastOneNonAdmin where
  teamGenMod = AtLeastOneNonAdmin

applyConstraint :: forall mod. (KnownTeamGenMod mod) => Gen ArbitraryTeam -> Gen ArbitraryTeam
applyConstraint =
  case teamGenMod @mod of
    AtLeastOneMember -> flip suchThat \team ->
      not $ Imports.null team.others
    AtLeastOneNonAdmin -> flip suchThat \team ->
      any (\(_, mem) -> not $ isAdminOrOwner (mem ^. permissions)) team.others

newtype WithMods (mods :: [TeamGenMod]) a = WithMods a
  deriving (Show, Eq)

class ArbitraryWithMods mods a where
  arbitraryWithMods :: Gen a

instance (Arbitrary a) => ArbitraryWithMods '[] a where
  arbitraryWithMods = arbitrary

instance (KnownTeamGenMod mod, ArbitraryWithMods mods ArbitraryTeam) => ArbitraryWithMods (mod ': mods) ArbitraryTeam where
  arbitraryWithMods =
    applyConstraint @mod $ arbitraryWithMods @mods

instance (ArbitraryWithMods mods a) => Arbitrary (WithMods mods a) where
  arbitrary = WithMods <$> arbitraryWithMods @mods

data ArbitraryTeam = ArbitraryTeam
  { tid :: TeamId,
    owner :: (User, TeamMember),
    others :: [(User, TeamMember)]
  }
  deriving (Show, Eq)

instance Arbitrary ArbitraryTeam where
  arbitrary = do
    tid <- arbitrary
    let assignTeam u = u {userTeam = Just tid}
    adminUser <- assignTeam <$> arbitrary
    adminMember <-
      arbitrary @TeamMember
        <&> (permissions .~ rolePermissions RoleOwner)
        <&> (TeamMember.userId .~ User.userId adminUser)
    otherUsers <- listOf' arbitrary
    otherUserWithMembers <- for otherUsers $ \u -> do
      mem <- arbitrary
      pure (u, mem & TeamMember.userId .~ User.userId u)
    pure . ArbitraryTeam tid (adminUser, adminMember) $ map (first assignTeam) otherUserWithMembers

allUsers :: ArbitraryTeam -> [User]
allUsers t = fst <$> t.owner : t.others

ownerId :: ArbitraryTeam -> UserId
ownerId t = User.userId (fst t.owner)

-- | The Map is required by the mock GalleyAPIAccess
galleyTeam :: ArbitraryTeam -> Map TeamId [TeamMember]
galleyTeam t = Map.singleton t.tid . map snd $ t.owner : t.others
