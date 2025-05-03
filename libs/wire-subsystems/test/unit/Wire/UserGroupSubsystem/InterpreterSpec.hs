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
import Polysemy.State
import System.Random (StdGen)
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
import Wire.MockInterpreters as Mock
import Wire.Sem.Random qualified as Rnd
import Wire.UserGroupStore (UserGroupStore)
import Wire.UserGroupSubsystem
import Wire.UserGroupSubsystem.Interpreter
import Wire.UserSubsystem (UserSubsystem)

runDependencies ::
  [User] ->
  Map TeamId [TeamMember] ->
  Sem
    '[ UserSubsystem,
       GalleyAPIAccess,
       UserGroupStore,
       State MockState,
       Rnd.Random,
       State StdGen,
       Error UserGroupSubsystemError
     ]
    a ->
  Either UserGroupSubsystemError a
runDependencies initialUsers initialTeams =
  run
    . runError
    . runInMemoryUserGroupStore
    . miniGalleyAPIAccess initialTeams def
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
  -- TODO: describe "CreateGroup :: UserId -> NewUserGroup -> UserGroupSubsystem m UserGroup"

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
          createdGroup.name === newUserGroupName
            .&&. createdGroup.members === newUserGroup.members
            .&&. createdGroup.managedBy === ManagedByWire
            -- .&&. (ug.createdAt === now) -- TODO: test createdAt when it's back.
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

  -- TODO: describe "GetGroup :: UserId -> UserGroupId -> UserGroupSubsystem m (Maybe UserGroup)"

  it "key misses produce 404" $ do
    pendingWith "TODO"

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

  describe "GetGroups :: UserId -> Maybe Int -> Maybe UUID -> UserGroupSubsystem m UserGroupPage" $ do
    prop "team admins can get all groups in their team; outsiders can see nothing" $
      \_team _otherTeam _userGroupName -> False === True

    prop "team members can only get their own groups" $
      \_team _userGroupName1 _userGroupName2 -> False === True

    let -- now = unsafePerformIO getCurrentTime
        nugs = [1 .. 15] <&> \(i :: Int) -> NewUserGroup (cs $ show i) []

        check :: (HasCallStack) => UserGroupPage -> [UserGroupId] -> Bool -> Expectation
        check have wantPage wantHasMore = do
          (have.page <&> (.id_)) `shouldBe` wantPage
          have.hasMore `shouldBe` wantHasMore

        lastKeyOf :: UserGroupPage -> UUID
        lastKeyOf = toUUID . (.id_) . last . (.page)

    prop "pagination (static case): eventually gives you the entire data set (nothing gets removed or duplicated)" $
      \team userGroups ->
        Mock.runInMemoryUserGroupSubsystem now do
          gids <- (sort . ((.id_) <$>)) <$> createGroup `mapM` nugs
          allOfThem <- getGroups Nothing Nothing
          first3 <- getGroups (Just 3) Nothing
          next4 <- getGroups (Just 4) (Just $ lastKeyOf first3)
          halfPage1 <- getGroups (Just 100) Nothing
          halfPage2 <- getGroups (Just 100) (Just $ lastKeyOf next4)

          pure do
            check allOfThem gids False
            check first3 (take 3 gids) True
            check next4 (take 4 (drop 3 gids)) True
            check halfPage1 gids False
            check halfPage2 (drop 7 gids) False

    it "paginates well under updates" $ do
      Mock.runInMemoryUserGroupSubsystem now do
        gids <- (sort . ((.id_) <$>)) <$> createGroup `mapM` nugs
        beforeNewCreate <- getGroups (Just 8) Nothing
        newGroup <- createGroup (NewUserGroup "the new one" [])
        afterNewCreate <- getGroups (Just 8) (Just $ lastKeyOf beforeNewCreate)

        pure do
          check beforeNewCreate (take 8 gids) True

          let afterGids =
                if lastKeyOf beforeNewCreate < toUUID newGroup.id_
                  then drop 8 (sort (newGroup.id_ : gids))
                  else drop 8 gids
          check afterNewCreate afterGids False

    -- (...  or write this property?)
    prop "pagination (dynamic case): when writing and removing between getting pages, result is as good as we can expect" $ \() ->
      -- take the initial table, apply some removes and adds (which ones?), and you should get what you also get from paginating
      False === True

  describe "UpdateGroup :: UserId -> UserGroupId -> UserGroupUpdate -> UserGroupSubsystem m (Maybe UserGroup)" $ do
    prop "updateGroup updates the name" $ \originalName userGroupUpdate ->
      let now = unsafePerformIO getCurrentTime
       in Mock.runInMemoryUserGroupSubsystem
            now
            do
              ug0 <- createGroup (NewUserGroup originalName [])
              ug1 <- getGroup ug0.id_
              ug2 <- updateGroup ug0.id_ userGroupUpdate
              ug3 <- getGroup ug0.id_
              pure $
                (ug1 === Just ug0)
                  .&&. (ug2 === Just (ug0 {name = userGroupUpdate.name} :: UserGroup))
                  .&&. (ug3 === ug2)
            prop
            "only team admins can update user groups"
            $ \() -> False
              === True
                prop
                "only team members are allowed in the group"
              $ \() -> False === True

  describe "DeleteGroup :: UserId -> UserGroupId -> UserGroupSubsystem m ()" $ do
    prop "deleteGroup deletes" $ \newGroup1 newGroup2 -> do
      let now = unsafePerformIO getCurrentTime
       in Mock.runInMemoryUserGroupSubsystem now do
            ug1 <- createGroup newGroup1
            ug2 <- createGroup newGroup2
            deleteGroup ug1.id_
            allGroups <- (.page) <$> getGroups Nothing Nothing

            deleteGroup (Id UUID.nil) -- idempotency
            allGroups' <- (.page) <$> getGroups Nothing Nothing

            pure $ do
              allGroups `shouldBe` [ug2]
              allGroups' `shouldBe` [ug2]

      prop "only team admins can delete user groups" $ \() -> False === True

  describe "AddUser, RemoveUser :: UserId -> UserGroupId -> UserId -> UserGroupSubsystem m ()" $ do
    prop "addUser adds a user" $ \newGroup newUserId -> do
      -- TODO: how do we feel about dangling user ids?  maybe that should be handled on another
      -- level, and UserGroupSubsystem should be oblivious to what user ids point to?
      let now = unsafePerformIO getCurrentTime
       in Mock.runInMemoryUserGroupSubsystem now do
            ug :: UserGroup <- createGroup newGroup
            addUser ug.id_ newUserId
            ug' :: Maybe UserGroup <- getGroup ug.id_
            addUser ug.id_ newUserId -- idempotency
            ug'' :: Maybe UserGroup <- getGroup ug.id_
            pure $ do
              ((sort . (.members)) <$> ug') `shouldBe` Just (sort (newUserId : ug.members))
              ug'' `shouldBe` ug'

    prop "removeUser removes a user" $ \newGroup -> do
      let now = unsafePerformIO getCurrentTime
       in Mock.runInMemoryUserGroupSubsystem now do
            ug :: UserGroup <- createGroup newGroup
            let removee :: UserId
                removee = case ug.members of
                  [] -> Id UUID.nil -- idempotency
                  _ : _ -> ug.members !! (length ug.members `div` 2)
            removeUser ug.id_ removee
            ug' :: Maybe UserGroup <- getGroup ug.id_
            pure $ do
              ((sort . (.members)) <$> ug') `shouldBe` Just (sort (ug.members \\ [removee]))

      prop "only team admins can and remove users to groups" $ \() -> False === True

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
