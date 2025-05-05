{-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns #-}

module Wire.UserGroupSubsystem.InterpreterSpec (spec) where

import Control.Lens ((.~), (^.))
import Data.Bifunctor (first)
import Data.Default
import Data.Id
import Data.List.Extra
import Data.Map qualified as Map
import Data.Qualified
import Data.String.Conversions (cs)
-- import Data.Time
-- import Data.UUID qualified as UUID
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
    . runInMemoryUserGroupStore def
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
        now <- (.now) <$> get
        pure $
          createdGroup.name === newUserGroupName
            .&&. createdGroup.members === newUserGroup.members
            .&&. createdGroup.managedBy === ManagedByWire
            .&&. createdGroup.createdAt === now
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
    -- TODO
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

        getGroupAdmin <- getGroup (ownerId team) group1.id_
        getGroupOutsider <- getGroup (ownerId otherTeam) group1.id_

        getGroupsAdmin <- getGroups (ownerId team) Nothing Nothing
        getGroupsOutsider <- getGroups (ownerId otherTeam) Nothing Nothing

        pure $
          ((.id_) <$> getGroupAdmin) === Just group1.id_
            .&&. ((.id_) <$> getGroupOutsider) === Nothing
            .&&. ((.id_) <$> getGroupsAdmin.page) === [group1.id_]
            .&&. getGroupsOutsider.page === []

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

            -- user from group 1 wants to see both group1 and group2
            getOwnGroup <- getGroup (head memSet1) group1.id_
            getOtherGroup <- getGroup (head memSet1) group2.id_
            getAllGroups <- getGroups (head memSet1) Nothing Nothing

            pure $
              ((.id_) <$> getOwnGroup) === Just group1.id_ -- TODO: remove .id_ everywhere in this block.
                .&&. ((.id_) <$> getOtherGroup) === Nothing
                .&&. ((.id_) <$> getAllGroups.page) === [group1.id_]

  describe "GetGroups :: UserId -> Maybe Int -> Maybe UUID -> UserGroupSubsystem m UserGroupPage" $ do
    let nugs = [1 .. 15] <&> \(i :: Int) -> NewUserGroup (cs $ show i) mempty

        check :: UserGroupPage -> [UserGroupId] -> Bool -> Property
        check have wantPage wantHasMore = (have.page <&> (.id_)) === wantPage .&&. have.hasMore === wantHasMore

        lastKeyOf :: UserGroupPage -> UserGroupId
        lastKeyOf = Id . toUUID . (.id_) . last . (.page)

    prop "pagination (static case): eventually gives you the entire data set (nothing gets removed or duplicated)" $
      \team ->
        expectRight
          . runDependencies (allUsers team) (galleyTeam team)
          . interpretUserGroupSubsystem
          $ do
            let owner = qUnqualified (fst team.owner).userQualifiedId
            gids <- (sort . ((.id_) <$>)) <$> createGroup owner `mapM` nugs
            allOfThem <- getGroups owner Nothing Nothing
            first3 <- getGroups owner (Just 3) Nothing
            next4 <- getGroups owner (Just 4) (Just $ lastKeyOf first3)

            pure $
              check allOfThem gids False
                .&&. check first3 (take 3 gids) True
                .&&. check next4 (take 4 (drop 3 gids)) True

    prop "paginates well under updates" $ do
      \team ->
        expectRight
          . runDependencies (allUsers team) (galleyTeam team)
          . interpretUserGroupSubsystem
          $ do
            let owner = qUnqualified (fst team.owner).userQualifiedId
            gids <- (sort . ((.id_) <$>)) <$> createGroup owner `mapM` nugs

            -- read first page
            beforeNewCreate <- getGroups owner (Just 8) Nothing
            -- mess with database
            newGroup <- createGroup owner (NewUserGroup "the new one" mempty)
            -- read second page
            afterNewCreate <- getGroups owner Nothing (Just $ lastKeyOf beforeNewCreate)

            pure $
              check beforeNewCreate (take 8 gids) True
                .&&. let afterGids = drop 8 (sort (newGroup.id_ : gids))
                      in check afterNewCreate afterGids False

{-
  describe "UpdateGroup :: UserId -> UserGroupId -> UserGroupUpdate -> UserGroupSubsystem m (Maybe UserGroup)" $ do
    prop "updateGroup updates the name" $ \originalName userGroupUpdate ->
      let now = unsafePerformIO getCurrentTime
       in (runDependencies mempty mempty)
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
       in runDependencies mempty mempty do
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
       in runDependencies mempty mempty do
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
       in runDependencies mempty mempty do
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

-}

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
    others :: [(User, TeamMember)] -- TODO: rename to "members"?
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
