{-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns -Wno-incomplete-patterns #-}

module Wire.UserGroupSubsystem.InterpreterSpec (spec) where

import Control.Lens ((.~), (^.))
import Data.Aeson qualified as A
import Data.Bifunctor (first)
import Data.Default
import Data.Domain (Domain (Domain))
import Data.Id
import Data.List.Extra
import Data.Map qualified as Map
import Data.Qualified (Local, toLocalUnsafe)
import Data.Vector qualified as V
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input (Input, runInputConst)
import Polysemy.Internal.Kind (Append)
import Polysemy.State
import System.Random (StdGen)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.Push.V2 (RecipientClients (RecipientClientsAll), Route (RouteAny))
import Wire.API.Team.Member
import Wire.API.Team.Member qualified as TM
import Wire.API.Team.Member qualified as TeamMember
import Wire.API.Team.Role
import Wire.API.User as User
import Wire.API.UserEvent
import Wire.API.UserGroup
import Wire.Arbitrary
import Wire.GalleyAPIAccess
import Wire.MockInterpreters as Mock
import Wire.NotificationSubsystem
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
       State UserGroupInMemState,
       Rnd.Random,
       State StdGen,
       Input (Local ()),
       NotificationSubsystem,
       State [Push],
       Error UserGroupSubsystemError
     ]
    a ->
  Either UserGroupSubsystemError a
runDependencies initialUsers initialTeams =
  run
    . runError
    . evalState mempty
    . inMemoryNotificationSubsystemInterpreter
    . runInputConst (toLocalUnsafe (Domain "example.com") ())
    . runInMemoryUserGroupStore def
    . miniGalleyAPIAccess initialTeams def
    . userSubsystemTestInterpreter initialUsers

runDependenciesWithReturnState ::
  [User] ->
  Map TeamId [TeamMember] ->
  Sem
    ( '[ UserSubsystem,
         GalleyAPIAccess
       ]
        `Append` EffectStack
        `Append` '[ Input (Local ()),
                    NotificationSubsystem,
                    State [Push],
                    Error UserGroupSubsystemError
                  ]
    )
    a ->
  Either UserGroupSubsystemError ([Push], a)
runDependenciesWithReturnState initialUsers initialTeams =
  run
    . runError
    . runState mempty
    . inMemoryNotificationSubsystemInterpreter
    . runInputConst (toLocalUnsafe (Domain "example.com") ())
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

  prop "only team admins and owners should get a group created notification" $ \team name (tm :: TeamMember) role ->
    let extraTeamMember = tm & permissions .~ rolePermissions role
        resultOrError =
          runDependenciesWithReturnState (allUsers team) (galleyTeamWithExtra team [extraTeamMember])
            . interpretUserGroupSubsystem
            $ do
              let nug = NewUserGroup {name = name, members = mempty}
              createGroup (ownerId team) nug

        expectedRecipient = Recipient (tm ^. TM.userId) RecipientClientsAll

        assertPushEvents :: UserGroup -> [Push] -> Property
        assertPushEvents ug [push] = case A.fromJSON @Event (A.Object push.json) of
          A.Success (UserGroupEvent (UserGroupCreated ugid)) ->
            push.origin === Just (ownerId team)
              .&&. push.transient === True
              .&&. push.route === RouteAny
              .&&. push.nativePriority === Nothing
              .&&. push.isCellsEvent === False
              .&&. push.conn === Nothing
              .&&. ugid === ug.id_
              .&&. ( case role of
                       RoleAdmin -> (expectedRecipient `elem` push.recipients) === True
                       RoleOwner -> (expectedRecipient `elem` push.recipients) === True
                       RoleMember -> (expectedRecipient `elem` push.recipients) === False
                       RoleExternalPartner -> (expectedRecipient `elem` push.recipients) === False
                   )
          _ -> counterexample ("Failed to decode push: " <> show push) False
     in case resultOrError of
          Left err -> counterexample ("Unexpected error: " <> show err) False
          Right (pushes, ug) -> assertPushEvents ug pushes

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
              Just (nonAdminUser, _) = find (\(_, mem) -> not $ isAdminOrOwner (mem ^. permissions)) team.members
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

  prop "key misses produce 404" $ \team groupId ->
    expectRight
      . runDependencies (allUsers team) (galleyTeam team)
      . interpretUserGroupSubsystem
      $ do
        mGroup <- getGroup (ownerId team) groupId
        pure $ mGroup === Nothing

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

        pure $
          ((.id_) <$> getGroupAdmin) === Just group1.id_
            .&&. ((.id_) <$> getGroupOutsider) === Nothing

  prop "team members can only get their own groups" $ \team userGroupName1 userGroupName2 ->
    let (memSet1, memSet2) = splitAt (length team.members `div` 2) (User.userId . fst <$> team.members)
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

            pure $
              getOwnGroup === Just group1
                .&&. getOtherGroup === Nothing

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
      not $ Imports.null team.members
    AtLeastOneNonAdmin -> flip suchThat \team ->
      any (\(_, mem) -> not $ isAdminOrOwner (mem ^. permissions)) team.members

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
    members :: [(User, TeamMember)]
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
allUsers t = fst <$> t.owner : t.members

ownerId :: ArbitraryTeam -> UserId
ownerId t = User.userId (fst t.owner)

-- | The Map is required by the mock GalleyAPIAccess
galleyTeam :: ArbitraryTeam -> Map TeamId [TeamMember]
galleyTeam t = galleyTeamWithExtra t []

galleyTeamWithExtra :: ArbitraryTeam -> [TeamMember] -> Map TeamId [TeamMember]
galleyTeamWithExtra t tm = Map.singleton t.tid $ tm <> map snd (t.owner : t.members)
