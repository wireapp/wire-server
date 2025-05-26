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
import Data.Qualified
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input (Input, runInputConst)
import Polysemy.Internal.Kind (Append)
import Polysemy.State
import System.Random (StdGen)
import System.Timeout (timeout)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.Push.V2 (RecipientClients (RecipientClientsAll), Route (RouteAny))
import Wire.API.Team.Member as TM
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
  pure $ counterexample "An error was expected to have occurred by now" False

timeoutHook :: Spec -> Spec
timeoutHook = around_ $ maybe (fail "exceeded timeout") pure <=< timeout 1_000_000

spec :: Spec
spec = timeoutHook $ describe "UserGroupSubsystem.Interpreter" do
  -- TODO: add these "describe" sections once #4545 is merged.
  -- describe "CreateGroup :: UserId -> NewUserGroup -> UserGroupSubsystem m UserGroup" $ do
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
      expectLeft UserGroupNotATeamAdmin
        . runDependencies (allUsers team) (galleyTeam team)
        . interpretUserGroupSubsystem
        $ do
          let newUserGroup =
                NewUserGroup
                  { name = newUserGroupName,
                    members = User.userId <$> V.fromList (allUsers team)
                  }
              [nonAdminUser] = someAdminsOrOwners 1 team
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

  -- TODO: add these "describe" sections once #4545 is merged.
  -- describe "GetGroup, GetGroups, GetGroupsForUser" $ do
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
          getGroupAdmin === Just group1
            .&&. getGroupOutsider === Nothing

  prop "team members can only get user groups from their own team" $
    \(WithMods team1 :: WithMods '[AtLeastOneNonAdmin] ArbitraryTeam)
     userGroupName1
     (WithMods team2 :: WithMods '[AtLeastOneNonAdmin] ArbitraryTeam)
     userGroupName2 ->
        expectRight
          . runDependencies (allUsers team1 <> allUsers team2) (galleyTeam team1 <> galleyTeam team2)
          . interpretUserGroupSubsystem
          $ do
            let newUserGroup1 =
                  NewUserGroup
                    { name = userGroupName1,
                      members = mempty
                    }
                newUserGroup2 =
                  NewUserGroup
                    { name = userGroupName2,
                      members = mempty
                    }

            group1 <- createGroup (ownerId team1) newUserGroup1
            group2 <- createGroup (ownerId team2) newUserGroup2

            getOwnGroup <- getGroup (ownerId team1) group1.id_
            getOtherGroup <- getGroup (ownerId team1) group2.id_

            pure $
              getOwnGroup === Just group1
                .&&. getOtherGroup === Nothing

  describe "UpdateGroup :: UserId -> UserGroupId -> UserGroupUpdate -> UserGroupSubsystem m (Maybe UserGroup)" $ do
    prop "updateGroup updates the name" $
      \(team :: ArbitraryTeam) (originalName :: UserGroupName) (userGroupUpdate :: UserGroupUpdate) ->
        expectRight
          . runDependencies (allUsers team) (galleyTeam team)
          . interpretUserGroupSubsystem
          $ do
            ug0 :: UserGroup <- createGroup (ownerId team) (NewUserGroup originalName mempty)
            ug1 :: Maybe UserGroup <- getGroup (ownerId team) ug0.id_
            updateGroup (ownerId team) ug0.id_ userGroupUpdate
            ug2 :: Maybe UserGroup <- getGroup (ownerId team) ug0.id_
            pure $
              (ug1 === Just ug0)
                .&&. (ug2 === Just (ug0 {name = userGroupUpdate.name} :: UserGroup))

    prop "update sends events to all admins and owners" $ \team name newName (tm :: TeamMember) role ->
      let extraTeamMember = tm & permissions .~ rolePermissions role
          resultOrError =
            runDependenciesWithReturnState (allUsers team) (galleyTeamWithExtra team [extraTeamMember])
              . interpretUserGroupSubsystem
              $ do
                let nug = NewUserGroup {name = name, members = mempty}
                ug <- createGroup (ownerId team) nug
                updateGroup (ownerId team) ug.id_ (UserGroupUpdate (UserGroupName newName))
                pure ug

          expectedRecipient = Recipient (tm ^. TM.userId) RecipientClientsAll

          assertPushEvents :: UserGroup -> [Push] -> Property
          assertPushEvents ug [push, _createEvent] = case A.fromJSON @Event (A.Object push.json) of
            A.Success (UserGroupEvent (UserGroupUpdated ugid)) ->
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

    prop "only team admins should be able to update a group" $
      \((WithMods team) :: WithMods '[AtLeastOneNonAdmin] ArbitraryTeam) newUserGroupName newUserGroupName2 ->
        expectLeft UserGroupNotATeamAdmin
          . runDependencies (allUsers team) (galleyTeam team)
          . interpretUserGroupSubsystem
          $ do
            let newUserGroup =
                  NewUserGroup
                    { name = newUserGroupName,
                      members = User.userId <$> V.fromList (allUsers team)
                    }
                [nonAdminUser] = someAdminsOrOwners 1 team
            grp <- createGroup (ownerId team) newUserGroup
            void $ updateGroup (User.userId nonAdminUser) grp.id_ (UserGroupUpdate newUserGroupName2)
            unexpected

  describe "DeleteGroup :: UserId -> UserGroupId -> UserGroupSubsystem m ()" $ do
    prop "deleteGroup deletes" $ \team name name2 team2 -> do
      expectRight
        . runDependencies (allUsers team <> allUsers team2) (galleyTeam team <> galleyTeam team2)
        . interpretUserGroupSubsystem
        $ do
          ug <- createGroup (ownerId team) (NewUserGroup name mempty)
          ug2 <- createGroup (ownerId team) (NewUserGroup name2 mempty)

          mUg <- getGroup (ownerId team) ug.id_
          isDeleted <- isNothing <$> (deleteGroup (ownerId team) ug.id_ >> getGroup (ownerId team) ug.id_)
          mUg2 <- getGroup (ownerId team) ug2.id_
          e1 <- catchExpectedError $ deleteGroup (ownerId team2) ug.id_
          e2 <- catchExpectedError $ deleteGroup (ownerId team) (Id UUID.nil)

          pure $
            mUg === Just ug
              .&&. isDeleted === True
              .&&. mUg2 === Just ug2
              .&&. e1 === Just UserGroupNotFound
              .&&. e2 === Just UserGroupNotFound

    prop "delete sends events to all admins and owners" $ \team name (tm :: TeamMember) role ->
      let extraTeamMember = tm & permissions .~ rolePermissions role
          resultOrError =
            runDependenciesWithReturnState (allUsers team) (galleyTeamWithExtra team [extraTeamMember])
              . interpretUserGroupSubsystem
              $ do
                let nug = NewUserGroup name mempty
                ug <- createGroup (ownerId team) nug
                deleteGroup (ownerId team) ug.id_
                pure ug

          expectedRecipient = Recipient (tm ^. TM.userId) RecipientClientsAll

          assertPushEvents :: UserGroup -> [Push] -> Property
          assertPushEvents ug [push, _createEvent] = case A.fromJSON @Event (A.Object push.json) of
            A.Success (UserGroupEvent (UserGroupDeleted ugid)) ->
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

    prop "only team admins can delete user groups" $
      \((WithMods team) :: WithMods '[AtLeastOneNonAdmin] ArbitraryTeam) groupName ->
        expectLeft UserGroupNotATeamAdmin
          . runDependencies (allUsers team) (galleyTeam team)
          . interpretUserGroupSubsystem
          $ do
            grp <- createGroup (ownerId team) (NewUserGroup groupName mempty)
            let [nonAdminUser] = someAdminsOrOwners 1 team
            void $ deleteGroup (User.userId nonAdminUser) grp.id_
            unexpected

  describe "AddUser, RemoveUser :: UserId -> UserGroupId -> UserId -> UserGroupSubsystem m ()" $ do
    prop "addUser, removeUser adds, removes a user" $
      \((WithMods team) :: WithMods '[AtLeastSixMembers] ArbitraryTeam) newGroupName ->
        expectRight
          . runDependencies (allUsers team) (galleyTeam team)
          . interpretUserGroupSubsystem
          $ do
            let [mbr1, mbr2] = someMembersWithRoles 2 team Nothing
            ug :: UserGroup <- createGroup (ownerId team) (NewUserGroup newGroupName mempty)

            addUser (ownerId team) ug.id_ (User.userId mbr1)
            ugWithFirst <- getGroup (ownerId team) ug.id_

            addUser (ownerId team) ug.id_ (User.userId mbr1)
            ugWithIdemP <- getGroup (ownerId team) ug.id_

            addUser (ownerId team) ug.id_ (User.userId mbr2)
            ugWithSecond <- getGroup (ownerId team) ug.id_

            removeUser (ownerId team) ug.id_ (User.userId mbr1)
            ugWithoutFirst <- getGroup (ownerId team) ug.id_
            removeUser (ownerId team) ug.id_ (User.userId mbr1) -- idemp
            pure $
              ((.members) <$> ugWithFirst) === Just (V.fromList [User.userId mbr1])
                .&&. ((.members) <$> ugWithIdemP) === Just (V.fromList [User.userId mbr1])
                .&&. ((sort . V.toList . (.members)) <$> ugWithSecond) === Just (sort [User.userId mbr1, User.userId mbr2])
                .&&. ((.members) <$> ugWithoutFirst) === Just (V.fromList [User.userId mbr2])

    prop "added/removed user must be team member." $
      \((WithMods team) :: WithMods '[AtLeastSixMembers] ArbitraryTeam)
       newGroupName
       (team2 :: ArbitraryTeam)
       (addOrRemove :: Bool) ->
          expectLeft UserGroupMemberIsNotInTheSameTeam
            . runDependencies (allUsers team) (galleyTeam team)
            . interpretUserGroupSubsystem
            $ do
              ug <- createGroup (ownerId team) (NewUserGroup newGroupName mempty)
              (if addOrRemove then addUser else removeUser) (ownerId team) ug.id_ (ownerId team2)
              unexpected

    prop "adding/removing user must be team admin." $
      \((WithMods team) :: WithMods '[AtLeastSixMembers] ArbitraryTeam)
       newGroupName
       (team2 :: ArbitraryTeam)
       (addOrRemove :: Bool) ->
          expectLeft UserGroupNotFound
            . runDependencies (allUsers team) (galleyTeam team)
            . interpretUserGroupSubsystem
            $ do
              ug <- createGroup (ownerId team) (NewUserGroup newGroupName mempty)
              (if addOrRemove then addUser else removeUser) (ownerId team2) ug.id_ (ownerId team)
              unexpected

data TeamGenMod = AtLeastOneMember | AtLeastSixMembers | AtLeastOneNonAdmin

class KnownTeamGenMod a where
  teamGenMod :: TeamGenMod

instance KnownTeamGenMod 'AtLeastOneMember where
  teamGenMod = AtLeastOneMember

instance KnownTeamGenMod 'AtLeastSixMembers where
  teamGenMod = AtLeastSixMembers

instance KnownTeamGenMod 'AtLeastOneNonAdmin where
  teamGenMod = AtLeastOneNonAdmin

applyConstraint :: forall mod. (KnownTeamGenMod mod) => Gen ArbitraryTeam -> Gen ArbitraryTeam
applyConstraint =
  case teamGenMod @mod of
    AtLeastOneMember -> flip suchThat \team ->
      not $ Imports.null team.members
    AtLeastSixMembers -> flip suchThat \team ->
      length team.members > 6
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
  shrink _ =
    -- you can write down a better implementation, but make sure the constraints are still
    -- satisfied by the shrunk output.
    []

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
        <&> (TM.userId .~ User.userId adminUser)
    otherUsers <- listOf' arbitrary
    otherUserWithMembers <- for otherUsers $ \u -> do
      mem <- arbitrary
      pure (u, mem & TM.userId .~ User.userId u)
    pure . ArbitraryTeam tid (adminUser, adminMember) $ map (first assignTeam) otherUserWithMembers

  shrink team =
    if null team.members
      then []
      else
        let lessMembers = take (length team.members `div` 2) team.members
         in [team {members = lessMembers}]

allUsers :: ArbitraryTeam -> [User]
allUsers t = fst <$> t.owner : t.members

ownerId :: ArbitraryTeam -> UserId
ownerId t = User.userId (fst t.owner)

-- | The Map is required by the mock GalleyAPIAccess
galleyTeam :: ArbitraryTeam -> Map TeamId [TeamMember]
galleyTeam t = galleyTeamWithExtra t []

galleyTeamWithExtra :: ArbitraryTeam -> [TeamMember] -> Map TeamId [TeamMember]
galleyTeamWithExtra t tm = Map.singleton t.tid $ tm <> map snd (t.owner : t.members)

someAdminsOrOwners :: Int -> ArbitraryTeam -> [User]
someAdminsOrOwners num team = someMembersWithRoles num team (Just [RoleMember, RoleExternalPartner])

someMembersWithRoles :: (HasCallStack) => Int -> ArbitraryTeam -> Maybe [Role] -> [User]
someMembersWithRoles num team mbRoles = result
  where
    result =
      if length found == num
        then found
        else error $ "not enough members in the team?  " <> show (num, mbRoles, memberPerms)

    memberPerms :: [Maybe Role]
    memberPerms = (permissionsRole . (^. TM.permissions) . snd) <$> team.members

    found = fst <$> take num (filter f team.members)
      where
        f (_, mem) = case mbRoles of
          Just roles -> permissionsRole (mem ^. permissions) `elem` (Just <$> roles)
          Nothing -> True
