{-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns #-}

module Wire.UserGroupSubsystem.InterpreterSpec (spec) where

import Data.Id
import Data.String.Conversions (cs)
import Data.Time
import Data.UUID as UUID
import Imports
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.User hiding (DeleteUser)
import Wire.API.UserGroup
import Wire.MockInterpreters.UserGroupSubsystem qualified as Mock
import Wire.UserGroupSubsystem

spec :: Spec
spec = describe "UserGroupSubsystem.Interpreter" do
  prop "getGroup gets you what createGroup creates" $ \gname usrs ->
    let now = unsafePerformIO getCurrentTime
     in Mock.runInMemoryUserGroupSubsystem now do
          ug0 <- getGroup (Id UUID.nil)
          let nug = NewUserGroup gname usrs
          ug <- createGroup nug
          ug' <- getGroup ug.id_
          pure $
            counterexample (show (ug0, nug, ug, ug')) $
              (ug0 === Nothing)
                .&&. (ug.name === nug.name)
                .&&. (ug.members === nug.members)
                .&&. (ug.managedBy === ManagedByWire)
                .&&. (ug.createdAt === now)
                .&&. (ug' === Just ug)

  describe "getGroups" $ do
    let now = unsafePerformIO getCurrentTime
        nugs = [1 .. 15] <&> \(i :: Int) -> NewUserGroup (cs $ show i) []

        check :: (HasCallStack) => UserGroupPage -> [UserGroupId] -> Bool -> Expectation
        check have wantPage wantHasMore = do
          (have.page <&> (.id_)) `shouldBe` wantPage
          have.hasMore `shouldBe` wantHasMore

        lastKeyOf :: UserGroupPage -> UUID
        lastKeyOf = toUUID . (.id_) . last . (.page)

    it "paginates" $ do
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

  prop "updateGroup updates the name" $ \originalName userGroupUpdate ->
    let now = unsafePerformIO getCurrentTime
     in Mock.runInMemoryUserGroupSubsystem now do
          ug0 <- createGroup (NewUserGroup originalName [])
          ug1 <- getGroup ug0.id_
          ug2 <- updateGroup ug0.id_ userGroupUpdate
          ug3 <- getGroup ug0.id_
          pure $
            (ug1 === Just ug0)
              .&&. (ug2 === Just (ug0 {name = userGroupUpdate.name} :: UserGroup))
              .&&. (ug3 === ug2)

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
