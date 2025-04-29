{-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns #-}

module Wire.UserGroupSubsystem.InterpreterSpec (spec) where

import Imports
import Test.Hspec

spec :: Spec
spec = describe "UserGroupSubsystem.Interpreter" undefined

-- do
--   prop "getGroup gets you what createGroup creates" $ \gname usrs ->
--     let now = unsafePerformIO getCurrentTime
--      in Mock.runInMemoryUserGroupSubsystem now do
--           ug0 <- getGroup (Id UUID.nil)
--           let nug = NewUserGroup gname usrs
--           ug <- createGroup nug
--           ug' <- getGroup ug.id_
--           pure $
--             counterexample (show (ug0, nug, ug, ug')) $
--               (ug0 === Nothing)
--                 .&&. (ug.name === nug.name)
--                 .&&. (ug.members === nug.members)
--                 .&&. (ug.managedBy === ManagedByWire)
--                 .&&. (ug' === Just ug)

--   describe "getGroups" $ do
--     let now = unsafePerformIO getCurrentTime
--         nugs = [1 .. 15] <&> \(i :: Int) -> NewUserGroup (cs $ show i) mempty

--         check :: (HasCallStack) => UserGroupPage -> [UserGroupId] -> Bool -> Expectation
--         check have wantPage wantHasMore = do
--           (have.page <&> (.id_)) `shouldBe` wantPage
--           have.hasMore `shouldBe` wantHasMore

--         lastKeyOf :: UserGroupPage -> UUID
--         lastKeyOf = toUUID . (.id_) . last . (.page)

--     it "paginates" $ do
--       Mock.runInMemoryUserGroupSubsystem now do
--         gids <- (sort . ((.id_) <$>)) <$> createGroup `mapM` nugs
--         allOfThem <- getGroups Nothing Nothing
--         first3 <- getGroups (Just 3) Nothing
--         next4 <- getGroups (Just 4) (Just $ lastKeyOf first3)
--         halfPage1 <- getGroups (Just 100) Nothing
--         halfPage2 <- getGroups (Just 100) (Just $ lastKeyOf next4)

--         pure do
--           check allOfThem gids False
--           check first3 (take 3 gids) True
--           check next4 (take 4 (drop 3 gids)) True
--           check halfPage1 gids False
--           check halfPage2 (drop 7 gids) False

--     it "paginates well under updates" $ do
--       Mock.runInMemoryUserGroupSubsystem now do
--         gids <- (sort . ((.id_) <$>)) <$> createGroup `mapM` nugs
--         beforeNewCreate <- getGroups (Just 8) Nothing
--         newGroup <- createGroup (NewUserGroup "the new one" mempty)
--         afterNewCreate <- getGroups (Just 8) (Just $ lastKeyOf beforeNewCreate)

--         pure do
--           check beforeNewCreate (take 8 gids) True

--           let afterGids =
--                 if lastKeyOf beforeNewCreate < toUUID newGroup.id_
--                   then drop 8 (sort (newGroup.id_ : gids))
--                   else drop 8 gids
--           check afterNewCreate afterGids False

--   prop "updateGroup updates the name" $ \originalName userGroupUpdate ->
--     let now = unsafePerformIO getCurrentTime
--      in Mock.runInMemoryUserGroupSubsystem now do
--           ug0 <- createGroup (NewUserGroup originalName mempty)
--           ug1 <- getGroup ug0.id_
--           ug2 <- updateGroup ug0.id_ userGroupUpdate
--           ug3 <- getGroup ug0.id_
--           pure $
--             (ug1 === Just ug0)
--               .&&. (ug2 === Just (ug0 {name = userGroupUpdate.name} :: UserGroup))
--               .&&. (ug3 === ug2)

--   prop "deleteGroup deletes" $ \newGroup1 newGroup2 -> do
--     let now = unsafePerformIO getCurrentTime
--      in Mock.runInMemoryUserGroupSubsystem now do
--           ug1 <- createGroup newGroup1
--           ug2 <- createGroup newGroup2
--           deleteGroup ug1.id_
--           allGroups <- (.page) <$> getGroups Nothing Nothing

--           deleteGroup (Id UUID.nil) -- idempotency
--           allGroups' <- (.page) <$> getGroups Nothing Nothing

--           pure $ do
--             allGroups `shouldBe` [ug2]
--             allGroups' `shouldBe` [ug2]

--   prop "addUser adds a user" $ \newGroup newUserId -> do
--     -- TODO: how do we feel about dangling user ids?  maybe that should be handled on another
--     -- level, and UserGroupSubsystem should be oblivious to what user ids point to?
--     let now = unsafePerformIO getCurrentTime
--      in Mock.runInMemoryUserGroupSubsystem now do
--           ug :: UserGroup <- createGroup newGroup
--           addUser ug.id_ newUserId
--           ug' :: Maybe UserGroup <- getGroup ug.id_
--           addUser ug.id_ newUserId -- idempotency
--           ug'' :: Maybe UserGroup <- getGroup ug.id_
--           pure $ do
--             ((sort . V.toList . (.members)) <$> ug') `shouldBe` Just (sort (newUserId : V.toList ug.members))
--             ug'' `shouldBe` ug'

--   prop "removeUser removes a user" $ \newGroup -> do
--     let now = unsafePerformIO getCurrentTime
--      in Mock.runInMemoryUserGroupSubsystem now do
--           ug :: UserGroup <- createGroup newGroup
--           let removee :: UserId
--               removee = case V.toList ug.members of
--                 [] -> Id UUID.nil -- idempotency
--                 _ : _ -> ug.members V.! (length ug.members `div` 2)
--           removeUser ug.id_ removee
--           ug' :: Maybe UserGroup <- getGroup ug.id_
--           pure $ do
--             ((sort . V.toList . (.members)) <$> ug') `shouldBe` Just (sort (V.toList ug.members \\ [removee]))
