{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns -Wno-incomplete-patterns -Wno-partial-type-signatures #-}

module Wire.UserGroupStore.PostgresSpec (spec) where

import Data.Default
import Data.Id
import Data.String.Conversions
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Hasql.Connection.Setting qualified as HasqlSetting
import Hasql.Connection.Setting.Connection qualified as HasqlConn
import Hasql.Connection.Setting.Connection.Param qualified as HasqlConfig
import Hasql.Pool qualified as HasqlPool
import Hasql.Pool.Config qualified as HasqlPool
import Imports
import Polysemy
import Polysemy.Error (Error, runError)
import Polysemy.Input
import Polysemy.Internal (Append)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.User.Profile
import Wire.API.UserGroup
import Wire.API.UserGroup.Pagination
import Wire.MockInterpreters.UserGroupStore
import Wire.UserGroupStore
import Wire.UserGroupStore.Postgres

check :: (HasCallStack) => PaginationState -> (Text, Maybe Text) -> Spec
check pstate result =
  it
    (T.unpack (fst result))
    (paginationStateToSqlQuery tid pstate `shouldBe` result)
  where
    tid = Id (fromJust $ UUID.fromText "d52017d2-578b-11f0-9699-9344acad2031")

spec :: Spec
spec = do
  describe "paginationStateToSqlQuery" $ do
    check
      def
      ( "select id, name, managed_by, created_at \
        \from user_group \
        \order by created_at desc, name asc \
        \offset 0 \
        \limit 15 \
        \where team_id='d52017d2-578b-11f0-9699-9344acad2031'",
        Nothing
      )

    check
      def
        { sortBy = SortByName,
          sortOrderName = Asc,
          sortOrderCreatedAt = Desc,
          pageSize = pageSizeFromIntUnsafe 200,
          offset = Just 4
        }
      ( "select id, name, managed_by, created_at \
        \from user_group \
        \order by name asc, created_at desc \
        \offset 4 \
        \limit 200 \
        \where team_id='d52017d2-578b-11f0-9699-9344acad2031'",
        Nothing
      )

    check
      def
        { sortBy = SortByCreatedAt,
          sortOrderName = Desc,
          sortOrderCreatedAt = Asc,
          pageSize = pageSizeFromIntUnsafe 100,
          offset = Just 104
        }
      ( "select id, name, managed_by, created_at \
        \from user_group \
        \order by created_at asc, name desc \
        \offset 104 \
        \limit 100 \
        \where team_id='d52017d2-578b-11f0-9699-9344acad2031'",
        Nothing
      )

    check
      def
        { searchString = Just "grou"
        }
      ( "select id, name, managed_by, created_at \
        \from user_group \
        \order by created_at desc, name asc \
        \offset 0 \
        \limit 15 \
        \where team_id='d52017d2-578b-11f0-9699-9344acad2031' \
        \and name ilike ($1 :: text)",
        Just "%grou%"
      )

  describe "in-mem (mock) interpreter" $ do
    let new :: (_) => TeamId -> Text -> Sem r UserGroup
        new tid name = createUserGroup tid (NewUserGroup (userGroupNameFromTextUnsafe name) mempty) ManagedByWire

        pstate =
          PaginationState
            { searchString = Nothing,
              sortBy = SortByName,
              sortOrderName = Asc,
              sortOrderCreatedAt = Asc,
              pageSize = pageSizeFromIntUnsafe 500,
              offset = Just 0
            }

    it "can search for substrings" $ do
      let search :: (_) => TeamId -> Maybe Text -> Sem r [Text]
          search tid subst =
            (userGroupNameToText . (.name))
              <$$> getUserGroups tid pstate {searchString = subst}
      tid <- randomId
      ( inMemInt $ do
          new tid `mapM_` ["01", "02", "10", "12"]
          search tid `mapM` (Nothing : (Just <$> ["1", "2", "10", "100"]))
        )
        `shouldReturn` [ ["01", "02", "10", "12"],
                         ["01", "10", "12"],
                         ["02", "12"],
                         ["10"],
                         []
                       ]

    it "can paginate" $ do
      let search :: (_) => TeamId -> (Int, Maybe Int) -> Sem r [Text]
          search tid (size, off) =
            (userGroupNameToText . (.name))
              <$$> getUserGroups tid (pstate {pageSize = pageSizeFromIntUnsafe size, offset = fromIntegral <$> off})

      tid <- randomId
      ( inMemInt $ do
          new tid `mapM_` ["01", "02", "10", "12"]
          search tid
            `mapM` [ (0, Nothing),
                     (1, Nothing),
                     (0, Just 0),
                     (1, Just 0),
                     (2, Just 0),
                     (2, Just 1),
                     (2, Just 3)
                   ]
        )
        `shouldReturn` [ [],
                         [],
                         ["01", "02", "10", "12"],
                         ["01"],
                         ["01", "02"],
                         ["02", "10"],
                         ["12"]
                       ]

  describe "postgres vs. in-mem interpreters" $ do
    runAndCompare "CreateUserGroup" $ \tid -> do
      (userGroupNameToText . (.name)) <$> createUserGroup tid someNewUserGroup ManagedByWire

    runAndCompare "GetUserGroup" $ \tid -> do
      ugid <- (.id_) <$> createUserGroup tid someNewUserGroup ManagedByScim
      (userGroupNameToText . (.name)) <$$> getUserGroup tid ugid

    focus $ runAndCompareProp "GetUserGroups" $ \tid (TestPaginationState pstate) -> do
      _ugid <- (.id_) <$$> ((\new -> createUserGroup tid new ManagedByWire) `mapM` testPaginationNewUserGroups)
      (userGroupNameToText . (.name)) <$$> getUserGroups tid pstate

    runAndCompare "UpdateUserGroups" $ \tid -> do
      ugid <- (.id_) <$> createUserGroup tid someNewUserGroup ManagedByWire
      (,,)
        <$> updateUserGroup tid ugid (UserGroupUpdate userGroupName2)
        <*> updateUserGroup tid (Id UUID.nil) (UserGroupUpdate userGroupName2)
        <*> ((userGroupNameToText . (.name)) <$$> getUserGroup tid ugid)

    runAndCompare "DeleteUserGroup" $ \tid -> do
      ugid <- (.id_) <$> createUserGroup tid someNewUserGroup ManagedByWire
      d <- deleteUserGroup tid ugid
      g <- getUserGroup tid ugid
      pure (d, g)

    runAndCompare "AddUser, RemoveUser" $ \tid -> do
      ugid <- (.id_) <$> createUserGroup tid someNewUserGroup ManagedByWire
      let [uid1, uid2, uid3] = unsafePerformIO $ replicateM 3 randomId

      addUser ugid uid1
      addUser ugid uid2
      removeUser ugid uid2
      removeUser ugid uid2
      removeUser ugid uid3

-- * runAndCompare

runAndCompare ::
  forall a.
  (HasCallStack, Eq a, Show a) =>
  String ->
  (forall r. (Member UserGroupStore r, Member (Embed IO) r) => TeamId -> Sem r a) ->
  Spec
runAndCompare msg action = it msg do
  tid <- randomId
  pg <- either (error . show) pure =<< postgresInt (action tid)
  mem <- inMemInt (action tid)
  pg `shouldBe` mem

runAndCompareProp ::
  forall args a.
  (HasCallStack, Arbitrary args, Show args, Eq a, Show a) =>
  String ->
  (forall r. (Member UserGroupStore r, Member (Embed IO) r) => TeamId -> args -> Sem r a) ->
  Spec
runAndCompareProp msg action = do
  prop msg $ \tid args -> do
    let pg = unsafePerformIO $ either (error . show) pure =<< postgresInt (action tid args)
    let mem = unsafePerformIO $ inMemInt (action tid args)
    pg `shouldBe` mem

inMemInt :: Sem (UserGroupStoreInMemEffectStack `Append` '[Embed IO]) a -> IO a
inMemInt = runM . runInMemoryUserGroupStore def

type UserGroupStorePostgresEffectStack =
  '[ UserGroupStore,
     Input HasqlPool.Pool,
     Error HasqlPool.UsageError,
     Embed IO
   ]

postgresInt :: Sem UserGroupStorePostgresEffectStack a -> IO (Either HasqlPool.UsageError a)
postgresInt action = do
  pool <- initPostgresPool
  runM
    . runError
    . runInputConst pool
    . interpretUserGroupStoreToPostgres
    $ action

-- TODO: copied & cloned from Brig.App.  where should we move & consolidate both?  Postgres.Extended?
initPostgresPool :: IO HasqlPool.Pool
initPostgresPool = do
  let pgParams =
        [ (HasqlConfig.host "127.0.0.1"),
          (HasqlConfig.port 5432),
          (HasqlConfig.user "wire-server"),
          (HasqlConfig.dbname "backendA"),
          (HasqlConfig.password "posty-the-gres")
        ]
  HasqlPool.acquire $
    HasqlPool.settings
      [ HasqlPool.staticConnectionSettings $
          [HasqlSetting.connection $ HasqlConn.params pgParams]
      ]

-- * data for runAndCompare

newtype TestPaginationState = TestPaginationState PaginationState
  deriving (Eq, Show)

instance Arbitrary TestPaginationState where
  arbitrary = do
    searchString <- elements $ Nothing : (Just <$> ["1", "15", "group"])
    sortByKey <- arbitrary
    sortOrderName <- arbitrary
    sortOrderCreatedAt <- arbitrary
    pageSize <- arbitrary
    pure $ TestPaginationState (PaginationState {sortBy = sortByKey, offset = Just 0, ..})

testPaginationNewUserGroups :: [NewUserGroup]
testPaginationNewUserGroups =
  ( \nm ->
      NewUserGroup
        (forceRight $ userGroupNameFromText nm)
        mempty
  )
    . cs
    . show
    <$> [(1 :: Int) .. 9]

forceRight :: (ConvertibleStrings s String) => Either s a -> a
forceRight = either (error . cs) id

someNewUserGroup :: NewUserGroup
someNewUserGroup = NewUserGroup userGroupName1 mempty

userGroupName1 :: UserGroupName
userGroupName1 = forceRight $ userGroupNameFromText "the group of the users"

userGroupName2 :: UserGroupName
userGroupName2 = forceRight $ userGroupNameFromText "do not publish secrets here"
