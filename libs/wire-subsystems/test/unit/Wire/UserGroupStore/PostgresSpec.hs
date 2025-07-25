{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-ambiguous-fields -Wno-incomplete-uni-patterns -Wno-incomplete-patterns -Wno-partial-type-signatures #-}

module Wire.UserGroupStore.PostgresSpec (spec) where

import Data.Default
import Data.Id
import Data.Json.Util
import Data.String.Conversions
import Data.Text qualified as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
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

checkPaginationState :: (HasCallStack) => PaginationState -> (Text, [Text]) -> Spec
checkPaginationState pstate result =
  it (T.unpack (fst result)) do
    paginationStateToSqlQuery tid pstate `shouldBe` result
  where
    tid = Id (fromJust $ UUID.fromText "d52017d2-578b-11f0-9699-9344acad2031")

spec :: Spec
spec = do
  describe "paginationStateToSqlQuery" $ do
    checkPaginationState
      def
      ( "select id, name, managed_by, created_at \
        \from user_group \
        \where team_id='d52017d2-578b-11f0-9699-9344acad2031' \
        \order by created_at desc, id desc \
        \limit 15",
        []
      )

    checkPaginationState
      def
        { sortBy = SortByName,
          sortOrder = Asc,
          pageSize = pageSizeFromIntUnsafe 200,
          lastSeen =
            Just
              ( LastSeen
                  { name = (Just $ userGroupNameFromTextUnsafe "ug1"),
                    createdAt = Nothing,
                    tieBreaker = (Id . fromJust . UUID.fromText $ "38fb011e-673a-11f0-86f3-eb55b8ac7296")
                  }
              )
        }
      ( "select id, name, managed_by, created_at \
        \from user_group \
        \where team_id='d52017d2-578b-11f0-9699-9344acad2031' and (name, id) > ($1 :: text, '38fb011e-673a-11f0-86f3-eb55b8ac7296') \
        \order by name asc, id asc \
        \limit 200",
        ["ug1"]
      )

    checkPaginationState
      def
        { sortBy = SortByCreatedAt,
          sortOrder = Desc,
          pageSize = pageSizeFromIntUnsafe 100,
          lastSeen =
            Just
              ( LastSeen
                  { name = Nothing,
                    createdAt = (Just . fromJust . readUTCTimeMillis $ "2021-05-12T10:52:02.000Z"),
                    tieBreaker = (Id . fromJust . UUID.fromText $ "ab1363ba-663e-11f0-99cd-77aae8e6aadd")
                  }
              )
        }
      ( "select id, name, managed_by, created_at \
        \from user_group \
        \where team_id='d52017d2-578b-11f0-9699-9344acad2031' \
        \and (created_at, id) < ('2021-05-12T10:52:02.000Z', 'ab1363ba-663e-11f0-99cd-77aae8e6aadd') \
        \order by created_at desc, id desc \
        \limit 100",
        []
      )

    checkPaginationState
      def
        { searchString = Just "grou"
        }
      ( "select id, name, managed_by, created_at \
        \from user_group \
        \where team_id='d52017d2-578b-11f0-9699-9344acad2031' \
        \and name ilike ($1 :: text) \
        \order by created_at desc, id desc \
        \limit 15",
        ["%grou%"]
      )

    checkPaginationState
      def
        { searchString = Just "grou",
          sortBy = SortByName,
          sortOrder = Desc,
          pageSize = pageSizeFromIntUnsafe 100,
          lastSeen =
            Just
              ( LastSeen
                  { name = Just $ userGroupNameFromTextUnsafe "group therapy",
                    createdAt = Nothing,
                    tieBreaker = (Id . fromJust . UUID.fromText $ "ab1363ba-663e-11f0-99cd-77aae8e6aadd")
                  }
              )
        }
      ( "select id, name, managed_by, created_at \
        \from user_group \
        \where team_id='d52017d2-578b-11f0-9699-9344acad2031' \
        \and (name, id) < ($1 :: text, 'ab1363ba-663e-11f0-99cd-77aae8e6aadd') \
        \and name ilike ($2 :: text) \
        \order by name desc, id desc \
        \limit 100",
        ["group therapy", "%grou%"]
      )

  describe "getUserGroups: in-mem (mock) interpreter" $ do
    let new :: (_) => TeamId -> Text -> Sem r UserGroup
        new tid name = do
          moveClock 1
          createUserGroup tid (NewUserGroup (userGroupNameFromTextUnsafe name) mempty) ManagedByWire

        pstate =
          def
            { sortBy = SortByName,
              sortOrder = Asc,
              pageSize = pageSizeFromIntUnsafe 500
            }

    it "searches for substrings" $ do
      let search :: (_) => TeamId -> Maybe Text -> Sem r [Text]
          search tid subst =
            (userGroupNameToText . (.name))
              <$$> getUserGroups tid pstate {searchString = subst}
      tid <- randomId
      inMemInt
        def
        ( do
            new tid `mapM_` ["01", "02", "10", "12"]
            search tid `mapM` (Nothing : (Just <$> ["1", "2", "10", "100"]))
        )
        `shouldReturn` [ ["01", "02", "10", "12"],
                         ["01", "10", "12"],
                         ["02", "12"],
                         ["10"],
                         []
                       ]

    it "answer contains rows in correct order" $ do
      let search :: (_) => TeamId -> (SortBy, SortOrder) -> Sem r [UserGroupMeta]
          search tid (sBy, sOrder) =
            getUserGroups tid (pstate {sortBy = sBy, sortOrder = sOrder})

          x0 = {- time 1 -} "anarchy"
          x1 = {- time 2 -} "bubble"
          x2 = {- time 3 -} "chocolate"
          x3 = {- time 3 -} "bubble"

      tid <- randomId
      (ugs, result) :: ([UserGroupMeta], [[UserGroupMeta]]) <- inMemInt def $ do
        setClock (posixSecondsToUTCTime 0)
        ugs <- do
          u012 <- new tid `mapM` [x0, x1, x2]
          moveClock (-1)
          u3 <- new tid x3
          pure (u012 <> [u3])

        res :: [[UserGroupMeta]] <-
          search tid
            `mapM` [ (SortByName, Asc),
                     (SortByName, Desc),
                     (SortByCreatedAt, Asc),
                     (SortByCreatedAt, Desc)
                   ]
        pure (userGroupToMeta <$> ugs, res)

      {-
      -- this gives you more debug output:
      let fingerprint :: UserGroup -> (Text, Text)
          fingerprint ug =
            ( userGroupNameToText ug.name,
              T.pack (formatTime defaultTimeLocale "%T" (fromUTCTimeMillis ug.createdAt))
            )
       in length (show (ugs, result)) `seq` traceShowM `mapM_` (fingerprint <$$> result)
      -}

      length result `shouldBe` 4
      head result `shouldBe` ((ugs !!) <$> [0, 1, 3, 2])
      result !! 1 `shouldBe` ((ugs !!) <$> [2, 3, 1, 0])
      result !! 2
        `shouldSatisfy` ( `elem`
                            [ ((ugs !!) <$> [0, 1, 3, 2]),
                              ((ugs !!) <$> [0, 1, 2, 3])
                            ]
                        )
      result !! 3
        `shouldSatisfy` ( `elem`
                            [ ((ugs !!) <$> [2, 3, 1, 0]),
                              ((ugs !!) <$> [3, 2, 1, 0])
                            ]
                        )

    it "paginates" $ do
      let search :: (_) => TeamId -> (Int, Maybe LastSeen) -> Sem r [Text]
          search tid (size, mblseen) =
            (userGroupNameToText . (.name))
              <$$> getUserGroups
                tid
                ( pstate
                    { pageSize = pageSizeFromIntUnsafe size,
                      lastSeen = mblseen
                    }
                )
      tid <- randomId
      inMemInt
        def
        ( do
            groups <- new tid `mapM` ["01", "02", "10", "12"]
            search tid
              `mapM` [ (0, Nothing), -- API does not allow page size=0
                       (1, Nothing),
                       (2, Nothing),
                       ( 2,
                         Just
                           ( LastSeen
                               { name = Just $ userGroupNameFromTextUnsafe "02",
                                 createdAt = Nothing,
                                 tieBreaker = (groups !! 1).id_
                               }
                           )
                       ),
                       ( 2,
                         Just
                           ( LastSeen
                               { name = Just $ userGroupNameFromTextUnsafe "12",
                                 createdAt = Nothing,
                                 tieBreaker = (groups !! 3).id_
                               }
                           )
                       )
                     ]
        )
        `shouldReturn` [ ["01", "02", "10", "12"],
                         ["01"],
                         ["01", "02"],
                         ["10", "12"],
                         []
                       ]

  -- This only works locally with /deploy/docker-ephemeral/run.sh running.  If it's in the
  -- way, it's fine to delete it.  The only benefit bla minibackend sci-fi bla.
  xdescribe "postgres vs. in-mem interpreters" $ do
    runAndCompare "CreateUserGroup" $ \tid -> do
      (userGroupNameToText . (.name)) <$> createUserGroup tid someNewUserGroup ManagedByWire

    runAndCompare "GetUserGroup" $ \tid -> do
      ugid <- (.id_) <$> createUserGroup tid someNewUserGroup ManagedByScim
      (userGroupNameToText . (.name)) <$$> getUserGroup tid ugid

    runAndCompareProp "GetUserGroups" $ \tid (TestPaginationState pstate) -> do
      (\new -> createUserGroup tid new ManagedByWire) `mapM_` testPaginationNewUserGroups
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
  mem <- inMemInt def {clockStep = 1} (action tid)
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
    let mem = unsafePerformIO $ inMemInt def {clockStep = 1} (action tid args)
    pg `shouldBe` mem

inMemInt :: UserGroupInMemState -> Sem (UserGroupStoreInMemEffectStack `Append` '[Embed IO]) a -> IO a
inMemInt st = runM . runInMemoryUserGroupStore st

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

-- TODO: copied & cloned from Brig.App.  where should we move & consolidate both?
-- Postgres.Extended?
-- TODO: when running this test suite in a "make devtest-package" loop, postgres clients will
-- not be cleaned up, and eventually there will be too many.
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
    arbitrary @PaginationState >>= \ps -> do
      searchString <- elements $ Nothing : (Just <$> ["1", "15", "group"])
      pure $ TestPaginationState ps {searchString}

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
