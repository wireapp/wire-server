{-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns -Wno-incomplete-patterns #-}

module Wire.UserGroupStore.PostgresSpec (spec) where

import Data.Default
import Data.Id
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Imports
import Test.Hspec
import Wire.API.UserGroup.Pagination
import Wire.UserGroupStore.Postgres

check :: (HasCallStack) => PaginationState -> (Text, Maybe Text) -> Spec
check pstate result =
  it
    (T.unpack (fst result))
    (paginationStateToSqlQuery tid pstate `shouldBe` result)
  where
    tid = Id (fromJust $ UUID.fromText "d52017d2-578b-11f0-9699-9344acad2031")

spec :: Spec
spec =
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
        \offset 4 limit 200 \
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
        \order by created_at asc, name desc offset 104 \
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
        \where team_id='d52017d2-578b-11f0-9699-9344acad2031' and name ilike ($1 :: text)",
        Just "%grou%"
      )
