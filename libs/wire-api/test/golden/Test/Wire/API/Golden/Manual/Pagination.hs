{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Wire.API.Golden.Manual.Pagination where

import Data.Default
import Data.Json.Util
import Imports
import Wire.API.Pagination

someUTCTime :: UTCTimeMillis
Just someUTCTime = readUTCTimeMillis "2025-04-16T16:22:21.703Z"

someOtherUTCTime :: UTCTimeMillis
Just someOtherUTCTime = readUTCTimeMillis "2021-12-12T00:00:00.000Z"

testObject_PaginationResult_1 :: PaginationResult Int Int
testObject_PaginationResult_1 =
  PaginationResult
    []
    PaginationState
      { searchString = "",
        sortByKeys = SortBy [],
        sortOrder = def,
        pageSize = def,
        lastRowSent = -1
      }

testObject_PaginationResult_2 :: PaginationResult Int Int
testObject_PaginationResult_2 =
  PaginationResult
    [3, 5]
    PaginationState
      { searchString = "q",
        sortByKeys = SortBy ["key1", "key2"],
        sortOrder = Asc,
        pageSize = pageSizeFromIntUnsafe 500,
        lastRowSent = 3
      }

testObject_PaginationResult_3 :: PaginationResult Int Int
testObject_PaginationResult_3 =
  PaginationResult
    [7 .. 12]
    PaginationState
      { searchString = "rst",
        sortByKeys = SortBy ["key1", "", "key2"],
        sortOrder = Asc,
        pageSize = pageSizeFromIntUnsafe 1,
        lastRowSent = 10
      }
