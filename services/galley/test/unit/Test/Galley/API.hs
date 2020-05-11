{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.Galley.API where

import Data.Predicate (exec)
import Data.Set hiding (drop)
import Galley.API ()
import Galley.API.Public (filterMissing)
import Galley.Types
import Imports
import qualified Network.HTTP.Types as H
import qualified Network.Wai as W
import qualified Network.Wai.Predicate as P
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ runFilterMissing
        [("ignore_missing", Just "true"), ("report_missing", Just "true")]
        (Right OtrIgnoreAllMissing),
      runFilterMissing
        [("ignore_missing", Just "false"), ("report_missing", Just "true")]
        (Right OtrReportAllMissing),
      runFilterMissing
        [("ignore_missing", Nothing), ("report_missing", Just "true")]
        (Right OtrReportAllMissing),
      runFilterMissing
        [("report_missing", Just "true")]
        (Right OtrReportAllMissing),
      runFilterMissing
        [("ignore_missing", Just "af5d99e4-8477-11ea-9a66-7b8304d416cd")]
        (Right (OtrIgnoreMissing (fromList [read "af5d99e4-8477-11ea-9a66-7b8304d416cd"]))),
      runFilterMissing
        [("ignore_missing", Just "af5d99e4-8477-11ea-9a66-7b8304d416cd"), ("report_missing", Just "21c56f5c-8478-11ea-938e-b7f0a819ff2b")]
        (Right (OtrIgnoreMissing (fromList [read "af5d99e4-8477-11ea-9a66-7b8304d416cd"]))),
      runFilterMissing
        [("report_missing", Just "af5d99e4-8477-11ea-9a66-7b8304d416cd,b11ec5b4-8477-11ea-9926-279424ad995e")]
        (Right (OtrReportMissing (fromList [read "af5d99e4-8477-11ea-9a66-7b8304d416cd", read "b11ec5b4-8477-11ea-9926-279424ad995e"]))),
      runFilterMissing
        [("report_missing", Just "21c56f5c-8478-11ea-938e-b7f0a819ff2b")]
        (Right (OtrReportMissing (fromList [read "21c56f5c-8478-11ea-938e-b7f0a819ff2b"]))),
      runFilterMissing
        [("ignore_missing", Just "false"), ("report_missing", Just "21c56f5c-8478-11ea-938e-b7f0a819ff2b")]
        (Right OtrReportAllMissing),
      runFilterMissing
        [("ignore_missing", Just "@@@")]
        (Left 400),
      runFilterMissing
        [("ignore_missing", Just "@@@"), ("report_missing", Just "21c56f5c-8478-11ea-938e-b7f0a819ff2b")]
        (Left 400),
      runFilterMissing
        [("report_missing", Just "@@@")]
        (Left 400),
      runFilterMissing
        [("ignore_missing", Just "true"), ("report_missing", Just "@@@")]
        (Right OtrIgnoreAllMissing)
    ]

runFilterMissing :: HasCallStack => H.Query -> Either Int OtrFilterMissing -> TestTree
runFilterMissing given want = testCase ("filterMissing: " <> show want) $ run given @=? want
  where
    run queryString =
      exec
        filterMissing
        (W.defaultRequest {W.queryString = queryString})
        (Left . H.statusCode . P.status)
        Right
