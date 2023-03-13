-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

-- | related: https://github.com/nomeata/tasty-expected-failure
module Test.Tasty.Pending (flakyTestCase) where

import Imports
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Tasty.HUnit

flakyTestCase :: TestName -> Assertion -> TestTree
flakyTestCase name test = testCase name test'
  where
    test' = when (runthem == Just "1") test
    runthem = unsafePerformIO $ lookupEnv "RUN_FLAKY_TESTS"
