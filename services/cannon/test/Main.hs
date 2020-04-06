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

module Main where

import qualified Bench as B
import qualified Cannon.API
import Data.Metrics.Test (pathsConsistencyCheck)
import Data.Metrics.WaiRoute (treeToPaths)
import Imports
import Network.Wai.Utilities.Server (compile)
import qualified Test.Cannon.Dict as D
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  B.benchmark
  defaultMain $
    testGroup
      "Tests"
      [ testCase "sitemap" $
          assertEqual
            "inconcistent sitemap"
            mempty
            (pathsConsistencyCheck . treeToPaths . compile $ Cannon.API.sitemap),
        D.tests
      ]
