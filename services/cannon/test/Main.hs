-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

import Data.Metrics.Servant (routesToPaths)
import Data.Metrics.Test (pathsConsistencyCheck)
import Data.Metrics.Types (treeLookup)
import Imports
import Test.Cannon.Dict qualified as D
import Test.Cannon.Options qualified as Options
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Routes.Internal.Cannon qualified as Internal
import Wire.API.Routes.Public.Cannon (CannonAPI)
import Wire.API.Routes.Version (SpecialiseToVersion, Version (V8))

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ testCase "sitemap" $
          assertEqual
            "inconcistent sitemap"
            mempty
            (pathsConsistencyCheck $ routesToPaths @Internal.API),
        testCase "fan-in and classic notifications routes" $ do
          let paths = routesToPaths @(SpecialiseToVersion 'V8 CannonAPI)
          treeLookup paths ["events"] @?= Just "/events"
          treeLookup paths ["await"] @?= Just "/await",
        Options.tests,
        D.tests
      ]
