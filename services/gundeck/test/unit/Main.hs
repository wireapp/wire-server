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

module Main (main) where

import Data.Metrics.Test (pathsConsistencyCheck)
import Data.Metrics.WaiRoute (treeToPaths)
import qualified DelayQueue
import qualified Gundeck.API
import Imports
import qualified Json
import qualified Native
import Network.Wai.Utilities.Server (compile)
import OpenSSL (withOpenSSL)
import qualified Push
import Test.Tasty
import Test.Tasty.HUnit
import qualified ThreadBudget

main :: IO ()
main =
  withOpenSSL . defaultMain $
    testGroup
      "Main"
      [ testCase "sitemap" $
          assertEqual
            "inconcistent sitemap"
            mempty
            (pathsConsistencyCheck . treeToPaths . compile $ Gundeck.API.sitemap),
        DelayQueue.tests,
        Json.tests,
        Native.tests,
        Push.tests,
        ThreadBudget.tests
      ]
