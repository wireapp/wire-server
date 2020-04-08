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

module Main
  ( main,
  )
where

import Data.Metrics (Metrics, metrics)
import Data.Metrics.Test (sitemapConsistency)
import Imports
import Network.Wai.Utilities.Server (compile)
import Proxy.API (sitemap)
import Proxy.Env (Env, createEnv)
import Proxy.Options (mockOpts)
import Test.Tasty

main :: IO ()
main = do
  mockEnv <- mkMockEnv =<< metrics
  defaultMain $
    testGroup
      "Main"
      [ sitemapConsistency . compile $ sitemap mockEnv
      ]

mkMockEnv :: Metrics -> IO Env
mkMockEnv mtrx = createEnv mtrx (mockOpts "doc/example.config")
