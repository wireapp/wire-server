-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

-- | Unit tests for 'Gundeck.Push.Web.Runner.runWebPush'.
--
-- These exercise the Polysemy stack /assembly/ (the order of interpreters and
-- the 'Wire.Postgres.PGConstraints' plumbing) without touching a real database:
-- @'runWebPush' pool ('pure' x)@ never invokes any effect handler, so the pool
-- is acquired but never connected to. 'Hasql.Pool.acquire' is itself lazy — it
-- only opens connections on demand — so a bogus libpq settings 'Map' is
-- sufficient.
module WebPushRunner where

import Data.Map qualified as Map
import Data.Misc (Duration (..))
import Data.Time.Clock (secondsToDiffTime)
import Gundeck.Push.Web.Runner
import Hasql.Pool qualified as Hasql
import Hasql.Pool.Extended (PoolConfig (..), initPostgresPool)
import Imports
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "WebPushRunner"
    [ testCase "runWebPush pool (pure ()) returns Right ()" $ do
        pool <- acquireDummyPool
        result <- runWebPush pool (pure ())
        Hasql.release pool
        result @?= Right ()
    ]

-- | A pool whose connection settings are immaterial: the runner's trivial
-- program never reaches the database, and 'Hasql.Pool.acquire' does not open
-- connections eagerly. We still pick a tiny @size@ and short timeouts so the
-- pool allocates no background resources of note.
acquireDummyPool :: IO Hasql.Pool
acquireDummyPool =
  initPostgresPool dummyPoolConfig dummyPgSettings Nothing
  where
    -- Minimal libpq params; never used to connect.
    dummyPgSettings :: Map Text Text
    dummyPgSettings = Map.fromList [("host", "localhost"), ("port", "5432")]
    dummyPoolConfig :: PoolConfig
    dummyPoolConfig =
      PoolConfig
        { size = 1,
          acquisitionTimeout = Duration (secondsToDiffTime 1),
          agingTimeout = Duration (secondsToDiffTime 1),
          idlenessTimeout = Duration (secondsToDiffTime 1)
        }
