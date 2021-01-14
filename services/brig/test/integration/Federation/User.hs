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

module Federation.User where

import Bilge (Manager)
import qualified Brig.Options as BrigOpts
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Util (Brig)
import Util.Options (Endpoint)

spec :: BrigOpts.Opts -> Manager -> Brig -> Endpoint -> IO TestTree
spec brigOpts mg brig federator =
  pure $
    testGroup
      "brig-federation-user"
      [ testCase "lookup user by qualified handle on remote backend" $ testHandleLookup brigOpts mg brig federator
      ]

testHandleLookup :: BrigOpts.Opts -> Manager -> Brig -> Endpoint -> Assertion
testHandleLookup _brigOpts _mg _brig _federator = do
  undefined

-- TODO: we can make use of the 'NAMESPACE' environment variable to get our own namespace
-- next, we can either do a SRV lookup, or create a host+port combination based on a convention. Let's say if this particular kubernetes test namespace in which this executable is deployed is test-random123, then we expect a second installation for federation to be at test-random123-fed2, thus we can talk to the "second" brig on "http://brig." <> NAMESPACE <> "-fed2.svc.cluster.local:8080"
-- This allows us to do meaningful end2end tests:
--
-- 1. create a user on the "other side" using an internal brig endpoint in the other namespace
-- 2. query the local-namespace brig for a user sitting on the other backend
-- which should involve the following network traffic:
--
-- brig-integration -> brig -> federator -> fed2-federator -> fed2-brig
-- (and back)
--
-- While individual functions can and should be tested in a more unit-testy way, and end-to-end integration test will also be important.
--
-- TODO requires adding an option for "federation-integration-setup" (i.e. deploying two sets of all services - with their own config value overrides)
