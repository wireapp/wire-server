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

import API.Search.Util
import Bilge (Http, Manager)
import qualified Brig.Options as BrigOpts
import Brig.Types
import Data.Handle
import Data.Qualified
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Util.Options (Endpoint)

-- NOTE: These federation tests require deploying two sets of (some) services
-- This might be best left to a kubernetes setup.
--
-- While individual functions can and should be tested in a more unit-testy way,
-- these more end-to-end integration test serve as a way to test the overall
-- network flow
--
-- FUTUREWORK(federation): Add tests for these scenarios (but not here in end2end, but in unit/1-backend-integration tests):
-- - Remote discovery fails
-- - Remote discovery succeeds but server doesn't exist
-- - Remote federator fails to respond in many ways (protocol error, timeout, etc.)
-- - SRV record has two servers but higher priority one always fails
spec :: BrigOpts.Opts -> Manager -> Brig -> Endpoint -> Brig -> IO TestTree
spec _brigOpts mg brig _federator brigTwo =
  pure $
    testGroup
      "brig-federation-user"
      [ test mg "lookup user by qualified handle on remote backend" $ testHandleLookup brig brigTwo,
        test mg "search users on remote backend" $ testSearchUsers brig brigTwo
      ]

-- | Path covered by this test:
--
-- +------+         +---------+        +---------+          +------+
-- | brig |   grpc  |federator| grpc   |federator|   http   | brig |
-- |      +-------->+         +------->+         +--------->+      |
-- +------+         +-+-------+        +---------+          +------+
testHandleLookup :: Brig -> Brig -> Http ()
testHandleLookup brig brigTwo = do
  -- Create a user on the "other side" using an internal brig endpoint from a
  -- second brig instance in backendTwo (in another namespace in kubernetes)
  (handle, userBrigTwo) <- createUserWithHandle brigTwo
  -- Get result from brig two for comparison
  let domain = qDomain $ userQualifiedId userBrigTwo
  resultViaBrigTwo <- getUserInfoFromHandle brigTwo domain handle

  -- query the local-namespace brig for a user sitting on the other backend
  -- (which will exercise the network traffic via two federators to the remote brig)
  resultViaBrigOne <- getUserInfoFromHandle brig domain handle

  liftIO $ assertEqual "remote handle lookup via federator should work in the happy case" (profileQualifiedId resultViaBrigOne) (userQualifiedId userBrigTwo)
  liftIO $ assertEqual "querying brig1 or brig2 about the same user should give same result" resultViaBrigTwo resultViaBrigOne

testSearchUsers :: Brig -> Brig -> Http ()
testSearchUsers brig brigTwo = do
  -- Create a user on the "other side" using an internal brig endpoint from a
  -- second brig instance in backendTwo (in another namespace in kubernetes)
  (handle, userBrigTwo) <- createUserWithHandle brigTwo

  searcher <- userId <$> randomUser brig
  let expectedUserId = userQualifiedId userBrigTwo
      searchTerm = (fromHandle handle)
      domain = (qDomain expectedUserId)
  liftIO $ putStrLn "search for user on brigTwo (directly)..."
  assertCanFindWithDomain brigTwo searcher expectedUserId searchTerm domain

  -- exercises multi-backend network traffic
  liftIO $ putStrLn "search for user on brigOne via federators to remote brig..."
  assertCanFindWithDomain brig searcher expectedUserId searchTerm domain

createUserWithHandle :: Brig -> Http (Handle, User)
createUserWithHandle brig = do
  u <- randomUser brig
  h <- randomHandle
  void $ putHandle brig (userId u) h
  userWithHandle <- selfUser <$> getSelfProfile brig (userId u)
  -- Verify if creating user and setting handle succeeded
  let handle = fromJust (userHandle userWithHandle)
  liftIO $ assertEqual "creating user with handle should return handle" h (fromHandle handle)
  pure (handle, userWithHandle)
