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
spec :: BrigOpts.Opts -> Manager -> Brig -> Endpoint -> Brig -> IO TestTree
spec _brigOpts mg brig _federator brigTwo =
  pure $
    testGroup
      "brig-federation-user"
      [ test mg "lookup user by qualified handle on remote backend" $ testHandleLookup brig brigTwo
      ]

testHandleLookup :: Brig -> Brig -> Http ()
testHandleLookup brig brigTwo = do
  -- Create a user on the "other
  -- side" using an internal brig endpoint from a second brig instance in
  -- backendTwo (in another namespace in kubernetes)
  u <- randomUser brigTwo
  h <- randomHandle
  void $ putHandle brigTwo (userId u) h
  self <- selfUser <$> getSelfProfile brigTwo (userId u)
  let handle = fromJust (userHandle self)
  liftIO $ assertEqual "creating user with handle should return handle" h (fromHandle handle)
  let domain = qDomain $ userQualifiedId self
  -- query the local-namespace brig for a user sitting on the other backend
  -- which should involve the following network traffic:
  --
  -- brig-integration -> brig -> federator -> fed2-federator -> fed2-brig
  -- (and back)
  result <- userHandleId <$> getUserInfoFromHandle brig domain handle
  liftIO $ assertEqual "remote handle lookup via federator should work in the happy case" result (Qualified (userId u) domain)
