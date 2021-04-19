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

module Federation.End2end where

import API.Search.Util
import Bilge
import Bilge.Assert ((!!!), (===))
import qualified Brig.Options as BrigOpts
import Brig.Types
import qualified Data.Aeson as Aeson
import Data.ByteString.Conversion (toByteString')
import Data.Handle
import Data.Qualified
import Federation.Util (generateClientPrekeys)
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Util.Options (Endpoint)
import Wire.API.User (ListUsersQuery (ListUsersByIds))

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
      "federation-end2end-user"
      [ test mg "lookup user by qualified handle on remote backend" $ testHandleLookup brig brigTwo,
        test mg "search users on remote backend" $ testSearchUsers brig brigTwo,
        test mg "get users by ids on multiple backends" $ testGetUsersById brig brigTwo,
        test mg "get client prekey" $ testGetPrekey brig brigTwo
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

testGetUsersById :: Brig -> Brig -> Http ()
testGetUsersById brig1 brig2 = do
  users <- traverse randomUser [brig1, brig2]
  let self = Imports.head users
      q = ListUsersByIds (map userQualifiedId users)
      sortUsers :: [UserProfile] -> [UserProfile]
      sortUsers = sortBy (compare `on` profileQualifiedId)
      expected = sortUsers (map connectedProfile users)
  post
    ( brig1
        . path "list-users"
        . zUser (userId self)
        . body (RequestBodyLBS (Aeson.encode q))
        . contentJson
        . acceptJson
        . expect2xx
    )
    !!! do
      const 200 === statusCode
      const (Just expected) === fmap sortUsers . responseJsonMaybe

testGetPrekey :: Brig -> Brig -> Http ()
testGetPrekey brig1 brig2 = do
  self <- randomUser brig1
  user <- randomUser brig2
  let new = defNewClient TemporaryClientType (take 1 somePrekeys) (Imports.head someLastPrekeys)
  c <- responseJsonError =<< addClient brig2 (userId user) new
  let cpk = ClientPrekey (clientId c) (Imports.head somePrekeys)
  let quser = userQualifiedId user
  get
    ( brig1
        . zUser (userId self)
        . paths
          [ "users",
            toByteString' (qDomain quser),
            toByteString' (qUnqualified quser),
            "prekeys",
            toByteString' (clientId c)
          ]
    )
    !!! do
      const 200 === statusCode
      const (Just cpk) === responseJsonMaybe

testClaimPrekeyBundleSuccess :: Brig -> Brig -> Http ()
testClaimPrekeyBundleSuccess brig1 brig2 = do
  qself <- userQualifiedId <$> randomUser brig1
  let prekeys = take 5 (zip somePrekeys someLastPrekeys)
  (quser, clients) <- generateClientPrekeys brig2 prekeys
  let sortClients = sortBy (compare `on` prekeyClient)
  get
    ( brig1
        . zUser (qUnqualified qself)
        . paths
          [ "users",
            toByteString' (qDomain quser),
            toByteString' (qUnqualified quser),
            "prekeys"
          ]
        . expect2xx
    )
    !!! do
      const 200 === statusCode
      const (Just (sortClients clients))
        === fmap (sortClients . prekeyClients) . responseJsonMaybe
