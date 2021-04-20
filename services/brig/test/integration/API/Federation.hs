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

module API.Federation where

import API.Search.Util (refreshIndex)
import Bilge
import Bilge.Assert
import Brig.Types
import Data.Aeson (encode)
import qualified Data.Aeson as Aeson
import Data.Handle (Handle (..))
import Imports
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (generate)
import Test.Tasty
import Test.Tasty.HUnit (assertEqual, assertFailure)
import Util
import Wire.API.Federation.API.Brig (SearchRequest (SearchRequest))

-- FUTUREWORK(federation): use servant-client in tests for the federation endpoints instead of the bilge requests.
tests :: Manager -> Brig -> IO TestTree
tests m brig = do
  return $
    testGroup "federation" $
      [ test m "GET /federation/search/users : Found" (testSearchSuccess brig),
        test m "GET /federation/search/users : NotFound" (testSearchNotFound brig),
        test m "GET /federation/search/users : Empty Input - NotFound" (testSearchNotFoundEmpty brig),
        test m "GET /federation/users/by-handle : Found" (testGetUserByHandleSuccess brig),
        test m "GET /federation/users/by-handle : NotFound" (testGetUserByHandleNotFound brig)
      ]

testSearchSuccess :: Brig -> Http ()
testSearchSuccess brig = do
  (handle, user) <- createUserWithHandle brig
  let quid = userQualifiedId user

  -- create another user with a similar handle and the same display name
  -- That user should not be returned in search results.
  -- (as federated search should only search for exact handle matches)
  identityThief <- randomUser brig
  void $ putHandle brig (userId identityThief) (fromHandle handle <> "a")
  update'' :: UserUpdate <- liftIO $ generate arbitrary
  let update' = update'' {uupName = Just (Name (fromHandle handle))}
      update = RequestBodyLBS . encode $ update'
  put (brig . path "/self" . contentJson . zUser (userId identityThief) . zConn "c" . body update) !!! const 200 === statusCode
  refreshIndex brig

  searchResult <- fedSearch brig (fromHandle handle)
  liftIO $ do
    let contacts = contactQualifiedId <$> searchResults searchResult
    assertEqual "should return only the first user id but not the identityThief" [quid] contacts

testSearchNotFound :: Brig -> Http ()
testSearchNotFound brig = do
  searchResult <- fedSearch brig "this-handle-should-not-exist"
  liftIO $ do
    let contacts = searchResults searchResult
    assertEqual "should return empty array of users" [] contacts

testSearchNotFoundEmpty :: Brig -> Http ()
testSearchNotFoundEmpty brig = do
  searchResult <- fedSearch brig ""
  liftIO $ do
    let contacts = searchResults searchResult
    assertEqual "should return empty array of users" [] contacts

testGetUserByHandleSuccess :: Brig -> Http ()
testGetUserByHandleSuccess brig = do
  (handle, user) <- createUserWithHandle brig
  let quid = userQualifiedId user
  maybeProfile <- fedGetUserByHandle brig handle
  liftIO $ do
    case maybeProfile of
      Nothing -> assertFailure "Expected to find profile, found Nothing"
      Just profile -> do
        assertEqual "should return correct user Id" quid (profileQualifiedId profile)
        assertEqual "should not have email address" Nothing (profileEmail profile)

testGetUserByHandleNotFound :: Brig -> Http ()
testGetUserByHandleNotFound brig = do
  hdl <- randomHandle
  maybeProfile <- fedGetUserByHandle brig (Handle hdl)
  liftIO $ assertEqual "should not return any UserProfile" Nothing maybeProfile

-------------------------------------------------
-- helpers

-- TODO replace by servant client code
--
fedSearch :: Brig -> Text -> Http (SearchResult Contact)
fedSearch brig term =
  responseJsonError
    =<< post
      ( brig
          . paths ["federation", "search", "users"]
          . body (RequestBodyLBS (Aeson.encode $ SearchRequest term))
          . contentJson
          . expect2xx
      )

fedGetUserByHandle :: Brig -> Handle -> Http (Maybe UserProfile)
fedGetUserByHandle brig handle =
  responseJsonError
    =<< post
      ( brig
          . paths ["federation", "users", "by-handle"]
          . body (RequestBodyLBS (Aeson.encode handle))
          . contentJson
          . expect2xx
      )
