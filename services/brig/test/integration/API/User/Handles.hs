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
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module API.User.Handles
  ( tests,
  )
where

import API.Search.Util qualified as Search
import API.Team.Util
import API.User.Util
import Bilge hiding (accept, timeout)
import Bilge.Assert
import Brig.Options qualified as Opt
import Control.Lens hiding (from, (#))
import Control.Monad.Catch (MonadCatch)
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.Handle (parseHandle)
import Data.Id
import Data.List1 qualified as List1
import Data.Qualified (Qualified (..))
import Data.UUID qualified as UUID
import Imports
import Network.Wai.Utilities.Error qualified as Error
import Network.Wai.Utilities.Error qualified as Wai
import Test.Tasty hiding (Timeout)
import Test.Tasty.Cannon hiding (Cannon, Timeout)
import Test.Tasty.Cannon qualified as WS
import Test.Tasty.HUnit
import UnliftIO (mapConcurrently)
import Util
import Util.Timeout
import Wire.API.Internal.Notification hiding (target)
import Wire.API.Team.Feature (FeatureStatus (..))
import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User.Handle

tests :: ConnectionLimit -> Timeout -> Opt.Opts -> Manager -> Brig -> Cannon -> Galley -> TestTree
tests _cl _at conf p b c g =
  testGroup
    "handles"
    [ test p "testHandleUpdate - handles/update" $ testHandleUpdate b c,
      test p "handles/race" $ testHandleRace b,
      test p "handles/query" $ testHandleQuery conf b,
      test p "handles/query - team-search-visibility SearchVisibilityStandard" $ testHandleQuerySearchVisibilityStandard conf b,
      test p "handles/query - team-search-visibility SearchVisibilityNoNameOutsideTeam" $ testHandleQuerySearchVisibilityNoNameOutsideTeam conf b g,
      test p "GET /users/handles/<handle> 200" $ testGetUserByUnqualifiedHandle b,
      test p "GET /users/handles/<handle> 404" $ testGetUserByUnqualifiedHandleFailure b,
      test p "GET /users/by-handle/<domain>/<handle> : 200" $ testGetUserByQualifiedHandle b,
      test p "GET /users/by-handle/<domain>/<handle> : 404" $ testGetUserByQualifiedHandleFailure b,
      test p "GET /users/by-handle/<domain>/<handle> : no federation" $ testGetUserByQualifiedHandleNoFederation conf b
    ]

-- The next line contains a mapping from the testHandleUpdate test to the following test standards:
-- @SF.Provisioning @TSFI.RESTfulAPI @S2
--
-- The test validates various updates to the user's handle. First, it attempts
-- to set invalid handles. This fails. Then it successfully sets a valid handle.
-- The user can retry setting the valid handle. The next scenario is for another
-- user to attempt to reuse an already used handle, which fails. Finally,
-- several scenarios of searching users by handle are explored, where users
-- appear by handle. A user can also free a handle and then reclaim it again.
testHandleUpdate :: Brig -> Cannon -> Http ()
testHandleUpdate brig cannon = do
  user <- randomUser brig
  let uid = userId user
      quid = userQualifiedId user
  -- Invalid handles are rejected
  let badHandles = ["ca$h", "w", "Capital", "wire"]
  forM_ badHandles $ \h -> do
    let upd = RequestBodyLBS . encode $ HandleUpdate h
    put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body upd) !!! do
      const 400 === statusCode
      const (Just "invalid-handle") === fmap Error.label . responseJsonMaybe
  -- Claim a valid handle & receive notification
  hdl <- randomHandle
  let update = RequestBodyLBS . encode $ HandleUpdate hdl
  WS.bracketR cannon uid $ \ws -> do
    put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body update)
      !!! const 200 === statusCode
    void . liftIO . WS.assertMatch (5 # Second) ws $ \n -> do
      let j = Object $ List1.head (ntfPayload n)
      j ^? key "type" . _String @?= Just "user.update"
      let u = j ^?! key "user"
      u ^? key "id" . _String @?= Just (UUID.toText (toUUID uid))
      u ^? key "handle" . _String @?= Just hdl
  -- The owner of the handle can always retry the update
  put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body update)
    !!! const 200 === statusCode
  Bilge.head (brig . paths ["users", "handles", toByteString' hdl] . zUser uid)
    !!! const 200 === statusCode
  -- For other users, the handle is unavailable
  uid2 <- userId <$> randomUser brig
  put (brig . path "/self/handle" . contentJson . zUser uid2 . zConn "c" . body update) !!! do
    const 409 === statusCode
    const (Just "handle-exists") === fmap Error.label . responseJsonMaybe
  -- The owner appears by that handle in search
  Search.refreshIndex brig
  Search.assertCanFind brig uid2 quid hdl
  -- Change the handle again, thus freeing the old handle
  hdl2 <- randomHandle
  let update2 = RequestBodyLBS . encode $ HandleUpdate hdl2
  put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body update2)
    !!! const 200 === statusCode
  Bilge.head (brig . paths ["users", "handles", toByteString' hdl] . zUser uid)
    !!! const 404 === statusCode
  -- The owner appears by the new handle in search
  Search.refreshIndex brig
  Search.assertCan'tFind brig uid2 quid hdl
  Search.assertCanFind brig uid2 quid hdl2
  -- Other users can immediately claim the old handle (the claim of the old handle is
  -- removed).
  put (brig . path "/self/handle" . contentJson . zUser uid2 . zConn "c" . body update) !!! do
    const 200 === statusCode
  -- The old handle can be claimed again immediately by the user who previously
  -- owned it (since the claim is either still active but his own, or expired).
  -- make sure 'hdl' is not used by 'uid2' already.
  hdl3 <- randomHandle
  let update3 = RequestBodyLBS . encode $ HandleUpdate hdl3
  put (brig . path "/self/handle" . contentJson . zUser uid2 . zConn "c" . body update3) !!! do
    const 200 === statusCode
  -- now 'uid2' takes 'hld' back.
  put (brig . path "/self/handle" . contentJson . zUser uid2 . zConn "c" . body update)
    !!! const 200 === statusCode

-- @END

testHandleRace :: Brig -> Http ()
testHandleRace brig = do
  us <- replicateM 10 (userId <$> randomUser brig)
  -- 10 races. In each race, 10 users try to claim the same handle.
  -- At most one of them should get the handle in each race
  -- (usually no-one due to the contention).
  void . replicateM 10 $ do
    hdl <- randomHandle
    let update = RequestBodyLBS . encode $ HandleUpdate hdl
    void . flip mapConcurrently us $ \u ->
      put (brig . path "/self/handle" . contentJson . zUser u . zConn "c" . body update)
    ps <- forM us $ \u -> responseJsonMaybe <$> get (brig . path "/self" . zUser u)
    let owners = catMaybes $ filter (maybe False ((== Just (fromJust (parseHandle hdl))) . userHandle)) ps
    liftIO $ assertBool "More than one owner of a handle" (length owners <= 1)

testHandleQuery :: Opt.Opts -> Brig -> Http ()
testHandleQuery opts brig = do
  uid <- userId <$> randomUser brig
  hdl <- randomHandle
  -- Query for the handle availability (must be free)
  Bilge.head (brig . paths ["users", "handles", toByteString' hdl] . zUser uid)
    !!! const 404 === statusCode
  -- Set handle
  let update = RequestBodyLBS . encode $ HandleUpdate hdl
  put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body update)
    !!! const 200 === statusCode
  -- Query the updated profile
  get (brig . path "/self" . zUser uid) !!! do
    const 200 === statusCode
    const (Just (fromJust $ parseHandle hdl)) === (userHandle <=< responseJsonMaybe)
  -- Query for the handle availability (must be taken)
  Bilge.head (brig . paths ["users", "handles", toByteString' hdl] . zUser uid)
    !!! const 200 === statusCode
  -- Query user profiles by handles
  get (apiVersion "v1" . brig . path "/users" . queryItem "handles" (toByteString' hdl) . zUser uid) !!! do
    const 200 === statusCode
    const (Just (fromJust $ parseHandle hdl)) === (profileHandle <=< listToMaybe <=< responseJsonMaybe)
  -- Bulk availability check
  hdl2 <- randomHandle
  hdl3 <- randomHandle
  checkHandles brig uid [hdl, hdl2, "InVa£iD", hdl3] 1 !!! do
    const 200 === statusCode
    const (Just [hdl2]) === responseJsonMaybe
  checkHandles brig uid [hdl2, hdl, hdl3] 3 !!! do
    const 200 === statusCode
    const (Just [hdl2, hdl3]) === responseJsonMaybe

  -- Let's check for availability outside the team when an option is given
  (_, user3, _) <- createPopulatedBindingTeamWithNamesAndHandles brig 0
  (_, user4, _) <- createPopulatedBindingTeamWithNamesAndHandles brig 0

  -- Usually, you can search outside your team
  assertCanFind brig user3 user4
  -- Usually, you can search outside your team but not if this config option is set
  let newOpts = opts & ((Opt.optionSettings . Opt.searchSameTeamOnlyLens) ?~ True)
  withSettingsOverrides newOpts $
    assertCannotFind brig user3 user4

testHandleQuerySearchVisibilityStandard :: Opt.Opts -> Brig -> Http ()
testHandleQuerySearchVisibilityStandard _opts brig = do
  (_, owner1, [member1]) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
  (_, owner2, [member2]) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
  extern <- randomUserWithHandle brig
  -- Assert that everyone can find each other:
  --   in the same or different team, by handle - direction does not matter
  assertCanFind brig owner1 owner2
  assertCanFind brig owner1 member1
  assertCanFind brig owner1 member2
  assertCanFind brig owner1 extern
  assertCanFind brig owner2 owner1
  assertCanFind brig member1 owner1
  assertCanFind brig member2 owner1
  assertCanFind brig extern owner1

testHandleQuerySearchVisibilityNoNameOutsideTeam :: Opt.Opts -> Brig -> Galley -> Http ()
testHandleQuerySearchVisibilityNoNameOutsideTeam _opts brig galley = do
  (tid1, owner1, [member1]) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
  (_, owner2, [member2]) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
  extern <- randomUserWithHandle brig
  setTeamTeamSearchVisibilityAvailable galley tid1 FeatureStatusEnabled
  setTeamSearchVisibility galley tid1 SearchVisibilityNoNameOutsideTeam
  -- this is the same as in 'testHandleQuerySearchVisibilityStandard' above, because we search
  -- for handles, not names.
  assertCanFind brig owner1 owner2
  assertCanFind brig owner1 member1
  assertCanFind brig owner1 member2
  assertCanFind brig owner1 extern
  assertCanFind brig owner2 owner1
  assertCanFind brig member1 owner1
  assertCanFind brig member2 owner1
  assertCanFind brig extern owner1

testGetUserByUnqualifiedHandle :: Brig -> Http ()
testGetUserByUnqualifiedHandle brig = do
  user <- randomUser brig
  handle <- randomHandle
  _ <- putHandle brig (userId user) handle
  requestingUser <- randomId
  get
    ( apiVersion "v1"
        . brig
        . paths ["users", "handles", toByteString' handle]
        . zUser requestingUser
    )
    !!! do
      const 200 === statusCode
      const (Right (UserHandleInfo (userQualifiedId user))) === responseJsonEither

testGetUserByUnqualifiedHandleFailure :: Brig -> Http ()
testGetUserByUnqualifiedHandleFailure brig = do
  handle <- randomHandle
  requestingUser <- randomId
  get
    ( apiVersion "v1"
        . brig
        . paths ["users", "handles", toByteString' handle]
        . zUser requestingUser
    )
    !!! do
      const 404 === statusCode
      const (Just "not-found") === fmap Error.label . responseJsonMaybe

testGetUserByQualifiedHandle :: Brig -> Http ()
testGetUserByQualifiedHandle brig = do
  user <- randomUser brig
  handle <- randomHandle
  let domain = qDomain (userQualifiedId user)
  _ <- putHandle brig (userId user) handle
  unconnectedUser <- randomUser brig
  profileForUnconnectedUser <-
    responseJsonError
      =<< get
        ( apiVersion "v1"
            . brig
            . paths ["users", "by-handle", toByteString' domain, toByteString' handle]
            . zUser (userId unconnectedUser)
            . expect2xx
        )
  liftIO $
    assertEqual
      "Id should match"
      (userQualifiedId user)
      (profileQualifiedId profileForUnconnectedUser)

  -- N.B. Internally this endpoint uses same implementation as getting a user
  -- profile by id. So, it is not necessary to test rest of the cases.
  liftIO $
    assertEqual
      "Email shouldn't be shown to unconnected user"
      Nothing
      (profileEmail profileForUnconnectedUser)

testGetUserByQualifiedHandleFailure :: Brig -> Http ()
testGetUserByQualifiedHandleFailure brig = do
  handle <- randomHandle
  qself <- userQualifiedId <$> randomUser brig
  get
    ( apiVersion "v1"
        . brig
        . paths
          [ "users",
            "by-handle",
            toByteString' (qDomain qself),
            toByteString' handle
          ]
        . zUser (qUnqualified qself)
    )
    !!! do
      const 404 === statusCode
      const (Just "not-found") === fmap Error.label . responseJsonMaybe

testGetUserByQualifiedHandleNoFederation :: Opt.Opts -> Brig -> Http ()
testGetUserByQualifiedHandleNoFederation opt brig = do
  let newOpts = opt {Opt.federatorInternal = Nothing, Opt.rabbitmq = Nothing}
  someUser <- randomUser brig
  withSettingsOverrides newOpts $
    get
      ( apiVersion "v1"
          . brig
          . paths ["users", "by-handle", "non-existant.example.com", "oh-a-handle"]
          . zUser (userId someUser)
      )
      !!! do
        const 400 === statusCode
        const "Bad Request" === statusMessage
        const (Right "federation-not-enabled") === fmap Wai.label . responseJsonEither

assertCanFind :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> User -> User -> m ()
assertCanFind brig from target = do
  liftIO $ assertBool "assertCanFind: Target must have a handle set" (isJust $ userHandle target)
  let targetHandle = fromMaybe (error "Impossible") (userHandle target)
  get (apiVersion "v1" . brig . path "/users" . queryItem "handles" (toByteString' targetHandle) . zUser (userId from)) !!! do
    const 200 === statusCode
    const (userHandle target) === (responseJsonMaybe >=> listToMaybe >=> profileHandle)

  get (apiVersion "v1" . brig . paths ["users", "handles", toByteString' targetHandle] . zUser (userId from)) !!! do
    const 200 === statusCode
    const (Just (UserHandleInfo $ userQualifiedId target)) === responseJsonMaybe

assertCannotFind :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> User -> User -> m ()
assertCannotFind brig from target = do
  liftIO $ assertBool "assertCannotFind: Target must have a handle set" (isJust $ userHandle target)
  let targetHandle = fromMaybe (error "Impossible") (userHandle target)
  get (apiVersion "v1" . brig . path "/users" . queryItem "handles" (toByteString' targetHandle) . zUser (userId from)) !!! do
    const 404 === statusCode
  get (apiVersion "v1" . brig . paths ["users", "handles", toByteString' targetHandle] . zUser (userId from)) !!! do
    const 404 === statusCode
