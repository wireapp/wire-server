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

module API.MLS where

import API.MLS.Util
import Bilge
import Bilge.Assert ((<!!), (===))
import Brig.Options
import Data.Aeson qualified as Aeson
import Data.ByteString.Conversion
import Data.Default
import Data.Id
import Data.Qualified
import Data.Set qualified as Set
import Data.Timeout
import Debug.Trace (traceM)
import Federation.Util
import Imports
import System.IO.Temp
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation
import Wire.API.User
import Wire.API.User.Client

tests :: Manager -> Brig -> Opts -> TestTree
tests m b opts =
  testGroup
    "MLS"
    [ test m "POST /mls/key-packages/self/:client (no public keys)" (testKeyPackageUploadNoKey b),
      -- FUTUREWORK test m "GET /mls/key-packages/self/:client/count (expired package)" (testKeyPackageExpired b),
      test m "GET /mls/key-packages/claim/local/:user" (testKeyPackageClaim b),
      test m "GET /mls/key-packages/claim/local/:user - self claim" (testKeyPackageSelfClaim b),
      test m "GET /mls/key-packages/claim/remote/:user" (testKeyPackageRemoteClaim opts b)
    ]

testKeyPackageUploadNoKey :: Brig -> Http ()
testKeyPackageUploadNoKey brig = do
  u <- userQualifiedId <$> randomUser brig
  c <- createClient brig u 0
  withSystemTempDirectory "mls" $ \tmp ->
    uploadKeyPackages brig tmp def {kiSetKey = DontSetKey} u c 5

  count <- getKeyPackageCount brig u c
  liftIO $ count @?= 0

testKeyPackageExpired :: Brig -> Http ()
testKeyPackageExpired brig = do
  u <- userQualifiedId <$> randomUser brig
  let lifetime = 3 # Second
  [c1, c2] <- for [(0, Just lifetime), (1, Nothing)] $ \(i, lt) -> do
    c <- createClient brig u i
    -- upload 1 key package for each client
    withSystemTempDirectory "mls" $ \tmp ->
      uploadKeyPackages brig tmp def {kiLifetime = lt} u c 1
    pure c
  for_ [(c1, 1), (c2, 1)] $ \(cid, expectedCount) -> do
    count <- getKeyPackageCount brig u cid
    liftIO $ count @?= expectedCount
  -- wait for c1's key package to expire
  threadDelay (fromIntegral ((lifetime + 4 # Second) #> MicroSecond))

  -- c1's key package has expired by now
  for_ [(c1, 0), (c2, 1)] $ \(cid, expectedCount) -> do
    count <- getKeyPackageCount brig u cid
    liftIO $ count @?= expectedCount

testKeyPackageClaim :: Brig -> Http ()
testKeyPackageClaim brig = do
  -- setup a user u with two clients c1 and c2
  u <- userQualifiedId <$> randomUser brig
  [c1, c2] <- for [0, 1] $ \i -> do
    c <- createClient brig u i
    -- upload 3 key packages for each client
    withSystemTempDirectory "mls" $ \tmp ->
      uploadKeyPackages brig tmp def u c 3
    pure c

  -- claim packages for both clients of u
  u' <- userQualifiedId <$> randomUser brig
  bundle :: KeyPackageBundle <-
    responseJsonError
      =<< post
        ( brig
            . paths ["mls", "key-packages", "claim", toByteString' (qDomain u), toByteString' (qUnqualified u)]
            . zUser (qUnqualified u')
        )
        <!! const 200 === statusCode

  liftIO $ Set.map (\e -> (e.user, e.client)) bundle.entries @?= Set.fromList [(u, c1), (u, c2)]

  -- check that we have one fewer key package now
  for_ [c1, c2] $ \c -> do
    count <- getKeyPackageCount brig u c
    liftIO $ count @?= 2

testKeyPackageSelfClaim :: Brig -> Http ()
testKeyPackageSelfClaim brig = do
  -- setup a user u with two clients c1 and c2
  u <- userQualifiedId <$> randomUser brig
  [c1, c2] <- for [0, 1] $ \i -> do
    c <- createClient brig u i
    -- upload 3 key packages for each client
    withSystemTempDirectory "mls" $ \tmp ->
      uploadKeyPackages brig tmp def u c 3
    pure c

  -- claim own packages but skip the first
  do
    bundle :: KeyPackageBundle <-
      responseJsonError
        =<< post
          ( brig
              . paths ["mls", "key-packages", "claim", toByteString' (qDomain u), toByteString' (qUnqualified u)]
              . zUser (qUnqualified u)
              . zClient c1
          )
          <!! const 200 === statusCode
    liftIO $ Set.map (\e -> (e.user, e.client)) bundle.entries @?= Set.fromList [(u, c2)]

    -- check that we still have all keypackages for client c1
    count <- getKeyPackageCount brig u c1
    liftIO $ count @?= 3

  -- if another user sets skip_own, nothing is skipped
  do
    u' <- userQualifiedId <$> randomUser brig
    bundle :: KeyPackageBundle <-
      responseJsonError
        =<< post
          ( brig
              . paths ["mls", "key-packages", "claim", toByteString' (qDomain u), toByteString' (qUnqualified u)]
              . queryItem "skip_own" (toByteString' c1)
              . zUser (qUnqualified u')
          )
          <!! const 200 === statusCode
    liftIO $ Set.map (\e -> (e.user, e.client)) bundle.entries @?= Set.fromList [(u, c1), (u, c2)]

  -- check package counts again
  for_ [(c1, 2), (c2, 1)] $ \(c, n) -> do
    count <- getKeyPackageCount brig u c
    liftIO $ count @?= n

testKeyPackageRemoteClaim :: Opts -> Brig -> Http ()
testKeyPackageRemoteClaim opts brig = do
  traceM "sun"
  u <- fakeRemoteUser

  u' <- userQualifiedId <$> randomUser brig

  qcid <- mkClientIdentity u <$> randomClient
  entries <- withSystemTempDirectory "mls" $ \tmp -> do
    initStore tmp qcid
    replicateM 2 $ do
      (r, kp) <- generateKeyPackage tmp qcid Nothing
      pure $
        KeyPackageBundleEntry
          { user = u,
            client = ciClient qcid,
            ref = kp,
            keyPackage = KeyPackageData . raw $ r
          }
  let mockBundle = KeyPackageBundle (Set.fromList entries)
  traceM "gun"
  (bundle :: KeyPackageBundle, _reqs) <-
    liftIO . withTempMockFederator opts (Aeson.encode mockBundle) $
      responseJsonError
        =<< post
          ( brig
              . paths ["mls", "key-packages", "claim", toByteString' (qDomain u), toByteString' (qUnqualified u)]
              . zUser (qUnqualified u')
          )
          <!! const 200 === statusCode

  liftIO $ bundle @?= mockBundle
  traceM "fun"

--------------------------------------------------------------------------------

createClient :: Brig -> Qualified UserId -> Int -> Http ClientId
createClient brig u i =
  fmap clientId $
    responseJsonError
      =<< addClient
        brig
        (qUnqualified u)
        (defNewClient PermanentClientType [somePrekeys !! i] (someLastPrekeys !! i))
        <!! const 201 === statusCode
