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

module Test.Backups where

import API.Brig (addClient, deleteClient)
import API.Cargohold
import API.Galley (setTeamFeatureConfig)
import Control.Monad.Reader (asks)
import Crypto.Hash (Context, Digest, SHA256, hashFinalize, hashInit, hashUpdate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.IORef
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status (statusCode)
import SetupHelpers
import Testlib.Prelude
import UnliftIO (pooledForConcurrentlyN)

-- REVIEW: Helpers should be below tests / using functions.

--------------------------------------------------------------------------------
-- Helpers

featureName :: String
featureName = "automaticBackups"

backupFeature :: String -> Value
backupFeature status =
  object
    [ "status" .= status,
      "config" .= object ["schedule" .= "weekly", "enforced" .= False, "serverOnly" .= False]
    ]

-- REVIEW: Quite some duplication with disableBackups. extract base function?
enableBackups :: (HasCallStack, MakesValue owner) => owner -> String -> App ()
enableBackups owner tid =
  bindResponse (setTeamFeatureConfig owner tid featureName (backupFeature "enabled"))
    $ \resp -> resp.status `shouldMatchInt` 200

disableBackups :: (HasCallStack, MakesValue owner) => owner -> String -> App ()
disableBackups owner tid =
  bindResponse (setTeamFeatureConfig owner tid featureName (backupFeature "disabled"))
    $ \resp -> resp.status `shouldMatchInt` 200

newClient :: (HasCallStack, MakesValue user) => user -> App String
newClient user = addClient user def >>= getJSON 201 >>= objId

-- | Team with backups enabled: owner, team id, another mate
backupTeam :: (HasCallStack, MakesValue domain) => domain -> App (Value, String, Value)
backupTeam domain = do
  (owner, tid, [mate]) <- createTeam domain 2
  enableBackups owner tid
  pure (owner, tid, mate)

-- | Content of the Location header of a redirect response
backupLocation :: (HasCallStack) => Response -> App String
backupLocation resp = do
  resp.status `shouldMatchInt` 302
  case locationHeader resp of
    Nothing -> assertFailure "missing Location header"
    Just (_, loc) -> pure (BSC.unpack loc)

-- | Plain GET of a (presigned) URL
fetchUrl :: (HasCallStack) => String -> App Response
fetchUrl url = externalRequest url >>= submit "GET"

-- | Epoch numbers of a client's backup summary, ascending
epochNumbers :: (HasCallStack) => Response -> App [Int]
epochNumbers resp = resp.json %. "epochs" & asList >>= traverse (\e -> e %. "epoch" & asInt)

-- | Id of the current full file of an epoch; it is needed for the download routes
currentFullId :: (HasCallStack, MakesValue user) => user -> String -> String -> Word32 -> App String
currentFullId user zcid cid epoch =
  bindResponse (listBackupFiles user zcid cid epoch) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "full.id" & asString

-- | Uploads a full file and returns its id
uploadFull :: (HasCallStack, MakesValue user) => user -> String -> Word32 -> ByteString -> App String
uploadFull user cid epoch bs =
  bindResponse (uploadBackup user cid cid epoch Nothing bs) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "id" & asString

-- | Upload one file and return the presigned URL of its download redirect
uploadAndGetUrl :: (HasCallStack, MakesValue user) => user -> String -> Word32 -> App String
uploadAndGetUrl user cid epoch = do
  fid <- uploadFull user cid epoch (BSC.pack "some backup")
  bindResponse (downloadBackupFile user cid cid epoch fid Nothing) backupLocation

--------------------------------------------------------------------------------
-- Tests

testBackupRoundtrip :: (HasCallStack) => App ()
testBackupRoundtrip = do
  (owner, _, _) <- backupTeam OwnDomain
  -- REVIEW: Use cid and only a number if there is more than one. (Fix this in whole diff)
  c1 <- newClient owner
  let payload = BSC.pack "full backup content"
  bindResponse (uploadBackup owner c1 c1 1 Nothing payload) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "size" `shouldMatchInt` BS.length payload
  bindResponse (listClientBackups owner c1 c1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    (epochNumbers >=> (`shouldMatch` [1 :: Int])) resp
  fid <- currentFullId owner c1 c1 1
  bindResponse (listBackupFiles owner c1 c1 1) $ \resp -> do
    resp.json %. "full.size" `shouldMatchInt` BS.length payload
    (resp.json %. "incrementals" & asList) >>= \is -> length is `shouldMatchInt` 0
  url <- bindResponse (downloadBackupFile owner c1 c1 1 fid Nothing) backupLocation
  bindResponse (fetchUrl url) $ \resp -> do
    resp.status `shouldMatchInt` 200
    BSC.unpack resp.body `shouldMatch` BSC.unpack payload

testBackupEpochRules :: (HasCallStack) => App ()
testBackupEpochRules = do
  (owner, _, _) <- backupTeam OwnDomain
  c1 <- newClient owner
  let upload epoch = uploadBackup owner c1 c1 epoch Nothing (BSC.pack "x")
      expectOk epoch = bindResponse (upload epoch) $ \resp -> resp.status `shouldMatchInt` 200
      expectRejected epoch = bindResponse (upload epoch) $ \resp -> do
        resp.status `shouldMatchInt` 409
        resp.json %. "label" `shouldMatch` "backup-epoch-not-allowed"
  expectOk 1
  expectRejected 3
  expectOk 2
  -- the successor of the newest epoch is allowed, only the two newest epochs are kept
  expectOk 3
  expectRejected 5
  bindResponse (listClientBackups owner c1 c1) $ \resp ->
    (epochNumbers >=> (`shouldMatch` [2 :: Int, 3])) resp

testBackupCaps :: (HasCallStack) => App ()
testBackupCaps = do
  let limits =
        def
          { cargoholdCfg =
              setField "settings.backupLimits.maxFullBytes" (100 :: Int)
                >=> setField "settings.backupLimits.maxIncrementalBytes" (10 :: Int)
                >=> setField "settings.backupLimits.maxFilesPerEpoch" (2 :: Int)
          }
  withModifiedBackend limits $ \domain -> do
    (owner, _, _) <- backupTeam domain
    c1 <- newClient owner
    let upload base size = uploadBackup owner c1 c1 1 base (BS.replicate size 0x61)
    bindResponse (upload Nothing 101) $ \resp -> do
      resp.status `shouldMatchInt` 413
      resp.json %. "label" `shouldMatch` "backup-too-large"
    fid <- uploadFull owner c1 1 (BS.replicate 50 0x61)
    bindResponse (upload (Just fid) 11) $ \resp -> do
      resp.status `shouldMatchInt` 413
      resp.json %. "label" `shouldMatch` "backup-too-large"
    bindResponse (upload (Just fid) 10) $ \resp ->
      resp.status `shouldMatchInt` 200
    bindResponse (upload (Just fid) 5) $ \resp -> do
      resp.status `shouldMatchInt` 409
      resp.json %. "label" `shouldMatch` "backup-too-many-files"

testBackupCrossUser :: (HasCallStack) => App ()
testBackupCrossUser = do
  (owner, _, mate) <- backupTeam OwnDomain
  c1 <- newClient owner
  m1 <- newClient mate
  bindResponse (uploadBackup owner c1 c1 1 Nothing (BSC.pack "x")) $ \resp ->
    resp.status `shouldMatchInt` 200
  bindResponse (listClientBackups mate m1 c1) $ \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "backup-not-found"

testBackupOtherDeviceWrite :: (HasCallStack) => App ()
testBackupOtherDeviceWrite = do
  (owner, _, _) <- backupTeam OwnDomain
  c1 <- newClient owner
  c2 <- newClient owner
  bindResponse (uploadBackup owner c1 c1 1 Nothing (BSC.pack "x")) $ \resp ->
    resp.status `shouldMatchInt` 200
  -- other devices may read ...
  bindResponse (listClientBackups owner c2 c1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    (epochNumbers >=> (`shouldMatch` [1 :: Int])) resp
  -- ... but not write
  bindResponse (uploadBackup owner c2 c1 2 Nothing (BSC.pack "x")) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "backup-forbidden"
  bindResponse (listClientBackups owner c1 c1) $ \resp ->
    (epochNumbers >=> (`shouldMatch` [1 :: Int])) resp

testBackupFeatureDisabled :: (HasCallStack) => App ()
testBackupFeatureDisabled = do
  (owner, tid, _) <- backupTeam OwnDomain
  c1 <- newClient owner
  bindResponse (uploadBackup owner c1 c1 1 Nothing (BSC.pack "x")) $ \resp ->
    resp.status `shouldMatchInt` 200
  disableBackups owner tid
  bindResponse (uploadBackup owner c1 c1 2 Nothing (BSC.pack "x")) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "backup-feature-disabled"
  bindResponse (listClientBackups owner c1 c1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    (epochNumbers >=> (`shouldMatch` [1 :: Int])) resp

testBackupNonTeamUser :: (HasCallStack) => App ()
testBackupNonTeamUser = do
  user <- randomUser OwnDomain def
  c1 <- newClient user
  bindResponse (uploadBackup user c1 c1 1 Nothing (BSC.pack "x")) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "backup-feature-disabled"

testBackupDeletedOnClientRemoval :: (HasCallStack) => App ()
testBackupDeletedOnClientRemoval = do
  (owner, _, _) <- backupTeam OwnDomain
  c1 <- newClient owner
  c2 <- newClient owner
  url <- uploadAndGetUrl owner c1 1
  bindResponse (fetchUrl url) $ \resp -> resp.status `shouldMatchInt` 200
  bindResponse (deleteClient owner c1) $ \resp -> resp.status `shouldMatchInt` 200
  -- the client is gone, so its backups are not listed anymore ...
  bindResponse (listClientBackups owner c2 c1) $ \resp -> resp.status `shouldMatchInt` 404
  -- ... and the blob itself is deleted
  eventually $ bindResponse (fetchUrl url) $ \resp -> resp.status `shouldMatchInt` 404

testBackupDeletedOnUserDeletion :: (HasCallStack) => App ()
testBackupDeletedOnUserDeletion = do
  (_, _, mate) <- backupTeam OwnDomain
  m1 <- newClient mate
  url <- uploadAndGetUrl mate m1 1
  bindResponse (fetchUrl url) $ \resp -> resp.status `shouldMatchInt` 200
  deleteUser mate
  eventually $ bindResponse (fetchUrl url) $ \resp -> resp.status `shouldMatchInt` 404

testBackupMissingContentLength :: (HasCallStack) => App ()
testBackupMissingContentLength = do
  (owner, _, _) <- backupTeam OwnDomain
  c1 <- newClient owner
  -- chunked transfer encoding: no Content-Length header
  let chunked = HTTP.RequestBodyStreamChunked $ \needsPopper -> do
        chunks <- newIORef [BSC.pack "some backup"]
        needsPopper $ atomicModifyIORef' chunks $ \case
          [] -> ([], BS.empty)
          x : xs -> (xs, x)
  bindResponse (uploadBackupBody owner c1 c1 1 Nothing chunked) $ \resp ->
    resp.status `shouldMatchOneOf` [400 :: Int, 411]
  bindResponse (listClientBackups owner c1 c1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    (epochNumbers >=> (`shouldMatch` ([] :: [Int]))) resp

testBackupLargeUploadRoundtrip :: (HasCallStack) => App ()
testBackupLargeUploadRoundtrip = do
  (owner, _, _) <- backupTeam OwnDomain
  c1 <- newClient owner
  let chunkSize = 4096
      total = 1024 * 1024 :: Int
      chunkAt :: Int -> ByteString
      chunkAt i = BS.pack [fromIntegral (i * 31 + j) | j <- [0 .. chunkSize - 1]]
  -- body is generated lazily while streaming, the input hash is computed on the way
  state <- liftIO $ newIORef (0 :: Int, hashInit :: Context SHA256)
  let popper = atomicModifyIORef' state $ \(i, ctx) ->
        if i * chunkSize >= total
          then ((i, ctx), BS.empty)
          else let c = chunkAt i in ((i + 1, hashUpdate ctx c), c)
      body = HTTP.RequestBodyStream (fromIntegral total) ($ popper)
  bindResponse (uploadBackupBody owner c1 c1 1 Nothing body) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "size" `shouldMatchInt` total
  inputDigest <- liftIO $ hashFinalize . snd <$> readIORef state
  fid <- currentFullId owner c1 c1 1
  url <- bindResponse (downloadBackupFile owner c1 c1 1 fid Nothing) backupLocation
  req <- externalRequest url
  manager <- asks (.manager)
  (status, outputDigest, received) <- liftIO $ HTTP.withResponse req manager $ \res -> do
    let loop ctx n = do
          chunk <- HTTP.brRead res.responseBody
          if BS.null chunk
            then pure (hashFinalize ctx, n)
            else loop (hashUpdate ctx chunk) (n + BS.length chunk)
    (d, n) <- loop (hashInit :: Context SHA256) 0
    pure (statusCode (HTTP.responseStatus res), d, n)
  status `shouldMatchInt` 200
  received `shouldMatchInt` total
  show (outputDigest :: Digest SHA256) `shouldMatch` show inputDigest

testBackupKeysHideIds :: (HasCallStack) => App ()
testBackupKeysHideIds = do
  (owner, _, _) <- backupTeam OwnDomain
  c1 <- newClient owner
  uid <- objId owner
  url <- uploadAndGetUrl owner c1 1
  url `shouldContain` "/backup/"
  for_ [("user id", uid), ("client id", c1)] $ \(what, plain) ->
    when (plain `isInfixOf` url)
      $ assertFailure ("presigned URL contains the plain " <> what)

testBackupManyFiles :: (HasCallStack) => App ()
testBackupManyFiles = do
  (owner, _, _) <- backupTeam OwnDomain
  c1 <- newClient owner
  let nFiles = 1005 :: Int -- more than one S3 page and one DeleteObjects batch (1000)
  fid <- uploadFull owner c1 1 (BSC.pack "x")
  for_ [2 .. nFiles] $ \_ ->
    bindResponse (uploadBackup owner c1 c1 1 (Just fid) (BSC.pack "x")) $ \resp ->
      resp.status `shouldMatchInt` 200
  bindResponse (listBackupFiles owner c1 c1 1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "full.id" `shouldMatch` fid
    incrementals <- resp.json %. "incrementals" & asList
    length incrementals `shouldMatchInt` (nFiles - 1)

testBackupEpochPrefixIsolation :: (HasCallStack) => App ()
testBackupEpochPrefixIsolation = do
  (owner, _, _) <- backupTeam OwnDomain
  a <- newClient owner
  b <- newClient owner
  let upload cid epoch base =
        bindResponse (uploadBackup owner cid cid epoch base (BSC.pack "x")) $ \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "id" & asString
  fid <- upload a 1 Nothing
  void $ upload a 1 (Just fid)
  void $ upload a 2 Nothing
  void $ upload b 1 Nothing
  bindResponse (listClientBackups owner a a) $ \resp ->
    (epochNumbers >=> (`shouldMatch` [1 :: Int, 2])) resp
  bindResponse (listBackupFiles owner a a 1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    (resp.json %. "incrementals" & asList) >>= \is -> length is `shouldMatchInt` 1
  bindResponse (listBackupFiles owner a a 2) $ \resp -> do
    resp.status `shouldMatchInt` 200
    (resp.json %. "incrementals" & asList) >>= \is -> length is `shouldMatchInt` 0
  bindResponse (listBackupFiles owner b b 1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    (resp.json %. "incrementals" & asList) >>= \is -> length is `shouldMatchInt` 0

testBackupFullReplaces :: (HasCallStack) => App ()
testBackupFullReplaces = do
  (owner, _, _) <- backupTeam OwnDomain
  c1 <- newClient owner
  oldFid <- uploadFull owner c1 1 (BSC.pack "x")
  bindResponse (uploadBackup owner c1 c1 1 (Just oldFid) (BSC.pack "i")) $ \resp ->
    resp.status `shouldMatchInt` 200
  -- a new full replaces the old full and its incrementals
  newFid <- uploadFull owner c1 1 (BSC.pack "yy")
  bindResponse (listBackupFiles owner c1 c1 1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "full.id" `shouldMatch` newFid
    resp.json %. "full.size" `shouldMatchInt` 2
    (resp.json %. "incrementals" & asList) >>= \is -> length is `shouldMatchInt` 0
  -- the old full is gone: no incremental can be stored under it
  bindResponse (uploadBackup owner c1 c1 1 (Just oldFid) (BSC.pack "late")) $ \resp -> do
    resp.status `shouldMatchInt` 409
    resp.json %. "label" `shouldMatch` "backup-full-missing"
  bindResponse (downloadBackupFile owner c1 c1 1 oldFid Nothing) $ \resp ->
    resp.status `shouldMatchInt` 404

testBackupIncrementalNeedsFull :: (HasCallStack) => App ()
testBackupIncrementalNeedsFull = do
  (owner, _, _) <- backupTeam OwnDomain
  c1 <- newClient owner
  bindResponse (uploadBackup owner c1 c1 1 (Just "00000000-0000-0000-0000-000000000000") (BSC.pack "x")) $ \resp -> do
    resp.status `shouldMatchInt` 409
    resp.json %. "label" `shouldMatch` "backup-full-missing"
  bindResponse (listBackupFiles owner c1 c1 1) $ \resp -> resp.status `shouldMatchInt` 404

-- | All parallel full uploads succeed, exactly one full file is left afterwards.
testBackupConcurrentFulls :: (HasCallStack) => App ()
testBackupConcurrentFulls = do
  (owner, _, _) <- backupTeam OwnDomain
  c1 <- newClient owner
  void $ pooledForConcurrentlyN 4 [1 .. 4 :: Int] $ \_ ->
    bindResponse (uploadBackup owner c1 c1 1 Nothing (BSC.pack "x")) $ \resp ->
      resp.status `shouldMatchInt` 200
  bindResponse (listBackupFiles owner c1 c1 1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    (resp.json %. "incrementals" & asList) >>= \is -> length is `shouldMatchInt` 0

testBackupListAll :: (HasCallStack) => App ()
testBackupListAll = do
  (owner, _, _) <- backupTeam OwnDomain
  c1 <- newClient owner
  c2 <- newClient owner
  c3 <- newClient owner
  bindResponse (listBackups owner c1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "clients" `shouldMatch` ([] :: [Value])
  bindResponse (uploadBackup owner c1 c1 1 Nothing (BSC.pack "x")) $ \resp ->
    resp.status `shouldMatchInt` 200
  fid2 <- uploadFull owner c2 1 (BSC.pack "y")
  bindResponse (uploadBackup owner c2 c2 1 (Just fid2) (BSC.pack "z")) $ \resp ->
    resp.status `shouldMatchInt` 200
  -- a new client lists the backups of the others; c3 has none
  bindResponse (listBackups owner c3) $ \resp -> do
    resp.status `shouldMatchInt` 200
    clients <- resp.json %. "clients" & asList
    length clients `shouldMatchInt` 2
    -- newest first
    (head clients %. "client") `shouldMatch` c2
    (head clients %. "epochs.0.epoch") `shouldMatchInt` 1
    (head clients %. "epochs.0.fileCount") `shouldMatchInt` 2
    (head clients %. "epochs.0.size") `shouldMatchInt` 2
