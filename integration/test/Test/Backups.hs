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

--------------------------------------------------------------------------------
-- Helpers

featureName :: String
featureName = "automaticBackups"

enableBackups :: (HasCallStack, MakesValue owner) => owner -> String -> App ()
enableBackups owner tid =
  bindResponse (setTeamFeatureConfig owner tid featureName (object ["status" .= "enabled"])) $
    \resp -> resp.status `shouldMatchInt` 200

disableBackups :: (HasCallStack, MakesValue owner) => owner -> String -> App ()
disableBackups owner tid =
  bindResponse (setTeamFeatureConfig owner tid featureName (object ["status" .= "disabled"])) $
    \resp -> resp.status `shouldMatchInt` 200

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

firstFileId :: (HasCallStack, MakesValue user) => user -> String -> String -> Word32 -> App String
firstFileId user zcid cid epoch =
  bindResponse (listBackupFiles user zcid cid epoch) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "files.0.id" & asString

-- | Upload one file and return the presigned URL of its download redirect
uploadAndGetUrl :: (HasCallStack, MakesValue user) => user -> String -> Word32 -> App String
uploadAndGetUrl user cid epoch = do
  bindResponse (uploadBackup user cid cid epoch (BSC.pack "some backup")) $ \resp ->
    resp.status `shouldMatchInt` 200
  fid <- firstFileId user cid cid epoch
  bindResponse (downloadBackup user cid cid epoch fid) backupLocation

--------------------------------------------------------------------------------
-- Tests

testBackupRoundtrip :: (HasCallStack) => App ()
testBackupRoundtrip = do
  (owner, _, _) <- backupTeam OwnDomain
  c1 <- newClient owner
  let payload = BSC.pack "full backup content"
  bindResponse (uploadBackup owner c1 c1 1 payload) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "kind" `shouldMatch` "full"
    resp.json %. "size" `shouldMatchInt` BS.length payload
  bindResponse (listBackupEpochs owner c1 c1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "epochs" `shouldMatch` [1 :: Int]
  fid <- firstFileId owner c1 c1 1
  bindResponse (listBackupFiles owner c1 c1 1) $ \resp -> do
    resp.json %. "files.0.kind" `shouldMatch` "full"
    resp.json %. "files.0.size" `shouldMatchInt` BS.length payload
  url <- bindResponse (downloadBackup owner c1 c1 1 fid) backupLocation
  bindResponse (fetchUrl url) $ \resp -> do
    resp.status `shouldMatchInt` 200
    BSC.unpack resp.body `shouldMatch` BSC.unpack payload

testBackupEpochRules :: (HasCallStack) => App ()
testBackupEpochRules = do
  (owner, _, _) <- backupTeam OwnDomain
  c1 <- newClient owner
  let upload epoch = uploadBackup owner c1 c1 epoch (BSC.pack "x")
      expectOk epoch = bindResponse (upload epoch) $ \resp -> resp.status `shouldMatchInt` 200
      expectRejected epoch = bindResponse (upload epoch) $ \resp -> do
        resp.status `shouldMatchInt` 409
        resp.json %. "label" `shouldMatch` "backup-epoch-not-allowed"
  expectOk 1
  expectRejected 3
  expectOk 2
  expectRejected 3
  bindResponse (deleteBackupEpoch owner c1 c1 1) $ \resp -> resp.status `shouldMatchInt` 204
  expectOk 3

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
    let upload size = uploadBackup owner c1 c1 1 (BS.replicate size 0x61)
    bindResponse (upload 50) $ \resp -> resp.status `shouldMatchInt` 200
    bindResponse (upload 11) $ \resp -> do
      resp.status `shouldMatchInt` 413
      resp.json %. "label" `shouldMatch` "backup-too-large"
    bindResponse (upload 10) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "kind" `shouldMatch` "incremental"
    bindResponse (upload 5) $ \resp -> do
      resp.status `shouldMatchInt` 409
      resp.json %. "label" `shouldMatch` "backup-too-many-files"

testBackupCrossUser :: (HasCallStack) => App ()
testBackupCrossUser = do
  (owner, _, mate) <- backupTeam OwnDomain
  c1 <- newClient owner
  m1 <- newClient mate
  bindResponse (uploadBackup owner c1 c1 1 (BSC.pack "x")) $ \resp ->
    resp.status `shouldMatchInt` 200
  bindResponse (listBackupEpochs mate m1 c1) $ \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "backup-not-found"

testBackupOtherDeviceDelete :: (HasCallStack) => App ()
testBackupOtherDeviceDelete = do
  (owner, _, _) <- backupTeam OwnDomain
  c1 <- newClient owner
  c2 <- newClient owner
  bindResponse (uploadBackup owner c1 c1 1 (BSC.pack "x")) $ \resp ->
    resp.status `shouldMatchInt` 200
  -- other devices may read ...
  bindResponse (listBackupEpochs owner c2 c1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "epochs" `shouldMatch` [1 :: Int]
  -- ... but not write
  bindResponse (deleteBackupEpoch owner c2 c1 1) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "backup-forbidden"
  bindResponse (deleteClientBackups owner c2 c1) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "backup-forbidden"
  bindResponse (uploadBackup owner c2 c1 2 (BSC.pack "x")) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "backup-forbidden"
  bindResponse (listBackupEpochs owner c1 c1) $ \resp ->
    resp.json %. "epochs" `shouldMatch` [1 :: Int]

testBackupFeatureDisabled :: (HasCallStack) => App ()
testBackupFeatureDisabled = do
  (owner, tid, _) <- backupTeam OwnDomain
  c1 <- newClient owner
  bindResponse (uploadBackup owner c1 c1 1 (BSC.pack "x")) $ \resp ->
    resp.status `shouldMatchInt` 200
  disableBackups owner tid
  bindResponse (uploadBackup owner c1 c1 2 (BSC.pack "x")) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "backup-feature-disabled"
  bindResponse (listBackupEpochs owner c1 c1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "epochs" `shouldMatch` [1 :: Int]
  bindResponse (deleteBackupEpoch owner c1 c1 1) $ \resp ->
    resp.status `shouldMatchInt` 204

testBackupNonTeamUser :: (HasCallStack) => App ()
testBackupNonTeamUser = do
  user <- randomUser OwnDomain def
  c1 <- newClient user
  bindResponse (uploadBackup user c1 c1 1 (BSC.pack "x")) $ \resp -> do
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
  bindResponse (listBackupEpochs owner c2 c1) $ \resp -> resp.status `shouldMatchInt` 404
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
  bindResponse (uploadBackupBody owner c1 c1 1 chunked) $ \resp ->
    resp.status `shouldMatchOneOf` [400 :: Int, 411]
  bindResponse (listBackupEpochs owner c1 c1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "epochs" `shouldMatch` ([] :: [Int])

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
  bindResponse (uploadBackupBody owner c1 c1 1 body) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "kind" `shouldMatch` "full"
    resp.json %. "size" `shouldMatchInt` total
  inputDigest <- liftIO $ hashFinalize . snd <$> readIORef state
  fid <- firstFileId owner c1 c1 1
  url <- bindResponse (downloadBackup owner c1 c1 1 fid) backupLocation
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
