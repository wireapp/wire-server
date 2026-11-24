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
{-# LANGUAGE OverloadedRecordDot #-}

module Test.Backups where

import API.Brig (addClient, deleteClient)
import API.Cargohold
import qualified API.GalleyInternal as Internal
import qualified Data.ByteString.Char8 as BC
import Data.CaseInsensitive (mk)
import qualified Network.HTTP.Client as HTTP
import SetupHelpers
import Testlib.Prelude
import UnliftIO (try)
import UnliftIO.Async (mapConcurrently)
import UnliftIO.Concurrent (threadDelay)

-- | A team owner with the feature @automaticBackups@ enabled and a client.
setupBackupUser :: (HasCallStack) => App (Value, String)
setupBackupUser = setupBackupUserOn OwnDomain

setupBackupUserOn :: (HasCallStack, MakesValue domain) => domain -> App (Value, String)
setupBackupUserOn domain = do
  (owner, tid, _) <- createTeam domain 0
  Internal.setTeamFeatureLockStatus owner tid "automaticBackups" "unlocked"
  bindResponse (Internal.setTeamFeatureStatus owner tid "automaticBackups" "enabled") $ \resp ->
    resp.status `shouldMatchInt` 200
  cid <- newClientId owner
  pure (owner, cid)

newClientId :: (HasCallStack, MakesValue user) => user -> App String
newClientId user = do
  c <- addClient user def >>= getJSON 201
  c %. "id" & asString

testBackupUploadFullAndList :: (HasCallStack) => App ()
testBackupUploadFullAndList = do
  (owner, cid) <- setupBackupUser
  fileId <- bindResponse (uploadBackupFull owner cid cid 1 (BC.replicate 1024 'x')) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "size" `shouldMatchInt` 1024
    resp.json %. "id" & asString
  bindResponse (getBackupEpoch owner cid cid 1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "full.id" `shouldMatch` fileId
    resp.json %. "incrementals" `shouldMatch` ([] :: [Value])

testBackupEpochRules :: (HasCallStack) => App ()
testBackupEpochRules = do
  (owner, cid) <- setupBackupUser
  let upload epoch = uploadBackupFull owner cid cid epoch (BC.replicate 16 'x')
  bindResponse (upload 2) $ \resp -> do
    resp.status `shouldMatchInt` 409
    resp.json %. "label" `shouldMatch` "backup-epoch-invalid"
  firstId <- bindResponse (upload 1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "id" & asString
  secondId <- bindResponse (upload 1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "id" & asString
  assertBool "replacing a full yields a new id" (firstId /= secondId)
  for_ [2, 3] $ \epoch -> bindResponse (upload epoch) $ \resp -> resp.status `shouldMatchInt` 200
  bindResponse (getBackupEpoch owner cid cid 1) $ \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "backup-not-found"
  bindResponse (getBackupEpoch owner cid cid 2) $ \resp -> resp.status `shouldMatchInt` 200

testBackupForbiddenOtherClient :: (HasCallStack) => App ()
testBackupForbiddenOtherClient = do
  (owner, cid) <- setupBackupUser
  other <- newClientId owner
  bindResponse (uploadBackupFull owner other cid 1 (BC.replicate 16 'x')) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "backup-forbidden"

testBackupRemovedClient :: (HasCallStack) => App ()
testBackupRemovedClient = do
  (owner, _) <- setupBackupUser
  removed <- newClientId owner
  deleteClient owner removed >>= assertSuccess
  bindResponse (uploadBackupFull owner removed removed 1 (BC.replicate 16 'x')) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "backup-forbidden"

testBackupDisabled :: (HasCallStack) => App ()
testBackupDisabled = do
  -- feature not enabled
  (owner, tid, _) <- createTeam OwnDomain 0
  Internal.setTeamFeatureLockStatus owner tid "automaticBackups" "unlocked"
  cid <- newClientId owner
  bindResponse (uploadBackupFull owner cid cid 1 (BC.replicate 16 'x')) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "backup-disabled"
  -- user without team
  personal <- randomUser OwnDomain def
  personalCid <- newClientId personal
  bindResponse (uploadBackupFull personal personalCid personalCid 1 (BC.replicate 16 'x')) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "backup-disabled"

testBackupTooLarge :: (HasCallStack) => App ()
testBackupTooLarge = do
  (owner, cid) <- setupBackupUser
  -- the limit is 1048576 bytes in the integration config
  bindResponse (uploadBackupFull owner cid cid 1 (BC.replicate 1048577 'x')) $ \resp -> do
    resp.status `shouldMatchInt` 413
    resp.json %. "label" `shouldMatch` "backup-too-large"

testBackupOtherUserCannotRead :: (HasCallStack) => App ()
testBackupOtherUserCannotRead = do
  (alice, aliceCid) <- setupBackupUser
  bindResponse (uploadBackupFull alice aliceCid aliceCid 1 (BC.replicate 16 'x')) $ \resp ->
    resp.status `shouldMatchInt` 200
  (bob, bobCid) <- setupBackupUser
  bindResponse (getBackupEpoch bob bobCid aliceCid 1) $ \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "backup-not-found"

testBackupDownloadFull :: (HasCallStack) => App ()
testBackupDownloadFull = do
  (owner, cid) <- setupBackupUser
  let bytes = BC.replicate 1024 'x'
  fileId <- bindResponse (uploadBackupFull owner cid cid 1 bytes) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "id" & asString
  bindResponse (downloadBackupFull owner cid cid 1 fileId) $ \resp -> do
    resp.status `shouldMatchInt` 302
    let location = BC.unpack (getHeader' (mk (BC.pack "Location")) resp)
    req <- liftIO $ HTTP.parseRequest location
    -- the presigned URL needs no authentication
    s3Resp <- submit "GET" req
    s3Resp.status `shouldMatchInt` 200
    assertBool "downloaded bytes equal the uploaded ones" (s3Resp.body == bytes)

testBackupDownloadReplacedFull :: (HasCallStack) => App ()
testBackupDownloadReplacedFull = do
  (owner, cid) <- setupBackupUser
  let upload = bindResponse (uploadBackupFull owner cid cid 1 (BC.replicate 16 'x')) $ \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "id" & asString
  oldId <- upload
  newId <- upload
  bindResponse (downloadBackupFull owner cid cid 1 oldId) $ \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "backup-not-found"
  bindResponse (downloadBackupFull owner cid cid 1 newId) $ \resp ->
    resp.status `shouldMatchInt` 302

testBackupDownloadOtherUser :: (HasCallStack) => App ()
testBackupDownloadOtherUser = do
  (alice, aliceCid) <- setupBackupUser
  fileId <- bindResponse (uploadBackupFull alice aliceCid aliceCid 1 (BC.replicate 16 'x')) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "id" & asString
  (bob, bobCid) <- setupBackupUser
  bindResponse (downloadBackupFull bob bobCid aliceCid 1 fileId) $ \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "backup-not-found"

-- | Upload a full of the epoch (16 bytes) and return its id.
uploadFullId :: (HasCallStack) => Value -> String -> Int -> App String
uploadFullId owner cid epoch =
  bindResponse (uploadBackupFull owner cid cid epoch (BC.replicate 16 'x')) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "id" & asString

-- | Upload an incremental of the given size to the epoch's full and return its id.
uploadIncrementalId :: (HasCallStack) => Value -> String -> Int -> String -> Int -> App String
uploadIncrementalId owner cid epoch fullId size =
  bindResponse (uploadBackupIncremental owner cid cid epoch fullId (BC.replicate size 'i')) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "size" `shouldMatchInt` size
    resp.json %. "id" & asString

testBackupUploadIncrementalAndList :: (HasCallStack) => App ()
testBackupUploadIncrementalAndList = do
  (owner, cid) <- setupBackupUser
  fullId <- bindResponse (uploadBackupFull owner cid cid 1 (BC.replicate 1024 'x')) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "id" & asString
  id1 <- uploadIncrementalId owner cid 1 fullId 100
  id2 <- uploadIncrementalId owner cid 1 fullId 200
  bindResponse (getBackupEpoch owner cid cid 1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "full.id" `shouldMatch` fullId
    incs <- resp.json %. "incrementals" & asList
    traverse (%. "id") incs `shouldMatch` [id1, id2]
    traverse (%. "size") incs `shouldMatch` [100 :: Int, 200]

testBackupIncrementalStaleFull :: (HasCallStack) => App ()
testBackupIncrementalStaleFull = do
  (owner, cid) <- setupBackupUser
  oldFull <- uploadFullId owner cid 1
  _ <- uploadFullId owner cid 1
  bindResponse (uploadBackupIncremental owner cid cid 1 oldFull (BC.replicate 16 'i')) $ \resp -> do
    resp.status `shouldMatchInt` 409
    resp.json %. "label" `shouldMatch` "backup-full-missing"
  unknownFull <- randomId
  bindResponse (uploadBackupIncremental owner cid cid 1 unknownFull (BC.replicate 16 'i')) $ \resp -> do
    resp.status `shouldMatchInt` 409
    resp.json %. "label" `shouldMatch` "backup-full-missing"

testBackupEpochFull :: (HasCallStack) => App ()
testBackupEpochFull = do
  (owner, cid) <- setupBackupUser
  fullId <- uploadFullId owner cid 1
  -- the limit is 3 files (including the full) in the integration config
  replicateM_ 2 $ uploadIncrementalId owner cid 1 fullId 16
  bindResponse (uploadBackupIncremental owner cid cid 1 fullId (BC.replicate 16 'i')) $ \resp -> do
    resp.status `shouldMatchInt` 409
    resp.json %. "label" `shouldMatch` "backup-epoch-full"

-- | The pre-check lets both pass; only the count under the lock in the commit stops one.
testBackupConcurrentIncrementalsAtLimit :: (HasCallStack) => App ()
testBackupConcurrentIncrementalsAtLimit = do
  (owner, cid) <- setupBackupUser
  fullId <- uploadFullId owner cid 1
  _ <- uploadIncrementalId owner cid 1 fullId 16
  responses <-
    mapConcurrently
      (\_ -> uploadBackupIncremental owner cid cid 1 fullId (BC.replicate 16 'i'))
      [1 :: Int, 2]
  sort (map (.status) responses) `shouldMatch` ([200, 409] :: [Int])
  for_ (filter ((== 409) . (.status)) responses) $ \resp ->
    resp.json %. "label" `shouldMatch` "backup-epoch-full"
  bindResponse (getBackupEpoch owner cid cid 1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    incs <- resp.json %. "incrementals" & asList
    length incs `shouldMatchInt` 2

testBackupIncrementalTooLarge :: (HasCallStack) => App ()
testBackupIncrementalTooLarge = do
  (owner, cid) <- setupBackupUser
  fullId <- uploadFullId owner cid 1
  -- the limit is 65536 bytes in the integration config
  bindResponse (uploadBackupIncremental owner cid cid 1 fullId (BC.replicate 65537 'i')) $ \resp -> do
    resp.status `shouldMatchInt` 413
    resp.json %. "label" `shouldMatch` "backup-too-large"

testBackupIncrementalForbiddenOtherClient :: (HasCallStack) => App ()
testBackupIncrementalForbiddenOtherClient = do
  (owner, cid) <- setupBackupUser
  other <- newClientId owner
  fullId <- uploadFullId owner cid 1
  bindResponse (uploadBackupIncremental owner other cid 1 fullId (BC.replicate 16 'i')) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "backup-forbidden"

-- | Replacing the full orphans its incrementals (real foreign key).
testBackupReplaceFullWithIncrementals :: (HasCallStack) => App ()
testBackupReplaceFullWithIncrementals = do
  (owner, cid) <- setupBackupUser
  oldFull <- uploadFullId owner cid 1
  oldIncs <- replicateM 2 $ uploadIncrementalId owner cid 1 oldFull 16
  _ <- uploadFullId owner cid 1
  bindResponse (getBackupEpoch owner cid cid 1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "incrementals" `shouldMatch` ([] :: [Value])
  for_ oldIncs $ \incId ->
    bindResponse (downloadBackupIncremental owner cid cid 1 oldFull incId) $ \resp -> do
      resp.status `shouldMatchInt` 404
      resp.json %. "label" `shouldMatch` "backup-not-found"

testBackupDownloadIncremental :: (HasCallStack) => App ()
testBackupDownloadIncremental = do
  (owner, cid) <- setupBackupUser
  fullId <- uploadFullId owner cid 1
  let bytes = BC.replicate 100 'i'
  incId <- bindResponse (uploadBackupIncremental owner cid cid 1 fullId bytes) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "id" & asString
  bindResponse (downloadBackupIncremental owner cid cid 1 fullId incId) $ \resp -> do
    resp.status `shouldMatchInt` 302
    let location = BC.unpack (getHeader' (mk (BC.pack "Location")) resp)
    req <- liftIO $ HTTP.parseRequest location
    -- the presigned URL needs no authentication
    s3Resp <- submit "GET" req
    s3Resp.status `shouldMatchInt` 200
    assertBool "downloaded bytes equal the uploaded ones" (s3Resp.body == bytes)

testBackupDownloadIncrementalWrongFull :: (HasCallStack) => App ()
testBackupDownloadIncrementalWrongFull = do
  (owner, cid) <- setupBackupUser
  fullId1 <- uploadFullId owner cid 1
  incId <- uploadIncrementalId owner cid 1 fullId1 16
  fullId2 <- uploadFullId owner cid 2
  bindResponse (downloadBackupIncremental owner cid cid 2 fullId2 incId) $ \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "backup-not-found"

testBackupDownloadIncrementalOtherUser :: (HasCallStack) => App ()
testBackupDownloadIncrementalOtherUser = do
  (alice, aliceCid) <- setupBackupUser
  fullId <- uploadFullId alice aliceCid 1
  incId <- uploadIncrementalId alice aliceCid 1 fullId 16
  (bob, bobCid) <- setupBackupUser
  bindResponse (downloadBackupIncremental bob bobCid aliceCid 1 fullId incId) $ \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "backup-not-found"

-- | An epoch with a full of 1024 bytes and an incremental of 512 bytes; returns the creation time
-- of the incremental.
uploadFullAndIncremental :: (HasCallStack) => Value -> String -> App String
uploadFullAndIncremental owner cid = do
  fullId <- bindResponse (uploadBackupFull owner cid cid 1 (BC.replicate 1024 'x')) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "id" & asString
  bindResponse (uploadBackupIncremental owner cid cid 1 fullId (BC.replicate 512 'i')) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "createdAt" & asString

testBackupList :: (HasCallStack) => App ()
testBackupList = do
  (owner, cid) <- setupBackupUser
  createdAt <- uploadFullAndIncremental owner cid
  bindResponse (listBackups owner cid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    clients <- resp.json %. "clients" & asList
    entry <- assertOne clients
    entry %. "client" `shouldMatch` cid
    epochs <- entry %. "epochs" & asList
    epoch <- assertOne epochs
    epoch %. "epoch" `shouldMatchInt` 1
    epoch %. "createdAt" `shouldMatch` createdAt
    epoch %. "fileCount" `shouldMatchInt` 2
    epoch %. "size" `shouldMatchInt` 1536

testBackupGetClient :: (HasCallStack) => App ()
testBackupGetClient = do
  (owner, cid) <- setupBackupUser
  createdAt <- uploadFullAndIncremental owner cid
  cid2 <- newClientId owner
  bindResponse (getClientBackups owner cid cid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "client" `shouldMatch` cid
    epoch <- resp.json %. "epochs" >>= asList >>= assertOne
    epoch %. "epoch" `shouldMatchInt` 1
    epoch %. "createdAt" `shouldMatch` createdAt
    epoch %. "fileCount" `shouldMatchInt` 2
    epoch %. "size" `shouldMatchInt` 1536
  bindResponse (getClientBackups owner cid cid2) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "client" `shouldMatch` cid2
    resp.json %. "epochs" `shouldMatch` ([] :: [Value])

testBackupListFromOtherClient :: (HasCallStack) => App ()
testBackupListFromOtherClient = do
  (owner, cid) <- setupBackupUser
  _ <- uploadFullAndIncremental owner cid
  cid2 <- newClientId owner
  bindResponse (listBackups owner cid2) $ \resp -> do
    resp.status `shouldMatchInt` 200
    entry <- resp.json %. "clients" >>= asList >>= assertOne
    entry %. "client" `shouldMatch` cid

testBackupListOtherUser :: (HasCallStack) => App ()
testBackupListOtherUser = do
  (alice, aliceCid) <- setupBackupUser
  _ <- uploadFullAndIncremental alice aliceCid
  (bob, bobCid) <- setupBackupUser
  bindResponse (listBackups bob bobCid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "clients" `shouldMatch` ([] :: [Value])
  bindResponse (getClientBackups bob bobCid aliceCid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "client" `shouldMatch` aliceCid
    resp.json %. "epochs" `shouldMatch` ([] :: [Value])

testBackupListAfterReplace :: (HasCallStack) => App ()
testBackupListAfterReplace = do
  (owner, cid) <- setupBackupUser
  _ <- uploadFullAndIncremental owner cid
  bindResponse (uploadBackupFull owner cid cid 1 (BC.replicate 2048 'y')) $ \resp ->
    resp.status `shouldMatchInt` 200
  bindResponse (listBackups owner cid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    entry <- resp.json %. "clients" >>= asList >>= assertOne
    epoch <- entry %. "epochs" >>= asList >>= assertOne
    epoch %. "fileCount" `shouldMatchInt` 1
    epoch %. "size" `shouldMatchInt` 2048

testBackupListDisabled :: (HasCallStack) => App ()
testBackupListDisabled = do
  -- feature not enabled
  (owner, tid, _) <- createTeam OwnDomain 0
  Internal.setTeamFeatureLockStatus owner tid "automaticBackups" "unlocked"
  cid <- newClientId owner
  bindResponse (listBackups owner cid) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "backup-disabled"
  bindResponse (getClientBackups owner cid cid) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "backup-disabled"
  -- removed Z-Client
  (owner', removed) <- setupBackupUser
  deleteClient owner' removed >>= assertSuccess
  bindResponse (listBackups owner' removed) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "backup-forbidden"
  bindResponse (getClientBackups owner' removed removed) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "backup-forbidden"

-- DELETE

-- | The presigned download URL of a full, as a request.
presignedFullRequest :: (HasCallStack) => Value -> String -> Int -> String -> App HTTP.Request
presignedFullRequest owner cid epoch fullId =
  bindResponse (downloadBackupFull owner cid cid epoch fullId) $ \resp -> do
    resp.status `shouldMatchInt` 302
    liftIO $ HTTP.parseRequest (BC.unpack (getHeader' (mk (BC.pack "Location")) resp))

testBackupDeleteClient :: (HasCallStack) => App ()
testBackupDeleteClient = do
  (owner, cid) <- setupBackupUser
  fullId <- uploadFullId owner cid 1
  _ <- uploadIncrementalId owner cid 1 fullId 100
  _ <- uploadFullId owner cid 2
  bindResponse (deleteClientBackups owner cid cid) $ \resp ->
    resp.status `shouldMatchInt` 204
  for_ [1, 2] $ \epoch ->
    bindResponse (getBackupEpoch owner cid cid epoch) $ \resp -> do
      resp.status `shouldMatchInt` 404
      resp.json %. "label" `shouldMatch` "backup-not-found"
  -- nothing left to delete
  bindResponse (deleteClientBackups owner cid cid) $ \resp ->
    resp.status `shouldMatchInt` 204

testBackupDeleteRemovesObject :: (HasCallStack) => App ()
testBackupDeleteRemovesObject = do
  (owner, cid) <- setupBackupUser
  fullId <- uploadFullId owner cid 1
  req <- presignedFullRequest owner cid 1 fullId
  bindResponse (deleteClientBackups owner cid cid) $ \resp ->
    resp.status `shouldMatchInt` 204
  -- the object is gone from S3, not just unreferenced
  bindResponse (submit "GET" req) $ \resp ->
    resp.status `shouldMatchInt` 404

testBackupReplaceRemovesOldObject :: (HasCallStack) => App ()
testBackupReplaceRemovesOldObject = do
  (owner, cid) <- setupBackupUser
  oldId <- uploadFullId owner cid 1
  oldReq <- presignedFullRequest owner cid 1 oldId
  bindResponse (uploadBackupFull owner cid cid 1 (BC.replicate 16 'y')) $ \resp ->
    resp.status `shouldMatchInt` 200
  bindResponse (submit "GET" oldReq) $ \resp ->
    resp.status `shouldMatchInt` 404

testBackupDeleteForbiddenOtherClient :: (HasCallStack) => App ()
testBackupDeleteForbiddenOtherClient = do
  (owner, cid) <- setupBackupUser
  other <- newClientId owner
  _ <- uploadFullId owner cid 1
  bindResponse (deleteClientBackups owner cid other) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "backup-forbidden"
  bindResponse (getBackupEpoch owner cid cid 1) $ \resp ->
    resp.status `shouldMatchInt` 200

testBackupDeleteRemovedClient :: (HasCallStack) => App ()
testBackupDeleteRemovedClient = do
  (owner, _) <- setupBackupUser
  removed <- newClientId owner
  deleteClient owner removed >>= assertSuccess
  bindResponse (deleteClientBackups owner removed removed) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "backup-forbidden"

-- | Deletion (GDPR) does not depend on the feature flag.
testBackupDeleteWhenDisabled :: (HasCallStack) => App ()
testBackupDeleteWhenDisabled = do
  (owner, tid, _) <- createTeam OwnDomain 0
  Internal.setTeamFeatureLockStatus owner tid "automaticBackups" "unlocked"
  setStatus owner tid "enabled"
  cid <- newClientId owner
  _ <- uploadFullId owner cid 1
  setStatus owner tid "disabled"
  bindResponse (deleteClientBackups owner cid cid) $ \resp ->
    resp.status `shouldMatchInt` 204
  setStatus owner tid "enabled"
  bindResponse (getBackupEpoch owner cid cid 1) $ \resp ->
    resp.status `shouldMatchInt` 404
  where
    setStatus owner tid status =
      bindResponse (Internal.setTeamFeatureStatus owner tid "automaticBackups" status) $ \resp ->
        resp.status `shouldMatchInt` 200

testBackupDeleteWithoutTeam :: (HasCallStack) => App ()
testBackupDeleteWithoutTeam = do
  personal <- randomUser OwnDomain def
  cid <- newClientId personal
  bindResponse (deleteClientBackups personal cid cid) $ \resp ->
    resp.status `shouldMatchInt` 204

testBackupDeleteOtherClientsUntouched :: (HasCallStack) => App ()
testBackupDeleteOtherClientsUntouched = do
  (owner, cidA) <- setupBackupUser
  cidB <- newClientId owner
  _ <- uploadFullId owner cidA 1
  _ <- bindResponse (uploadBackupFull owner cidB cidB 1 (BC.replicate 16 'x')) $ \resp ->
    resp.status `shouldMatchInt` 200
  bindResponse (deleteClientBackups owner cidA cidA) $ \resp ->
    resp.status `shouldMatchInt` 204
  bindResponse (getBackupEpoch owner cidB cidB 1) $ \resp ->
    resp.status `shouldMatchInt` 200

-- CLEANUP

-- | The number of files one cleanup batch deleted.
cleanupBatch :: (HasCallStack, MakesValue domain) => domain -> App Int
cleanupBatch domain =
  bindResponse (postBackupsCleanup domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "deleted" & asInt

-- | Cleans up until nothing is left (a dynamic backend may be reused).
drainCleanup :: (HasCallStack, MakesValue domain) => domain -> App ()
drainCleanup domain = do
  n <- cleanupBatch domain
  when (n > 0) $ drainCleanup domain

testBackupCleanupExpiredReservation :: (HasCallStack) => App ()
testBackupCleanupExpiredReservation =
  withModifiedBackend
    def
      { cargoholdCfg =
          setField "backups.uploadTimeout" (1 :: Int)
            >=> setField "backups.orphanGracePeriod" (10 :: Int),
        -- effectively never: the test calls the cleanup itself
        backgroundWorkerCfg = setField "backupsCleanup.schedule" ("0 0 1 1 *" :: String)
      }
    $ \domain -> do
      drainCleanup domain
      threadDelay 11_000_000
      drainCleanup domain
      (owner, cid) <- setupBackupUserOn domain
      -- cargohold may close the connection early
      stalled <- try (uploadBackupFullStalled owner cid 1)
      case stalled of
        Left (_ :: SomeException) -> pure ()
        Right resp -> resp.status `shouldMatchOneOf` [400, 500 :: Int]
      -- still in flight as far as the cleanup can tell
      cleanupBatch domain `shouldMatchInt` 0
      threadDelay 11_000_000
      cleanupBatch domain `shouldMatchInt` 1
      cleanupBatch domain `shouldMatchInt` 0

testBackupCleanupEndpoint :: (HasCallStack) => App ()
testBackupCleanupEndpoint =
  bindResponse (postBackupsCleanup OwnDomain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    -- the batch may find leftovers of other tests, any count is fine
    void (resp.json %. "deleted" & asInt)
