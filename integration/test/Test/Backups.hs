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

-- | A team owner with the feature @automaticBackups@ enabled and a client.
setupBackupUser :: (HasCallStack) => App (Value, String)
setupBackupUser = do
  (owner, tid, _) <- createTeam OwnDomain 0
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
