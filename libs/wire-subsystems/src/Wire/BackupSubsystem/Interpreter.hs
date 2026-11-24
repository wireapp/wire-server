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

module Wire.BackupSubsystem.Interpreter (interpretBackupSubsystem) where

import Conduit (ConduitT, ResourceT)
import Data.Id
import Data.Map qualified as Map
import Data.Qualified
import Data.Time.Clock (addUTCTime)
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Backup
import Wire.API.Team.Feature
import Wire.API.User.Client (UserClients (..))
import Wire.BackupObjectStore
import Wire.BackupStore
import Wire.BackupSubsystem
import Wire.BrigAPIAccess (BrigAPIAccess, lookupClients)
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess qualified as Galley
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.Sem.Random (Random)
import Wire.Sem.Random qualified as Random

interpretBackupSubsystem ::
  ( Member BackupStore r,
    Member BackupObjectStore r,
    Member BrigAPIAccess r,
    Member GalleyAPIAccess r,
    Member Now r,
    Member Random r,
    Member (Input BackupConfig) r,
    Member (Error BackupError) r
  ) =>
  InterpreterFor BackupSubsystem r
interpretBackupSubsystem = interpret $ \case
  UploadFull caller zClient cid epoch len body ->
    uploadFullImpl (tUnqualified caller) zClient cid epoch len body
  GetEpochFiles caller zClient cid epoch -> do
    let uid = tUnqualified caller
    ensureClientExists uid zClient
    ensureEnabled uid
    lookupEpochFiles uid cid epoch >>= maybe (throw BackupErrorNotFound) pure

uploadFullImpl ::
  ( Member BackupStore r,
    Member BackupObjectStore r,
    Member BrigAPIAccess r,
    Member GalleyAPIAccess r,
    Member Now r,
    Member Random r,
    Member (Input BackupConfig) r,
    Member (Error BackupError) r
  ) =>
  UserId ->
  ClientId ->
  ClientId ->
  BackupEpoch ->
  Word64 ->
  ConduitT () ByteString (ResourceT IO) () ->
  Sem r BackupFile
uploadFullImpl uid zClient cid epoch len body = do
  config <- input
  when (zClient /= cid) $ throw BackupErrorForbidden
  ensureClientExists uid zClient
  ensureEnabled uid
  when (len == 0) $ throw BackupErrorInvalidLength
  when (len > config.maxFullBytes) $ throw BackupErrorTooLarge
  -- cheap, non-authoritative pre-check; 'commitFull' checks again under the lock
  maxEpoch <- lookupMaxEpoch uid cid
  unless (epochAccepted maxEpoch epoch) $ throw BackupErrorEpochInvalid
  start <- Now.get
  fid <- BackupFileId <$> Random.uuid
  reserveUpload uid cid epoch fid
  putObject fid len body >>= either (throw . putError) pure
  end <- Now.get
  -- the reservation may have been cleaned up after the grace period
  when (end > addUTCTime config.uploadTimeout start) $ throw BackupErrorUploadFailed
  commitFull uid cid epoch fid len >>= \case
    Left CommitEpochInvalid -> throw BackupErrorEpochInvalid
    Left CommitReservationGone -> throw BackupErrorUploadFailed
    Right file -> pure file
  where
    putError PutIncompleteBody = BackupErrorIncompleteBody
    putError PutTimedOut = BackupErrorUploadFailed
    putError PutFailed = BackupErrorUploadFailed

-- | The calling client must still exist.
ensureClientExists ::
  (Member BrigAPIAccess r, Member (Error BackupError) r) =>
  UserId ->
  ClientId ->
  Sem r ()
ensureClientExists uid zClient = do
  UserClients clients <- lookupClients [uid]
  unless (zClient `elem` Map.findWithDefault mempty uid clients) $ throw BackupErrorForbidden

-- | The user must be in a team with the feature @automaticBackups@ enabled.
ensureEnabled ::
  (Member GalleyAPIAccess r, Member (Error BackupError) r) =>
  UserId ->
  Sem r ()
ensureEnabled uid = do
  tid <- Galley.getTeamId uid >>= maybe (throw BackupErrorDisabled) pure
  feature <- Galley.getFeatureConfigForTeam @_ @AutomaticBackupsConfig tid
  unless (feature.status == FeatureStatusEnabled) $ throw BackupErrorDisabled
