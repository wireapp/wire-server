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

module Wire.BackupStore.Postgres (interpretBackupStoreToPostgres) where

import Data.Id
import Data.Json.Util (toUTCTimeMillis)
import Data.Map qualified as Map
import Data.Profunctor (lmap)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Hasql.Statement (Statement)
import Hasql.TH
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (..))
import Imports
import Polysemy
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import System.Logger.Message qualified as Log
import Wire.API.Backup
import Wire.API.PostgresMarshall (postgresUnmarshall)
import Wire.BackupStore
import Wire.Postgres

interpretBackupStoreToPostgres ::
  (PGConstraints r, Member TinyLog r) =>
  InterpreterFor BackupStore r
interpretBackupStoreToPostgres = interpret $ \case
  ReserveUpload uid cid epoch fid ->
    runStatement (toUUID uid, clientToText cid, epochToInt epoch, fileIdToUUID fid) insertReservation
  CommitFull uid cid epoch fid size ->
    runTransaction ReadCommitted Write (commitFullTx uid cid epoch fid size)
  LookupMaxEpoch uid cid ->
    fmap intToEpoch <$> runStatement (toUUID uid, clientToText cid) selectMaxEpoch
  CommitIncremental maxFiles uid cid epoch full fid size ->
    runTransaction ReadCommitted Write (commitIncrementalTx maxFiles uid cid epoch full fid size)
  LookupEpochFiles uid cid epoch ->
    -- one snapshot, so that the full and its incrementals belong together
    runTransaction RepeatableRead Read $ do
      let key = (toUUID uid, clientToText cid, epochToInt epoch)
      row <- Tx.statement key selectEpochFull
      for row $ \(i, s, c) -> do
        incs <- Tx.statement i selectIncrementals
        pure
          BackupFiles
            { full = mkFile i s c,
              incrementals = [mkFile iid isize icreated | (iid, isize, icreated) <- toList incs]
            }
  LookupFull uid cid epoch fid -> do
    row <- runStatement (toUUID uid, clientToText cid, epochToInt epoch) selectEpochFull
    pure $ fmap (\(i, _, _) -> i) row == Just (fileIdToUUID fid)
  LookupIncremental uid cid epoch full fid ->
    runStatement
      (toUUID uid, clientToText cid, epochToInt epoch, fileIdToUUID full, fileIdToUUID fid)
      existsIncremental
  CountEpochFiles uid cid epoch ->
    fmap fromIntegral <$> runStatement (toUUID uid, clientToText cid, epochToInt epoch) selectFileCount
  ListClientBackups uid mcid -> do
    rows <- runStatement (toUUID uid, clientToText <$> mcid) selectBackupSummaries
    groupSummaries . catMaybes <$> for (toList rows) parseRow

-- | Client ids are written from 'ClientId's, so a row that does not parse is a bug: log and
-- skip it.
parseRow ::
  (Member TinyLog r) =>
  (Text, Int64, UTCTime, Int64, Int64) ->
  Sem r (Maybe (ClientId, EpochSummary))
parseRow (client, epochNo, created, count, size) =
  case postgresUnmarshall client of
    Left err -> do
      Log.err $ Log.msg (Log.val "Invalid client id in backup_epochs") . Log.field "error" err
      pure Nothing
    Right cid ->
      pure . Just $
        ( cid,
          EpochSummary
            { epoch = intToEpoch epochNo,
              createdAt = toUTCTimeMillis created,
              fileCount = fromIntegral count,
              size = fromIntegral size
            }
        )

-- | Rows are ordered by epoch, which the grouping keeps.
groupSummaries :: [(ClientId, EpochSummary)] -> [ClientBackups]
groupSummaries rows =
  [ ClientBackups {client = cid, epochs = es}
  | (cid, es) <- Map.toAscList (Map.fromListWith (flip (<>)) [(cid, [e]) | (cid, e) <- rows])
  ]

commitFullTx ::
  UserId ->
  ClientId ->
  BackupEpoch ->
  BackupFileId ->
  Word64 ->
  Tx.Transaction (Either CommitFullError BackupFile)
commitFullTx uid cid epoch fid size = do
  let user = toUUID uid
      client = clientToText cid
      epochNo = epochToInt epoch
      newId = fileIdToUUID fid
  -- exactly one lock per transaction, serializing all writers of this client
  void $ Tx.statement (user, client) lockClient
  maxEpoch <- Tx.statement (user, client) selectMaxEpoch
  if not (epochAccepted (intToEpoch <$> maxEpoch) epoch)
    then abort CommitEpochInvalid
    else do
      claimed <- Tx.statement (newId, user, client) deleteReservation
      if claimed /= 1
        then abort CommitReservationGone
        else do
          existing <- Tx.statement (user, client, epochNo) selectEpochFull
          (i, s, c) <- case existing of
            Just (oldId, _, oldCreatedAt) -> do
              -- the incrementals reference the old full: move them away before updating it
              Tx.statement (user, client, epochNo, oldId) orphanIncrementalsOfFull
              Tx.statement (oldId, user, client, epochNo, oldCreatedAt) insertDeletion
              Tx.statement (user, client, epochNo, newId, fromIntegral size) updateEpochFull
            Nothing ->
              Tx.statement (user, client, epochNo, newId, fromIntegral size) insertEpoch
          when (epoch >= BackupEpoch 3) $ do
            let newestStale = epochToInt (staleEpochs epoch)
            -- the incrementals reference the stale fulls: move them away before deleting those
            Tx.statement (user, client, newestStale) orphanStaleIncrementals
            Tx.statement (user, client, newestStale) orphanStaleEpochs
          pure (Right (mkFile i s c))
  where
    abort err = Tx.condemn $> Left err

commitIncrementalTx ::
  Word32 ->
  UserId ->
  ClientId ->
  BackupEpoch ->
  BackupFileId ->
  BackupFileId ->
  Word64 ->
  Tx.Transaction (Either CommitIncrementalError BackupFile)
commitIncrementalTx maxFiles uid cid epoch full fid size = do
  let user = toUUID uid
      client = clientToText cid
      fullId = fileIdToUUID full
      newId = fileIdToUUID fid
  -- exactly one lock per transaction, serializing all writers of this client
  void $ Tx.statement (user, client) lockClient
  current <- Tx.statement (user, client, epochToInt epoch) selectEpochFull
  case current of
    Just (currentId, _, _) | currentId == fullId -> do
      incrementals <- Tx.statement fullId countIncrementals
      -- the full counts as a file of the epoch
      if 1 + incrementals >= fromIntegral maxFiles
        then abort CommitEpochFull
        else do
          claimed <- Tx.statement (newId, user, client) deleteReservation
          if claimed /= 1
            then abort CommitIncrementalReservationGone
            else do
              (i, s, c) <- Tx.statement (newId, fullId, fromIntegral size) insertIncremental
              pure (Right (mkFile i s c))
    _ -> abort CommitFullMissing
  where
    abort err = Tx.condemn $> Left err

mkFile :: UUID -> Int64 -> UTCTime -> BackupFile
mkFile fid size createdAt =
  BackupFile
    { id = BackupFileId fid,
      size = fromIntegral size,
      createdAt = toUTCTimeMillis createdAt
    }

epochToInt :: BackupEpoch -> Int64
epochToInt (BackupEpoch w) = fromIntegral w

intToEpoch :: Int64 -> BackupEpoch
intToEpoch = BackupEpoch . fromIntegral

fileIdToUUID :: BackupFileId -> UUID
fileIdToUUID (BackupFileId u) = u

-- | (user, client)
lockClient :: Statement (UUID, Text) Int32
lockClient =
  [singletonStatement|
    SELECT (1 :: int4)
    FROM (SELECT pg_advisory_xact_lock(hashtextextended(CAST($1 :: uuid AS text) || ':' || $2 :: text, 0))) AS t
  |]

-- | (user, client)
selectMaxEpoch :: Statement (UUID, Text) (Maybe Int64)
selectMaxEpoch =
  [singletonStatement|
    SELECT max(epoch_no) :: int8?
    FROM backup_epochs
    WHERE user_id = $1 :: uuid AND client_id = $2 :: text
  |]

-- | (user, client, epoch)
selectEpochFull :: Statement (UUID, Text, Int64) (Maybe (UUID, Int64, UTCTime))
selectEpochFull =
  [maybeStatement|
    SELECT full_id :: uuid, full_size :: int8, full_created_at :: timestamptz
    FROM backup_epochs
    WHERE user_id = $1 :: uuid AND client_id = $2 :: text AND epoch_no = $3 :: int8
  |]

-- | (file, user, client, epoch)
insertReservation :: Statement (UUID, Text, Int64, UUID) ()
insertReservation =
  lmap
    (\(user, client, epochNo, fileId) -> (fileId, user, client, epochNo))
    [resultlessStatement|
      INSERT INTO backup_orphans (id, user_id, client_id, epoch_no, file_created_at)
      VALUES ($1 :: uuid, $2 :: uuid, $3 :: text, $4 :: int8, NULL)
    |]

-- | Removes the upload reservation of a file: (file, user, client). Returns the number of
-- removed rows.
deleteReservation :: Statement (UUID, UUID, Text) Int64
deleteReservation =
  [rowsAffectedStatement|
    DELETE FROM backup_orphans
    WHERE id = $1 :: uuid AND user_id = $2 :: uuid AND client_id = $3 :: text
      AND file_created_at IS NULL
  |]

-- | A deletion marker for a file: (file, user, client, epoch, file creation time)
insertDeletion :: Statement (UUID, UUID, Text, Int64, UTCTime) ()
insertDeletion =
  [resultlessStatement|
    INSERT INTO backup_orphans (id, user_id, client_id, epoch_no, file_created_at)
    VALUES ($1 :: uuid, $2 :: uuid, $3 :: text, $4 :: int8, $5 :: timestamptz)
  |]

-- | (user, client, epoch, new file, size)
insertEpoch :: Statement (UUID, Text, Int64, UUID, Int64) (UUID, Int64, UTCTime)
insertEpoch =
  [singletonStatement|
    INSERT INTO backup_epochs (user_id, client_id, epoch_no, created_at, full_id, full_size, full_created_at)
    VALUES ($1 :: uuid, $2 :: text, $3 :: int8, now(), $4 :: uuid, $5 :: int8, now())
    RETURNING full_id :: uuid, full_size :: int8, full_created_at :: timestamptz
  |]

-- | (user, client, epoch, new file, size)
updateEpochFull :: Statement (UUID, Text, Int64, UUID, Int64) (UUID, Int64, UTCTime)
updateEpochFull =
  [singletonStatement|
    UPDATE backup_epochs
    SET full_id = $4 :: uuid, full_size = $5 :: int8, full_created_at = now()
    WHERE user_id = $1 :: uuid AND client_id = $2 :: text AND epoch_no = $3 :: int8
    RETURNING full_id :: uuid, full_size :: int8, full_created_at :: timestamptz
  |]

-- | Removes the epochs up to and including the given one and orphans their fulls:
-- (user, client, newest stale epoch)
orphanStaleEpochs :: Statement (UUID, Text, Int64) ()
orphanStaleEpochs =
  [resultlessStatement|
    WITH stale AS (
      DELETE FROM backup_epochs
      WHERE user_id = $1 :: uuid AND client_id = $2 :: text AND epoch_no <= $3 :: int8
      RETURNING full_id, epoch_no, full_created_at
    )
    INSERT INTO backup_orphans (id, user_id, client_id, epoch_no, file_created_at)
    SELECT full_id, $1 :: uuid, $2 :: text, epoch_no, full_created_at FROM stale
  |]

-- | The incrementals of a full, oldest first: (full)
selectIncrementals :: Statement UUID (Vector (UUID, Int64, UTCTime))
selectIncrementals =
  [vectorStatement|
    SELECT id :: uuid, size :: int8, created_at :: timestamptz
    FROM backup_incrementals
    WHERE full_id = $1 :: uuid
    ORDER BY created_at, id
  |]

-- | (user, client, epoch)
selectFileCount :: Statement (UUID, Text, Int64) (Maybe Int64)
selectFileCount =
  [maybeStatement|
    SELECT (1 + (SELECT count(*) FROM backup_incrementals i WHERE i.full_id = e.full_id)) :: int8
    FROM backup_epochs e
    WHERE e.user_id = $1 :: uuid AND e.client_id = $2 :: text AND e.epoch_no = $3 :: int8
  |]

-- | (full)
countIncrementals :: Statement UUID Int64
countIncrementals =
  [singletonStatement|
    SELECT count(*) :: int8 FROM backup_incrementals WHERE full_id = $1 :: uuid
  |]

-- | (user, client, epoch, full, incremental)
existsIncremental :: Statement (UUID, Text, Int64, UUID, UUID) Bool
existsIncremental =
  [singletonStatement|
    SELECT EXISTS (
      SELECT 1
      FROM backup_incrementals i
      JOIN backup_epochs e ON e.full_id = i.full_id
      WHERE e.user_id = $1 :: uuid AND e.client_id = $2 :: text AND e.epoch_no = $3 :: int8
        AND e.full_id = $4 :: uuid AND i.id = $5 :: uuid
    ) :: bool
  |]

-- | The creation time is the time of the commit, not of the transaction start, so that the
-- order of commits (serialized by the lock) is the order of the incrementals.
-- (new file, full, size)
insertIncremental :: Statement (UUID, UUID, Int64) (UUID, Int64, UTCTime)
insertIncremental =
  [singletonStatement|
    INSERT INTO backup_incrementals (id, full_id, size, created_at)
    VALUES ($1 :: uuid, $2 :: uuid, $3 :: int8, clock_timestamp())
    RETURNING id :: uuid, size :: int8, created_at :: timestamptz
  |]

-- | Orphans the incrementals of a full: (user, client, epoch, full)
orphanIncrementalsOfFull :: Statement (UUID, Text, Int64, UUID) ()
orphanIncrementalsOfFull =
  [resultlessStatement|
    WITH moved AS (
      DELETE FROM backup_incrementals
      WHERE full_id = $4 :: uuid
      RETURNING id, created_at
    )
    INSERT INTO backup_orphans (id, user_id, client_id, epoch_no, file_created_at)
    SELECT id, $1 :: uuid, $2 :: text, $3 :: int8, created_at FROM moved
  |]

-- | Orphans the incrementals of the fulls of the epochs up to and including the given one:
-- (user, client, newest stale epoch)
orphanStaleIncrementals :: Statement (UUID, Text, Int64) ()
orphanStaleIncrementals =
  [resultlessStatement|
    WITH moved AS (
      DELETE FROM backup_incrementals i
      USING backup_epochs e
      WHERE i.full_id = e.full_id
        AND e.user_id = $1 :: uuid AND e.client_id = $2 :: text AND e.epoch_no <= $3 :: int8
      RETURNING i.id, i.created_at, e.epoch_no
    )
    INSERT INTO backup_orphans (id, user_id, client_id, epoch_no, file_created_at)
    SELECT id, $1 :: uuid, $2 :: text, epoch_no, created_at FROM moved
  |]

-- | One row per epoch of the user (of the client, if given); an epoch without incrementals
-- counts only its full. @SUM@ of @int8@ is @numeric@, hence the casts. (user, client)
selectBackupSummaries :: Statement (UUID, Maybe Text) (Vector (Text, Int64, UTCTime, Int64, Int64))
selectBackupSummaries =
  [vectorStatement|
    SELECT
      e.client_id :: text,
      e.epoch_no :: int8,
      GREATEST(e.full_created_at, max(i.created_at)) :: timestamptz,
      (1 + count(i.id)) :: int8,
      (e.full_size + coalesce(sum(i.size), 0)) :: int8
    FROM backup_epochs e
    LEFT JOIN backup_incrementals i ON i.full_id = e.full_id
    WHERE e.user_id = $1 :: uuid AND e.client_id = coalesce($2 :: text?, e.client_id)
    GROUP BY e.client_id, e.epoch_no, e.full_created_at, e.full_size
    ORDER BY e.client_id, e.epoch_no
  |]
