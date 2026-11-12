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

module Wire.BackupStore.Postgres (interpretBackupStoreToPostgres) where

import Data.Id
import Data.Json.Util (toUTCTimeMillis)
import Data.Map qualified as Map
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions
import Imports
import Polysemy
import Wire.API.Backup
import Wire.BackupStore
import Wire.BackupSubsystem.Rules
import Wire.Postgres

interpretBackupStoreToPostgres :: (PGConstraints r) => InterpreterFor BackupStore r
interpretBackupStoreToPostgres = interpret $ \case
  InsertBackupFile limits new -> insertBackupFileImpl limits new
  GetDeviceEpochCounts uid cid ->
    toCounts <$> runStatement (toUUID uid, clientToText cid) epochCountsStatement
  ListBackupFiles uid cid e ->
    map toBackupFile . V.toList
      <$> runStatement (toUUID uid, clientToText cid, epochToDb e) listStatement
  LookupBackupFile uid cid e fid ->
    fmap toBackupFile
      <$> runStatement (toUUID uid, clientToText cid, epochToDb e, fid.unBackupFileId) lookupStatement
  DeleteBackupFileRow fid -> runStatement fid.unBackupFileId deleteFileStatement
  DeleteEpochRows uid cid e ->
    toIds <$> runStatement (toUUID uid, clientToText cid, epochToDb e) deleteEpochStatement
  DeleteClientRows uid cid ->
    toIds <$> runStatement (toUUID uid, clientToText cid) deleteClientStatement
  DeleteUserRows uid -> toIds <$> runStatement (toUUID uid) deleteUserStatement

-- | The per-device advisory lock serialises concurrent uploads; the rules are checked against
-- counts read in the same transaction as the insert.
insertBackupFileImpl ::
  (PGConstraints r) =>
  BackupLimits ->
  NewBackupFile ->
  Sem r (Either UploadRejection BackupFile)
insertBackupFileImpl limits new =
  runTransaction Serializable Write $ do
    Transaction.statement (uid, cid) lockStatement
    counts <- toCounts <$> Transaction.statement (uid, cid) epochCountsStatement
    for (checkUpload limits counts new.epoch new.size) $ \kind -> do
      createdAt <-
        Transaction.statement
          (new.id.unBackupFileId, uid, cid, epochToDb new.epoch, kindToDb kind, fromIntegral new.size)
          insertStatement
      pure BackupFile {id = new.id, kind, size = new.size, createdAt = toUTCTimeMillis createdAt}
  where
    uid = toUUID new.user
    cid = clientToText new.client

    lockStatement :: Hasql.Statement (UUID, Text) ()
    lockStatement =
      [resultlessStatement|SELECT 1 :: int4 FROM pg_advisory_xact_lock(hashtextextended(($1 :: uuid) :: text || ($2 :: text), 0))|]

    insertStatement :: Hasql.Statement (UUID, UUID, Text, Int64, Int16, Int64) UTCTime
    insertStatement =
      [singletonStatement|INSERT INTO backup_files (id, user_id, client_id, epoch, kind, size)
                          VALUES ($1 :: uuid, $2 :: uuid, $3 :: text, $4 :: bigint, $5 :: smallint, $6 :: bigint)
                          RETURNING created_at :: timestamptz
                         |]

type FileRow = (UUID, Int16, Int64, UTCTime)

toBackupFile :: FileRow -> BackupFile
toBackupFile (fid, kind, size, createdAt) =
  BackupFile
    { id = BackupFileId fid,
      kind = if kind == 0 then BackupFull else BackupIncremental,
      size = fromIntegral size,
      createdAt = toUTCTimeMillis createdAt
    }

kindToDb :: BackupKind -> Int16
kindToDb BackupFull = 0
kindToDb BackupIncremental = 1

epochToDb :: BackupEpoch -> Int64
epochToDb = fromIntegral . (.unBackupEpoch)

toCounts :: Vector (Int64, Int64) -> Map BackupEpoch Word32
toCounts = Map.fromList . map (bimap (BackupEpoch . fromIntegral) fromIntegral) . V.toList

toIds :: Vector UUID -> [BackupFileId]
toIds = map BackupFileId . V.toList

epochCountsStatement :: Hasql.Statement (UUID, Text) (Vector (Int64, Int64))
epochCountsStatement =
  [vectorStatement|SELECT epoch :: bigint, count(*) :: bigint FROM backup_files
                   WHERE user_id = $1 :: uuid AND client_id = $2 :: text
                   GROUP BY epoch
                  |]

listStatement :: Hasql.Statement (UUID, Text, Int64) (Vector FileRow)
listStatement =
  [vectorStatement|SELECT id :: uuid, kind :: smallint, size :: bigint, created_at :: timestamptz
                   FROM backup_files
                   WHERE user_id = $1 :: uuid AND client_id = $2 :: text AND epoch = $3 :: bigint
                   ORDER BY created_at, id
                  |]

lookupStatement :: Hasql.Statement (UUID, Text, Int64, UUID) (Maybe FileRow)
lookupStatement =
  [maybeStatement|SELECT id :: uuid, kind :: smallint, size :: bigint, created_at :: timestamptz
                  FROM backup_files
                  WHERE user_id = $1 :: uuid AND client_id = $2 :: text
                    AND epoch = $3 :: bigint AND id = $4 :: uuid
                 |]

deleteFileStatement :: Hasql.Statement UUID ()
deleteFileStatement =
  [resultlessStatement|DELETE FROM backup_files WHERE id = $1 :: uuid|]

deleteEpochStatement :: Hasql.Statement (UUID, Text, Int64) (Vector UUID)
deleteEpochStatement =
  [vectorStatement|DELETE FROM backup_files
                   WHERE user_id = $1 :: uuid AND client_id = $2 :: text AND epoch = $3 :: bigint
                   RETURNING id :: uuid
                  |]

deleteClientStatement :: Hasql.Statement (UUID, Text) (Vector UUID)
deleteClientStatement =
  [vectorStatement|DELETE FROM backup_files
                   WHERE user_id = $1 :: uuid AND client_id = $2 :: text
                   RETURNING id :: uuid
                  |]

deleteUserStatement :: Hasql.Statement UUID (Vector UUID)
deleteUserStatement =
  [vectorStatement|DELETE FROM backup_files WHERE user_id = $1 :: uuid RETURNING id :: uuid|]
