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

module CargoHold.API.Backup (uploadFull, getEpochFiles, downloadFull, uploadIncremental, downloadIncremental, listBackups, getClientBackups, deleteClientBackups) where

import CargoHold.API.Error (incompleteBody, invalidLength, serverError)
import CargoHold.App
import CargoHold.Backup.ObjectStore (interpretBackupObjectStoreS3)
import CargoHold.Options (Opts (..), Settings (..))
import Control.Monad.Trans.Except (except)
import Data.Domain (Domain)
import Data.Id
import Data.Qualified
import qualified Hasql.Pool as Hasql
import qualified Hasql.Pool.Extended as HasqlPoolExt
import Imports hiding (log)
import qualified Network.Wai.Utilities.Error as Wai
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import qualified Polysemy.TinyLog as Log
import qualified System.Logger.Message as Log
import URI.ByteString (Absolute)
import Wire.API.Asset (AssetLocation (..))
import Wire.API.Backup
import Wire.API.Error
import Wire.API.Error.Cargohold
import Wire.API.Routes.Version (expandVersionExp)
import Wire.BackupObjectStore (BackupObjectStore)
import Wire.BackupStore (BackupStore)
import Wire.BackupStore.Postgres (interpretBackupStoreToPostgres)
import Wire.BackupSubsystem (BackupError (..), BackupSubsystem)
import qualified Wire.BackupSubsystem as Backup
import Wire.BackupSubsystem.Interpreter (interpretBackupSubsystem)
import Wire.BrigAPIAccess (BrigAPIAccess)
import Wire.BrigAPIAccess.Rpc (interpretBrigAccess)
import Wire.ClientSubsystem.Error (ClientError)
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess.Rpc (interpretGalleyAPIAccessToRpc)
import Wire.ParseException (ParseException)
import Wire.Rpc (Rpc, runRpcWithHttp)
import Wire.RpcException (RpcException)
import Wire.Sem.Logger.TinyLog (loggerToTinyLogReqId)
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO (nowToIO)
import Wire.Sem.Random (Random)
import Wire.Sem.Random.IO (randomToIO)

-- | Everything the backup handlers need, in the order of interpretation.
type BackupEffects =
  '[ BackupSubsystem,
     BackupStore,
     BackupObjectStore,
     BrigAPIAccess,
     GalleyAPIAccess,
     Rpc,
     Now,
     Random,
     Input Backup.BackupConfig,
     Input HasqlPoolExt.Pool,
     Error BackupError,
     Error Hasql.UsageError,
     Error RpcException,
     Error ParseException,
     Error ClientError,
     TinyLog,
     Error Wai.Error,
     Embed IO
   ]

uploadFull ::
  Local UserId ->
  ClientId ->
  ClientId ->
  BackupEpoch ->
  Word64 ->
  BackupSource ->
  Handler BackupFile
uploadFull lusr zClient cid epoch len body =
  runBackupEffects $ Backup.uploadFull lusr zClient cid epoch len body.getBackupSource

getEpochFiles :: Local UserId -> ClientId -> ClientId -> BackupEpoch -> Handler BackupFiles
getEpochFiles lusr zClient cid epoch =
  runBackupEffects $ Backup.getEpochFiles lusr zClient cid epoch

downloadFull ::
  Local UserId ->
  ClientId ->
  ClientId ->
  BackupEpoch ->
  BackupFileId ->
  Maybe Domain ->
  Handler (AssetLocation Absolute)
downloadFull lusr zClient cid epoch fid host =
  AssetLocation <$> runBackupEffects (Backup.downloadFull lusr zClient host cid epoch fid)

uploadIncremental ::
  Local UserId ->
  ClientId ->
  ClientId ->
  BackupEpoch ->
  BackupFileId ->
  Word64 ->
  BackupSource ->
  Handler BackupFile
uploadIncremental lusr zClient cid epoch full len body =
  runBackupEffects $
    Backup.uploadIncremental lusr zClient cid epoch full len body.getBackupSource

downloadIncremental ::
  Local UserId ->
  ClientId ->
  ClientId ->
  BackupEpoch ->
  BackupFileId ->
  BackupFileId ->
  Maybe Domain ->
  Handler (AssetLocation Absolute)
downloadIncremental lusr zClient cid epoch full fid host =
  AssetLocation
    <$> runBackupEffects (Backup.downloadIncremental lusr zClient host cid epoch full fid)

listBackups :: Local UserId -> ClientId -> Handler BackupList
listBackups lusr zClient = runBackupEffects $ Backup.listBackups lusr zClient

getClientBackups :: Local UserId -> ClientId -> ClientId -> Handler ClientBackups
getClientBackups lusr zClient cid = runBackupEffects $ Backup.getClientBackups lusr zClient cid

deleteClientBackups :: Local UserId -> ClientId -> ClientId -> Handler ()
deleteClientBackups lusr zClient cid = runBackupEffects $ Backup.deleteClientBackups lusr zClient cid

runBackupEffects :: Sem BackupEffects a -> Handler a
runBackupEffects action = do
  env <- ask
  let opts = env.options
  result <-
    liftIO
      . runM
      . runError @Wai.Error
      . loggerToTinyLogReqId env.requestId env.appLogger
      . logAndFail @ClientError
      . logAndFail @ParseException
      . logAndFail @RpcException
      . logAndFail @Hasql.UsageError
      . mapError backupErrorToWai
      . runInputConst env.hasqlPool
      . runInputConst opts.backups
      . randomToIO
      . nowToIO
      . runRpcWithHttp env.httpManager env.requestId
      . interpretGalleyAPIAccessToRpc (foldMap expandVersionExp opts.settings.disabledAPIVersions) opts.galley
      . interpretBrigAccess opts.brig
      . interpretBackupObjectStoreS3
        env.aws
        env.multiIngress
        opts.backups.uploadTimeout
        opts.settings.downloadLinkTTL
      . interpretBackupStoreToPostgres
      . interpretBackupSubsystem
      $ action
  except result

-- | Internal failures are logged and answered with a generic server error.
logAndFail ::
  forall e r a.
  (Show e, Member TinyLog r, Member (Error Wai.Error) r) =>
  Sem (Error e : r) a ->
  Sem r a
logAndFail =
  runError >=> \case
    Right a -> pure a
    Left e -> do
      Log.err $ Log.msg (show e)
      throw serverError

backupErrorToWai :: BackupError -> Wai.Error
backupErrorToWai = \case
  BackupErrorForbidden -> errorToWai @'BackupForbidden
  BackupErrorDisabled -> errorToWai @'BackupDisabled
  BackupErrorEpochInvalid -> errorToWai @'BackupEpochInvalid
  BackupErrorNotFound -> errorToWai @'BackupNotFound
  BackupErrorTooLarge -> errorToWai @'BackupTooLarge
  BackupErrorInvalidLength -> invalidLength
  BackupErrorIncompleteBody -> incompleteBody
  BackupErrorUploadFailed -> serverError
  BackupErrorFullMissing -> errorToWai @'BackupFullMissing
  BackupErrorEpochFull -> errorToWai @'BackupEpochFull
