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

-- | Runs the backup subsystem in the cargohold 'Handler', with blobs in S3.
module CargoHold.Backup
  ( BackupEffects,
    runBackupEffects,
  )
where

import CargoHold.API.Error (incompleteBody, serverError)
import CargoHold.App
import CargoHold.Options (Opts (..), Settings (..))
import qualified CargoHold.S3 as S3
import Control.Error (runExceptT, throwE)
import qualified Hasql.Pool as Hasql
import Hasql.Pool.Extended (Pool)
import Imports
import qualified Network.Wai.Utilities.Error as Wai
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import qualified Polysemy.TinyLog as P
import qualified System.Logger as Log
import Wire.API.Routes.Version (expandVersionExp)
import Wire.BackupBlobStore
import Wire.BackupStore (BackupStore)
import Wire.BackupStore.Postgres (interpretBackupStoreToPostgres)
import Wire.BackupSubsystem
import Wire.BackupSubsystem.Interpreter (interpretBackupSubsystem)
import Wire.BackupSubsystem.Rules (BackupLimits)
import Wire.BrigAPIAccess (BrigAPIAccess)
import Wire.BrigAPIAccess.Rpc (interpretBrigAccess)
import Wire.ClientSubsystem.Error (ClientError)
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess.Rpc (interpretGalleyAPIAccessToRpc)
import Wire.ParseException (ParseException)
import Wire.Rpc (Rpc, runRpcWithHttp)
import Wire.RpcException (RpcException)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.Sem.Random (Random)
import Wire.Sem.Random.IO (randomToIO)

type BackupEffects =
  '[ BackupSubsystem,
     BackupBlobStore,
     BackupStore,
     BrigAPIAccess,
     GalleyAPIAccess,
     Rpc,
     Random,
     Input BackupLimits,
     Input Pool,
     Error BackupError,
     Error BlobUploadError,
     Error Hasql.UsageError,
     Error ParseException,
     Error RpcException,
     Error ClientError,
     TinyLog,
     Error Wai.Error,
     Embed IO
   ]

runBackupEffects :: Sem BackupEffects a -> Handler a
runBackupEffects action = do
  env <- ask
  let opts = env.options
      disabled = foldMap expandVersionExp opts.settings.disabledAPIVersions
  result <-
    liftIO
      . runM
      . runError @Wai.Error
      . loggerToTinyLog env.appLogger
      . internalError @ClientError
      . internalError @RpcException
      . internalError @ParseException
      . internalError @Hasql.UsageError
      -- 'uploadBackupImpl' catches 'BlobLengthMismatch', this is a fallback.
      . mapError (\BlobLengthMismatch -> incompleteBody)
      . mapError backupErrorToWai
      . runInputConst env.hasqlPool
      . runInputConst opts.settings.backupLimits
      . randomToIO
      . runRpcWithHttp env.httpManager env.requestId
      . interpretGalleyAPIAccessToRpc disabled opts.galley
      . interpretBrigAccess opts.brig
      . interpretBackupStoreToPostgres
      . interpretBackupBlobStoreToS3 env
      . interpretBackupSubsystem
      $ action
  either throwE pure result

-- | Logs the error and responds with a generic 500.
internalError ::
  forall e r a.
  (Show e, Member TinyLog r, Member (Error Wai.Error) r) =>
  Sem (Error e ': r) a ->
  Sem r a
internalError =
  runError >=> either (\e -> P.err (Log.msg (show e)) >> throw serverError) pure

interpretBackupBlobStoreToS3 ::
  ( Member (Embed IO) r,
    Member (Error BlobUploadError) r,
    Member (Error Wai.Error) r
  ) =>
  Env ->
  InterpreterFor BackupBlobStore r
interpretBackupBlobStoreToS3 env = interpret $ \case
  PutBlob fid len src ->
    runHandlerSem (S3.uploadBackupBlob (S3.backupKey fid) len src) >>= fromEither
  SignedDownloadUrl fid -> runHandlerSem (S3.signedURL (S3.backupKey fid) Nothing)
  DeleteBlobs fids -> traverse_ (runHandlerSem . S3.deleteObject . S3.backupKey) fids
  where
    runHandlerSem :: (Member (Embed IO) r', Member (Error Wai.Error) r') => Handler x -> Sem r' x
    runHandlerSem h = embed (runAppT env (runExceptT h)) >>= fromEither
