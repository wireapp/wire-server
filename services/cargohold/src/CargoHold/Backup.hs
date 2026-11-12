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
import Wire.BackupBlobStore.Key
import Wire.BackupSubsystem
import Wire.BackupSubsystem.Disabled (interpretBackupSubsystemDisabled)
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
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO (nowToIO)
import Wire.Sem.Random (Random)
import Wire.Sem.Random.IO (randomToIO)

type BackupEffects =
  '[ BackupSubsystem,
     BrigAPIAccess,
     GalleyAPIAccess,
     Rpc,
     Random,
     Now,
     Input BackupLimits,
     Error BackupError,
     Error BlobUploadError,
     Error ParseException,
     Error RpcException,
     Error ClientError,
     TinyLog,
     Error Wai.Error,
     Embed IO
   ]

-- | Without @backupHmacKey@ the subsystem is disabled and S3 is never touched.
runBackupEffects :: Sem BackupEffects a -> Handler a
runBackupEffects action = do
  env <- ask
  let opts = env.options
      disabled = foldMap expandVersionExp opts.settings.disabledAPIVersions
      subsystem = case env.backupHmacKey of
        Just k -> interpretBackupBlobStoreToS3 env k . interpretBackupSubsystem . raiseUnder
        Nothing -> interpretBackupSubsystemDisabled
  result <-
    liftIO
      . runM
      . runError @Wai.Error
      . loggerToTinyLog env.appLogger
      . internalError @ClientError
      . internalError @RpcException
      . internalError @ParseException
      -- 'uploadBackupImpl' catches 'BlobLengthMismatch', this is a fallback.
      . mapError (\BlobLengthMismatch -> incompleteBody)
      . mapError backupErrorToWai
      . runInputConst opts.settings.backupLimits
      . nowToIO
      . randomToIO
      . runRpcWithHttp env.httpManager env.requestId
      . interpretGalleyAPIAccessToRpc disabled opts.galley
      . interpretBrigAccess opts.brig
      . subsystem
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
  BackupHmacKey ->
  InterpreterFor BackupBlobStore r
interpretBackupBlobStoreToS3 env k = interpret $ \case
  PutBlob l len src -> runHandlerSem (S3.uploadBackupBlob (objectKey l) len src) >>= fromEither
  BlobExists l -> runHandlerSem (S3.headObjectExists (objectKey l))
  SignedDownloadUrl l -> runHandlerSem (S3.signedBackupURL (objectKey l))
  ListBlobEpochs u c -> do
    let p = renderPrefix k (ClientPrefix u c)
    mapMaybe (parseEpochPrefix p) <$> runHandlerSem (S3.listCommonPrefixes p)
  ListBlobs u c e -> do
    let p = renderPrefix k (EpochPrefix u c e)
        blob (key, size) = (\(kind, fileId) -> BackupBlob {kind, fileId, size}) <$> parseFileKey p key
    mapMaybe blob <$> runHandlerSem (S3.listObjects p)
  DeleteBlobVersion l version -> runHandlerSem (S3.deleteObject (objectKey l) version)
  DeleteBlob l -> let key = renderKey k l in runHandlerSem (S3.deleteAllVersions key (== key))
  DeleteBlobs p -> runHandlerSem (S3.deleteAllVersions (renderPrefix k p) (const True))
  where
    objectKey = S3.S3AssetKey . renderKey k
    runHandlerSem :: (Member (Embed IO) r', Member (Error Wai.Error) r') => Handler x -> Sem r' x
    runHandlerSem h = embed (runAppT env (runExceptT h)) >>= fromEither
