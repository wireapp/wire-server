{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Adapter that lets Arbiter run against wire-server's shared Hasql pool.
--
-- The pool we want to reuse is 'HasqlPoolExt.Pool'. Internally that is backed
-- by @Data.Pool (Either ConnectionError Connection)@, but the underlying
-- resource pool is intentionally opaque in @hasql-resource-pool@. The only
-- missing piece is therefore a small exported helper from that package that
-- borrows one live 'Connection' for the duration of a callback.
module Wire.JobSubsystem.ArbiterAdapter where

import Arbiter.Core.Codec (Params, RowCodec)
import Arbiter.Core.Exceptions (throwInternal)
import Arbiter.Core.HasArbiterSchema (HasArbiterSchema (..))
import Arbiter.Core.MonadArbiter (MonadArbiter (..))
import Arbiter.Core.QueueRegistry (JobPayloadRegistry)
import Arbiter.Hasql.Decode qualified as Decode
import Arbiter.Hasql.Encode qualified as Encode
import Control.Exception (mask, onException, try)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Reader
import Data.Text qualified as T
import Hasql.Connection qualified as HasqlConn
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Pool qualified as HasqlPool
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Imports

data WireArbiterEnv = WireArbiterEnv
  { schemaName :: Text,
    connectionPool :: HasqlPoolExt.Pool,
    activeConn :: Maybe HasqlConn.Connection,
    transactionDepth :: Int,
    preparedStatements :: Bool
  }

newtype WireArbiter (registry :: JobPayloadRegistry) a = WireArbiter
  { unWireArbiter :: ReaderT WireArbiterEnv IO a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadCatch,
      MonadIO,
      MonadMask,
      MonadReader WireArbiterEnv,
      MonadThrow,
      MonadUnliftIO
    )

runWireArbiter :: WireArbiterEnv -> WireArbiter registry a -> IO a
runWireArbiter env (WireArbiter action) = runReaderT action env

instance HasArbiterSchema (WireArbiter registry) registry where
  getSchema = asks schemaName

instance MonadArbiter (WireArbiter registry) where
  type Handler (WireArbiter registry) jobs result = HasqlConn.Connection -> jobs -> WireArbiter registry result

  executeQuery sql params codec = do
    env <- ask
    withConn env $ \conn ->
      runQueryStatement env.preparedStatements conn sql params codec

  executeStatement sql params = do
    env <- ask
    withConn env $ \conn ->
      runExecStatement conn sql params

  withDbTransaction (WireArbiter action) = WireArbiter $ do
    env <- ask
    case activeConn env of
      Nothing ->
        unWireArbiter $
          withPoolConnection env.connectionPool $ \conn ->
            beginCommitOrRollback conn $
              runReaderT action env {activeConn = Just conn, transactionDepth = 1}
      Just conn
        | transactionDepth env <= 0 ->
            liftIO $
              beginCommitOrRollback conn $
                runReaderT action env {transactionDepth = 1}
        | otherwise ->
            liftIO $
              beginSavepointTransaction (transactionDepth env) conn $
                runReaderT action env {transactionDepth = transactionDepth env + 1}

  runHandlerWithConnection handler jobs = do
    env <- ask
    case activeConn env of
      Just conn -> handler conn jobs
      Nothing -> throwInternal "runHandlerWithConnection: no active connection"

withConn :: WireArbiterEnv -> (HasqlConn.Connection -> IO a) -> WireArbiter registry a
withConn env f =
  case activeConn env of
    Just conn -> liftIO $ f conn
    Nothing -> withPoolConnection env.connectionPool f

-- | Borrow a live connection from wire-server's shared pool.
withPoolConnection :: HasqlPoolExt.Pool -> (HasqlConn.Connection -> IO a) -> WireArbiter registry a
withPoolConnection pool f = do
  result <-
    liftIO $
      HasqlPool.withConnectionWithPoolAcquisitionTimeout
        pool.poolAcquisitionTimeout
        pool.rawPool
        (fmap Right . f)
  case result of
    Right x -> pure x
    Left HasqlPool.AcquisitionTimeoutUsageError -> do
      liftIO $ HasqlPoolExt.recordHasqlPoolAcquisitionTimeout pool.metrics
      throwInternal "hasql pool acquisition timeout"
    Left (HasqlPool.ConnectionError err) -> do
      liftIO $ HasqlPoolExt.recordHasqlPoolConnectionFailure pool.metrics
      throwInternal $ "hasql connection error: " <> T.pack (show err)
    Left (HasqlPool.SessionError err) -> do
      liftIO $ HasqlPoolExt.recordHasqlPoolSessionFailure pool.metrics
      throwInternal $ "hasql session error: " <> T.pack (show err)

runQueryStatement :: Bool -> HasqlConn.Connection -> Text -> Params -> RowCodec a -> IO [a]
runQueryStatement prepare conn sql params codec = do
  let mk = if prepare then Statement.preparable else Statement.unpreparable
      stmt =
        mk
          (Encode.convertPlaceholders sql)
          (Encode.buildEncoder params)
          (Decode.hasqlRowDecoder codec)
  result <- HasqlConn.use conn (Session.statement () stmt)
  case result of
    Right rows -> pure rows
    Left err -> fail $ "hasql query error: " <> show err

runExecStatement :: HasqlConn.Connection -> Text -> Params -> IO Int64
runExecStatement conn sql params = do
  let stmt = Encode.buildStatementRowCount sql params
  result <- HasqlConn.use conn (Session.statement () stmt)
  case result of
    Right n -> pure n
    Left err -> fail $ "hasql statement error: " <> show err

runRawSql :: HasqlConn.Connection -> Text -> IO ()
runRawSql conn sql = do
  let stmt = Statement.unpreparable sql Encoders.noParams Decoders.noResult
  result <- HasqlConn.use conn (Session.statement () stmt)
  case result of
    Right () -> pure ()
    Left err -> fail $ "hasql sql error: " <> show err

beginCommitOrRollback :: HasqlConn.Connection -> IO a -> IO a
beginCommitOrRollback conn action = mask $ \restore -> do
  runRawSql conn "BEGIN"
  result <- restore action `onException` rollbackSafely
  runRawSql conn "COMMIT"
  pure result
  where
    rollbackSafely = do
      _ <- try (runRawSql conn "ROLLBACK") :: IO (Either SomeException ())
      pure ()

beginSavepointTransaction :: Int -> HasqlConn.Connection -> IO a -> IO a
beginSavepointTransaction depth conn action = mask $ \restore -> do
  let spName = "arbiter_sp_" <> T.pack (show depth)
  runRawSql conn ("SAVEPOINT " <> spName)
  result <-
    restore action
      `onException` do
        _ <- try (runRawSql conn ("ROLLBACK TO SAVEPOINT " <> spName)) :: IO (Either SomeException ())
        pure ()
  runRawSql conn ("RELEASE SAVEPOINT " <> spName)
  pure result
