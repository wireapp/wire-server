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
import Arbiter.Core.MonadArbiter (MonadArbiter (..), Query (..))
import Arbiter.Core.QueueRegistry (JobPayloadRegistry)
import Arbiter.Core.Sql.Query (numberPlaceholders)
import Arbiter.Hasql.Decode qualified as Decode
import Arbiter.Hasql.Encode qualified as Encode
import Control.Exception (mask, onException, try)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Reader
import Data.Misc (durationToCeilingSeconds)
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
    transactionDepth :: Int
  }

mkNewWireArbiterEnv :: Text -> HasqlPoolExt.Pool -> WireArbiterEnv
mkNewWireArbiterEnv schema pool =
  WireArbiterEnv
    { schemaName = schema,
      connectionPool = pool,
      activeConn = Nothing,
      transactionDepth = 0
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

instance MonadArbiter (WireArbiter registry) where
  type RegistryOf (WireArbiter registry) = registry
  type Handler (WireArbiter registry) jobs result = HasqlConn.Connection -> jobs -> WireArbiter registry result
  getSchema = asks schemaName

  executeQuery (Query sql params codec) = do
    env <- ask
    withConn env $ \conn ->
      runQueryStatement False conn sql params codec

  executeQueryPrepared (Query sql params codec) = do
    env <- ask
    withConn env $ \conn ->
      runQueryStatement True conn sql params codec

  executeStatement (Query sql params _) = do
    env <- ask
    withConn env $ \conn ->
      runExecStatement conn sql params

  withDbTransaction action = do
    env <- ask
    case activeConn env of
      Nothing -> withRunInIO $ \run ->
        run $ withPoolConnection env.connectionPool $ \conn -> run (beginCommitOrRollback conn action)
      Just conn
        | transactionDepth env <= 0 -> beginCommitOrRollback conn action
        | otherwise -> beginSavepointTransaction conn action

  runHandlerWithConnection handler jobs = do
    env <- ask
    case activeConn env of
      Just conn -> handler conn jobs
      Nothing -> throwInternal "runHandlerWithConnection: no active connection"

  -- Wire's shared pool is already used for all Arbiter database work. Keep
  -- workers poll-only so they do not pin an additional PostgreSQL connection
  -- for LISTEN/NOTIFY.
  getListener = pure Nothing

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
        (durationToCeilingSeconds pool.poolAcquisitionTimeout)
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
          (numberPlaceholders sql)
          (Encode.buildEncoder params)
          (Decode.hasqlRowDecoder codec)
  result <- HasqlConn.use conn (Session.statement () stmt)
  case result of
    Right rows -> pure rows
    Left err -> throwInternal $ "hasql query error: " <> T.pack (show err)

runExecStatement :: HasqlConn.Connection -> Text -> Params -> IO Int64
runExecStatement conn sql params = do
  let stmt = Encode.buildStatementRowCount sql params
  result <- HasqlConn.use conn (Session.statement () stmt)
  case result of
    Right n -> pure n
    Left err -> throwInternal $ "hasql statement error: " <> T.pack (show err)

runRawSql :: HasqlConn.Connection -> Text -> IO ()
runRawSql conn sql = do
  let stmt = Statement.unpreparable sql Encoders.noParams Decoders.noResult
  result <- HasqlConn.use conn (Session.statement () stmt)
  case result of
    Right () -> pure ()
    Left err -> throwInternal $ "hasql sql error: " <> T.pack (show err)

beginCommitOrRollback :: HasqlConn.Connection -> WireArbiter registry a -> WireArbiter registry a
beginCommitOrRollback conn action = do
  withRunInIO $ \run ->
    beginCommitOrRollbackIO conn $
      run $
        local
          (\e -> e {activeConn = Just conn, transactionDepth = 1})
          action

beginCommitOrRollbackIO :: HasqlConn.Connection -> IO a -> IO a
beginCommitOrRollbackIO conn action = mask $ \restore -> do
  runRawSql conn "BEGIN"
  result <- restore action `onException` rollbackSafely
  runRawSql conn "COMMIT"
  pure result
  where
    rollbackSafely = do
      _ <- try (runRawSql conn "ROLLBACK") :: IO (Either SomeException ())
      pure ()

beginSavepointTransaction :: HasqlConn.Connection -> WireArbiter registry a -> WireArbiter registry a
beginSavepointTransaction conn action = do
  env <- ask
  let depth = transactionDepth env
  withRunInIO $ \run ->
    beginSavepointTransactionIO depth conn $
      run $
        local
          (\e -> e {activeConn = Just conn, transactionDepth = depth + 1})
          action

beginSavepointTransactionIO :: Int -> HasqlConn.Connection -> IO a -> IO a
beginSavepointTransactionIO depth conn action = mask $ \restore -> do
  let spName = "arbiter_sp_" <> T.pack (show depth)
  runRawSql conn ("SAVEPOINT " <> spName)
  result <-
    restore action
      `onException` do
        _ <- try (runRawSql conn ("ROLLBACK TO SAVEPOINT " <> spName)) :: IO (Either SomeException ())
        pure ()
  runRawSql conn ("RELEASE SAVEPOINT " <> spName)
  pure result
