{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Temporary adapter that lets wire-server run Arbiter operations against the
-- existing shared Hasql pool.
--
-- The intent is to keep Arbiter logic generic over the monad while wire-server
-- provides the concrete pool and schema inputs from its own effect stack.
--
-- This is intentionally incomplete: transaction pinning is the next boundary
-- that would need a real pool-level abstraction.
module Wire.JobSubsystem.ArbiterAdapter where

import Arbiter.Core.Exceptions (throwInternal)
import Arbiter.Core.HasArbiterSchema (HasArbiterSchema (..))
import Arbiter.Core.MonadArbiter (MonadArbiter (..))
import Arbiter.Core.QueueRegistry (JobPayloadRegistry)
import Arbiter.Hasql.Decode qualified as Decode
import Arbiter.Hasql.Encode qualified as Encode
import Data.Text qualified as T
import Hasql.Connection qualified as Hasql
import Hasql.Pool qualified as HasqlPool
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Imports

data WireArbiterEnv = WireArbiterEnv
  { schemaName :: Text,
    pool :: HasqlPool.Pool,
    inTransaction :: Bool
  }

newtype WireArbiter (registry :: JobPayloadRegistry) a = WireArbiter {unWireArbiter :: ReaderT WireArbiterEnv IO a}
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadReader WireArbiterEnv
    )

instance MonadIO (WireArbiter registry) where
  liftIO = WireArbiter . liftIO

runWireArbiter :: WireArbiterEnv -> WireArbiter registry a -> IO a
runWireArbiter env (WireArbiter action) = runReaderT action env

instance HasArbiterSchema (WireArbiter registry) registry where
  getSchema = WireArbiter $ asks schemaName

instance MonadArbiter (WireArbiter registry) where
  type Handler (WireArbiter registry) jobs result = jobs -> WireArbiter registry result

  executeQuery sql params codec = WireArbiter $ do
    env <- ask
    result <-
      liftIO $
        HasqlPool.use env.pool $
          Session.statement
            ()
            (Statement.unpreparable (Encode.convertPlaceholders sql) (Encode.buildEncoder params) (Decode.hasqlRowDecoder codec))
    case result of
      Right rows -> pure rows
      Left err -> throwInternal $ "hasql query error: " <> T.pack (show err)

  executeStatement sql params = WireArbiter $ do
    env <- ask
    result <-
      liftIO $
        HasqlPool.use env.pool $
          Session.statement () (Encode.buildStatementRowCount sql params)
    case result of
      Right n -> pure n
      Left err -> throwInternal $ "hasql statement error: " <> T.pack (show err)

  withDbTransaction (WireArbiter action) = do
    pool <- asks (.pool)

    WireArbiter $ do
      action

  runHandlerWithConnection handler jobs =
    handler jobs
