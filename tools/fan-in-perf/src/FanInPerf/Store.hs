module FanInPerf.Store
  ( Env (..),
    StoreEffects,
    runStore,
    describeUsageError,
    truncateText,
    toIsolationLevel,
  )
where

import Data.Qualified
import Data.Text qualified as T
import FanInPerf.Options (Isolation (..))
import Hasql.Pool (UsageError (..))
import Hasql.Pool.Extended (Pool)
import Hasql.Transaction.Sessions qualified as TxSessions
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.FanInNotificationsAdmin
import Wire.FanInNotificationsAdmin.Postgres
import Wire.FanInNotificationsStore
import Wire.FanInNotificationsStore.Postgres

data Env = Env
  { pool :: Pool,
    local :: Local (),
    isolation :: TxSessions.IsolationLevel
  }

type StoreEffects =
  '[ FanInNotificationsStore,
     FanInNotificationsAdmin,
     Input (Local ()),
     Input Pool,
     Error UsageError,
     Embed IO
   ]

runStore :: Env -> Sem StoreEffects a -> IO (Either UsageError a)
runStore env =
  runM
    . runError
    . runInputConst env.pool
    . runInputConst env.local
    . interpretFanInNotificationsAdminToPostgres
    . interpretFanInNotificationsStoreToPostgres env.isolation

-- | Connection errors can contain host names or credentials; keep them out of
-- the terminal.
describeUsageError :: UsageError -> Text
describeUsageError = \case
  ConnectionError _ -> "database connection error"
  AcquisitionTimeoutUsageError -> "connection pool acquisition timeout"
  SessionError e -> "database session error: " <> truncateText 200 (T.pack (show e))

toIsolationLevel :: Isolation -> TxSessions.IsolationLevel
toIsolationLevel = \case
  ReadCommitted -> TxSessions.ReadCommitted
  Serializable -> TxSessions.Serializable

truncateText :: Int -> Text -> Text
truncateText n t = if T.length t > n then T.take n t <> "…" else t
