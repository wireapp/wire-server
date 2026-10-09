module FanInPerf.Store
  ( Env (..),
    StoreEffects,
    runStore,
  )
where

import Data.Qualified
import Hasql.Pool (UsageError)
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
