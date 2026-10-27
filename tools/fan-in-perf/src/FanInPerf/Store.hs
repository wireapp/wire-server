module FanInPerf.Store
  ( Env (..),
    StoreEffects,
    runStore,
    runOrAbort,
    abort,
  )
where

import Data.Misc (showT)
import Data.Qualified
import Data.Text qualified as T
import FanInPerf.NotificationsAdmin
import FanInPerf.NotificationsAdmin.Postgres
import FanInPerf.NotificationsStore
import FanInPerf.NotificationsStore.Postgres
import Hasql.Pool (UsageError)
import Hasql.Pool.Extended (Pool)
import Hasql.Transaction.Sessions qualified as TxSessions
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import System.Exit (die)

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

-- | 'runStore', exiting with a message on failure.
runOrAbort :: Text -> Env -> Sem StoreEffects a -> IO a
runOrAbort what env act =
  runStore env act >>= either (\e -> abort (what <> " failed: " <> showT e)) pure

abort :: Text -> IO a
abort = die . T.unpack
