module Wire.UserSearch.Metrics where

import Imports
import Prometheus qualified as Prom

{-# NOINLINE indexUpdateCounter #-}
indexUpdateCounter :: Prom.Counter
indexUpdateCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "user_index_update_count",
          Prom.metricHelp = "Number of updates on user index"
        }

{-# NOINLINE indexUpdateErrorCounter #-}
indexUpdateErrorCounter :: Prom.Counter
indexUpdateErrorCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "user_index_update_err",
          Prom.metricHelp = "Number of errors during user index update"
        }

{-# NOINLINE indexUpdateSuccessCounter #-}
indexUpdateSuccessCounter :: Prom.Counter
indexUpdateSuccessCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "user_index_update_ok",
          Prom.metricHelp = "Number of successful user index updates"
        }

{-# NOINLINE indexDeleteCounter #-}
indexDeleteCounter :: Prom.Counter
indexDeleteCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "user_index_delete_count",
          Prom.metricHelp = "Number of deletes on user index"
        }
