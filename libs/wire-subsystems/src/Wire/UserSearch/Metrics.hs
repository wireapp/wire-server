-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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
