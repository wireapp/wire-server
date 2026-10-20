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

module Wire.BudgetStore.DualWrite
  ( interpretBudgetStoreToCassandraAndPostgres,
  )
where

import Cassandra (MonadClient)
import Imports
import Polysemy
import Wire.BudgetStore
import Wire.BudgetStore.Cassandra (budgetStoreToCassandra)
import Wire.BudgetStore.Postgres (interpretBudgetStoreToPostgres)
import Wire.Postgres (PGConstraints)

-- | Cassandra is the source of truth during migration (reads stay Cassandra to
-- preserve in-flight TTL semantics); writes are mirrored to Postgres. Budget
-- rows are ephemeral TTL'd rate-limit counters, so there is no backfill loop.
interpretBudgetStoreToCassandraAndPostgres ::
  forall m r.
  (MonadClient m, Member (Embed m) r, PGConstraints r) =>
  InterpreterFor BudgetStore r
interpretBudgetStoreToCassandraAndPostgres = interpret $ \case
  LookupBudget k ->
    budgetStoreToCassandra @m (lookupBudget k)
  InsertBudget k b -> do
    budgetStoreToCassandra @m (insertBudget k b)
    interpretBudgetStoreToPostgres (insertBudget k b)
