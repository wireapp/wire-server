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

module Wire.BudgetStore.Cassandra
  ( budgetStoreToCassandra,
  )
where

import Cassandra
import Imports
import Polysemy
import Wire.BudgetStore

budgetStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (BudgetStore ': r) a ->
  Sem r a
budgetStoreToCassandra =
  interpret $
    embed @m . \case
      LookupBudget k -> lookupBudgetC k
      InsertBudget k b -> insertBudgetC k b

lookupBudgetC :: (MonadClient m) => BudgetKey -> m (Maybe Budget)
lookupBudgetC k = fmap mk <$> query1 budgetSelect (params One (Identity k))
  where
    mk (val, ttl) = Budget (fromIntegral ttl) val

insertBudgetC :: (MonadClient m) => BudgetKey -> Budget -> m ()
insertBudgetC k (Budget ttl val) =
  retry x5 $ write budgetInsert (params One (k, val, round ttl))

-------------------------------------------------------------------------------
-- Queries

budgetInsert :: PrepQuery W (BudgetKey, Int32, Int32) ()
budgetInsert = "INSERT INTO budget (key, budget) VALUES (?, ?) USING TTL ?"

budgetSelect :: PrepQuery R (Identity BudgetKey) (Int32, Int32)
budgetSelect = "SELECT budget, ttl(budget) FROM budget where key = ?"
