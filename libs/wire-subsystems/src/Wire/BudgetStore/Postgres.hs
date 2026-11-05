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

module Wire.BudgetStore.Postgres
  ( interpretBudgetStoreToPostgres,
  )
where

import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement, resultlessStatement)
import Imports
import Polysemy
import Wire.BudgetStore
import Wire.Postgres

interpretBudgetStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor BudgetStore r
interpretBudgetStoreToPostgres =
  interpret $ \case
    LookupBudget k -> lookupBudgetP k
    InsertBudget k b -> insertBudgetP k b

lookupBudgetP :: (PGConstraints r) => BudgetKey -> Sem r (Maybe Budget)
lookupBudgetP (BudgetKey k') =
  fmap mk <$> runStatement k' lookupBudgetStatement
  where
    mk (val, ttl) = Budget (fromIntegral ttl) val

lookupBudgetStatement :: Statement Text (Maybe (Int32, Int32))
lookupBudgetStatement =
  -- Mirrors Cassandra's ttl(budget): remaining whole seconds, 0 once the row
  -- is expired. GREATEST(0, ...) only guards the theoretical now() == row
  -- boundary; the expires_at > now() filter already excludes expired rows.
  [maybeStatement|
      SELECT budget :: int4, GREATEST(0, FLOOR(EXTRACT(EPOCH FROM (expires_at - now())))) :: int4
      FROM budget WHERE key = ($1 :: text) AND expires_at > now()
    |]

insertBudgetP :: (PGConstraints r) => BudgetKey -> Budget -> Sem r ()
insertBudgetP (BudgetKey k') (Budget ttl val) =
  runStatement (k', val, round ttl) insertBudgetStatement

insertBudgetStatement :: Statement (Text, Int32, Int32) ()
insertBudgetStatement =
  [resultlessStatement|
      INSERT INTO budget (key, budget, expires_at)
      VALUES (($1 :: text), ($2 :: int4), now() + make_interval(secs => $3 :: int))
      ON CONFLICT (key) DO UPDATE
      SET budget = ($2 :: int4),
          expires_at = now() + make_interval(secs => $3 :: int)
    |]
