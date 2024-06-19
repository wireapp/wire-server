{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Budget
  ( Budget (..),
    BudgetKey (..),
    Budgeted (..),
    withBudget,
    checkBudget,
    lookupBudget,
    insertBudget,
  )
where

import Cassandra
import Data.Time.Clock
import Imports

data Budget = Budget
  { budgetTimeout :: !NominalDiffTime,
    budgetValue :: !Int32
  }
  deriving (Eq, Show, Generic)

data Budgeted a
  = BudgetExhausted NominalDiffTime
  | BudgetedValue a Int32
  deriving (Eq, Show, Generic)

newtype BudgetKey = BudgetKey Text
  deriving (Eq, Show, Cql)

-- | @withBudget (BudgetKey "k") (Budget 30 5) action@ runs @action@ at most 5 times every 30
-- seconds.  @"k"@ is used for keeping different calls to 'withBudget' apart; use something
-- there that's unique to your context, like @"login#" <> uid@.
--
-- See the docs in "Gundeck.ThreadBudget" for related work.
--
-- FUTUREWORK: encourage caller to define their own type for budget keys (rather than using an
-- untyped text), and represent the types in a way that guarantees that if i'm using a local
-- type that i don't export, then nobody will be able to use my namespace.
--
-- FUTUREWORK: exceptions are not handled very nicely, but it's not clear what it would mean
-- to improve this.
withBudget :: (MonadClient m) => BudgetKey -> Budget -> m a -> m (Budgeted a)
withBudget k b ma = do
  Budget ttl val <- fromMaybe b <$> lookupBudget k
  let remaining = val - 1
  if remaining < 0
    then pure (BudgetExhausted ttl)
    else do
      a <- ma
      insertBudget k (Budget ttl remaining)
      pure (BudgetedValue a remaining)

-- | Like 'withBudget', but does not decrease budget, only takes a look.
checkBudget :: (MonadClient m) => BudgetKey -> Budget -> m (Budgeted ())
checkBudget k b = do
  Budget ttl val <- fromMaybe b <$> lookupBudget k
  let remaining = val - 1
  pure $
    if remaining < 0
      then BudgetExhausted ttl
      else BudgetedValue () remaining

lookupBudget :: (MonadClient m) => BudgetKey -> m (Maybe Budget)
lookupBudget k = fmap mk <$> query1 budgetSelect (params One (Identity k))
  where
    mk (val, ttl) = Budget (fromIntegral ttl) val

insertBudget :: (MonadClient m) => BudgetKey -> Budget -> m ()
insertBudget k (Budget ttl val) =
  retry x5 $ write budgetInsert (params One (k, val, round ttl))

-------------------------------------------------------------------------------
-- Queries

budgetInsert :: PrepQuery W (BudgetKey, Int32, Int32) ()
budgetInsert = "INSERT INTO budget (key, budget) VALUES (?, ?) USING TTL ?"

budgetSelect :: PrepQuery R (Identity BudgetKey) (Int32, Int32)
budgetSelect = "SELECT budget, ttl(budget) FROM budget where key = ?"
