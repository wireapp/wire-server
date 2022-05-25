{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Brig.Sem.BudgetStore where

import Cassandra (Cql)
import Data.Time.Clock
import Imports
import Polysemy

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

data BudgetStore m a where
  GetBudget :: BudgetKey -> BudgetStore m (Maybe (Int32, Int32))
  InsertBudget :: BudgetKey -> Budget -> BudgetStore m ()

makeSem ''BudgetStore

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
withBudget ::
  Member BudgetStore r =>
  BudgetKey ->
  Budget ->
  Sem r a ->
  Sem r (Budgeted a)
withBudget k b ma = do
  Budget ttl val <- fromMaybe b <$> lookupBudget k
  let remaining = val - 1
  if remaining < 0
    then return (BudgetExhausted ttl)
    else do
      a <- ma
      insertBudget k (Budget ttl remaining)
      return (BudgetedValue a remaining)

-- | Like 'withBudget', but does not decrease budget, only takes a look.
checkBudget :: Member BudgetStore r => BudgetKey -> Budget -> Sem r (Budgeted ())
checkBudget k b = do
  Budget ttl val <- fromMaybe b <$> lookupBudget k
  let remaining = val - 1
  return $
    if remaining < 0
      then BudgetExhausted ttl
      else BudgetedValue () remaining

lookupBudget :: Member BudgetStore r => BudgetKey -> Sem r (Maybe Budget)
lookupBudget k = fmap mk <$> getBudget k
  where
    mk (val, ttl) = Budget (fromIntegral ttl) val
