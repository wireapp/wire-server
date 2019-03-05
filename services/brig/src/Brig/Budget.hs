{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Brig.Budget
    ( Budget (..)
    , BudgetKey (..)
    , Budgeted (..)
    , withBudget
    , lookupBudget
    , insertBudget
    ) where

import Imports
import Brig.Data.Instances ()
import Cassandra
import Data.Time.Clock

data Budget = Budget
    { budgetTimeout :: !NominalDiffTime
    , budgetValue   :: !Int32
    }

data Budgeted a
    = BudgetExhausted NominalDiffTime
    | BudgetedValue a Int32

newtype BudgetKey = BudgetKey Text
    deriving (Eq, Show, Cql)

withBudget :: MonadClient m => BudgetKey -> Budget -> m a -> m (Budgeted a)
withBudget k b ma = do
    Budget ttl val <- fromMaybe b <$> lookupBudget k
    let remaining = val - 1
    if remaining < 0
        then return (BudgetExhausted ttl)
        else do
            a <- ma
            insertBudget k (Budget ttl remaining)
            return (BudgetedValue a remaining)

lookupBudget :: MonadClient m => BudgetKey -> m (Maybe Budget)
lookupBudget k = fmap mk <$> query1 budgetSelect (params One (Identity k))
  where
    mk (val, ttl) = Budget (fromIntegral ttl) val

insertBudget :: MonadClient m => BudgetKey -> Budget -> m ()
insertBudget k (Budget ttl val) =
    retry x5 $ write budgetInsert (params One (k, val, round ttl))

-------------------------------------------------------------------------------
-- Queries

budgetInsert :: PrepQuery W (BudgetKey, Int32, Int32) ()
budgetInsert = "INSERT INTO budget (key, budget) VALUES (?, ?) USING TTL ?"

budgetSelect :: PrepQuery R (Identity BudgetKey) (Int32, Int32)
budgetSelect = "SELECT budget, ttl(budget) FROM budget where key = ?"
