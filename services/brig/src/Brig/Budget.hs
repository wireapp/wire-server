{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Brig.Budget
    ( Budget (..)
    , BudgetKey (..)
    , Budgeted (..)
    , withBudget
    , checkBudget
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
-- FUTUREWORK: encourage caller to define their own type for budget keys (rather than using an
-- untyped text), and represent the types in a way that guarantees that if i'm using a local
-- type that i don't export, then nobody will be able to use my namespace.
--
-- FUTUREWORK: exceptions are not handled very nicely, but it's not clear what it would mean
-- to improve this.
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

-- | Like 'withBudget', but does not decrease budget, only takes a look.
checkBudget :: MonadClient m => BudgetKey -> Budget -> m (Budgeted ())
checkBudget k b = do
    Budget ttl val <- fromMaybe b <$> lookupBudget k
    let remaining = val - 1
    return $ if remaining < 0
        then BudgetExhausted ttl
        else BudgetedValue () remaining

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
