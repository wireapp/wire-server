{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A version of "Control.Concurrent.Async.Lifted.Safe" with extra utilities.
module Control.Concurrent.Async.Lifted.Safe.Extended
    ( module Control.Concurrent.Async.Lifted.Safe
    -- * Pooled functions (using at most T threads)
    , forPooled
    , replicatePooled
    , sequencePooled
    ) where

import Control.Monad.Trans.Control
import Data.Constraint ((:-), (\\))
import Data.Constraint.Forall (inst)
import Control.Concurrent.Async.Lifted.Safe

import qualified Control.Concurrent.Async.Pool as Pool

-- | A concurrent variant of 'for' that uses at most T threads.
forPooled
    :: forall m a b . (MonadBaseControl IO m, Forall (Pure m))
    => Int -> [a] -> (a -> m b) -> m [b]
forPooled t xs f =
    (\\ (inst :: Forall (Pure m) :- Pure m b)) $
    liftBaseWith $ \runInIO ->
    Pool.withTaskGroup t $ \tg ->
      Pool.mapConcurrently tg (runInIO . f) xs

-- | 'Async.replicateConcurrently' that uses at most T threads.
replicatePooled
    :: (MonadBaseControl IO m, Forall (Pure m))
    => Int                       -- ^ How many threads to use
    -> Int
    -> m a
    -> m [a]
replicatePooled t n = sequencePooled t . replicate n

-- | A concurrent variant of 'sequence' that uses at most T threads.
sequencePooled
    :: forall m a . (MonadBaseControl IO m, Forall (Pure m))
    => Int -> [m a] -> m [a]
sequencePooled t xs =
    (\\ (inst :: Forall (Pure m) :- Pure m a)) $
    liftBaseWith $ \runInIO ->
    Pool.withTaskGroup t $ \tg ->
      Pool.mapConcurrently tg runInIO xs
