{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A version of "UnliftIO.Async" with extra utilities.
module UnliftIO.Async.Extended
    ( module UnliftIO.Async
    -- * Pooled functions (using at most T threads)
    , forPooled
    , mapMPooled
    , replicatePooled
    , sequencePooled
    ) where

import UnliftIO
import UnliftIO.Async

import qualified Control.Concurrent.Async.Pool as Pool

-- | A concurrent variant of 'for' that uses at most T threads.
forPooled
    :: MonadUnliftIO m
    => Int -> [a] -> (a -> m b) -> m [b]
forPooled t xs f =
    withRunInIO $ \runInIO ->
    Pool.withTaskGroup t $ \tg ->
      Pool.mapConcurrently tg (runInIO . f) xs

-- | A concurrent variant of 'mapM' that uses at most T threads.
mapMPooled
    :: MonadUnliftIO m
    => Int -> (a -> m b) -> [a] -> m [b]
mapMPooled t f xs =
    withRunInIO $ \runInIO ->
    Pool.withTaskGroup t $ \tg ->
      Pool.mapConcurrently tg (runInIO . f) xs

-- | 'Async.replicateConcurrently' that uses at most T threads.
replicatePooled
    :: MonadUnliftIO m
    => Int                       -- ^ How many threads to use
    -> Int
    -> m a
    -> m [a]
replicatePooled t n = sequencePooled t . replicate n

-- | A concurrent variant of 'sequence' that uses at most T threads.
sequencePooled
    :: MonadUnliftIO m
    => Int -> [m a] -> m [a]
sequencePooled t xs =
    withRunInIO $ \runInIO ->
    Pool.withTaskGroup t $ \tg ->
      Pool.mapConcurrently tg runInIO xs
