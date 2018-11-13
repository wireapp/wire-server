{-# LANGUAGE StrictData #-}

module Galley.Queue
    ( Queue
    , new
    , tryPush
    , pop
    , len
    ) where

import Imports
import qualified Control.Concurrent.STM as Stm

data Queue a = Queue
    { _len   :: Stm.TVar Word
    , _queue :: Stm.TBQueue a
    }

new :: MonadIO m => Int -> m (Queue a)
new n = liftIO $ Queue <$> Stm.newTVarIO 0 <*> Stm.newTBQueueIO n

tryPush :: MonadIO m => Queue a -> a -> m Bool
tryPush q a = liftIO . atomically $ do
    isFull <- Stm.isFullTBQueue (_queue q)
    unless isFull $ do
        Stm.modifyTVar' (_len q) succ
        Stm.writeTBQueue (_queue q) a
    pure (not isFull)

pop :: MonadIO m => Queue a -> m a
pop q = liftIO . atomically $ do
    Stm.modifyTVar' (_len q) (pred . max 1)
    Stm.readTBQueue (_queue q)

len :: MonadIO m => Queue a -> m Word
len q = liftIO $ Stm.readTVarIO (_len q)
