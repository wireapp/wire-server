{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Queue
  ( Queue,
    new,
    tryPush,
    pop,
    len,
  )
where

import qualified Control.Concurrent.STM as Stm
import Imports
import Numeric.Natural (Natural)

data Queue a = Queue
  { _len :: Stm.TVar Word,
    _queue :: Stm.TBQueue a
  }

new :: MonadIO m => Natural -> m (Queue a)
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
