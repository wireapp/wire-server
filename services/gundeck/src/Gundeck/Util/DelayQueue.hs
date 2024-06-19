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

module Gundeck.Util.DelayQueue
  ( DelayQueue,
    Clock (..),
    Delay (..),
    Limit (..),
    new,
    enqueue,
    dequeue,
    cancel,
    length,
    delay,
    limit,
  )
where

import Data.OrdPSQ (OrdPSQ)
import Data.OrdPSQ qualified as PSQ
import Imports hiding (length)

data DelayQueue k v = DelayQueue
  { _queue :: IORef (OrdPSQ k Word64 v),
    _clock :: Clock,
    delay :: !Delay,
    limit :: !Limit
  }

newtype Clock = Clock {getTime :: IO Word64}

newtype Delay = Delay {delayTime :: Word64}
  deriving (Eq, Show, Ord)

newtype Limit = Limit {getLimit :: Int}
  deriving (Eq, Show, Ord)

new :: Clock -> Delay -> Limit -> IO (DelayQueue k v)
new c d l = do
  queue <- newIORef PSQ.empty
  pure $! DelayQueue queue c d l

enqueue :: (Ord k) => DelayQueue k v -> k -> v -> IO Bool
enqueue (DelayQueue queue clock d l) k v = do
  time <- getTime clock
  let !p = time + delayTime d
  atomicModifyIORef' queue $ \q ->
    if PSQ.size q >= getLimit l
      then (q, False)
      else
        swap $
          PSQ.alter
            ( \case
                Nothing -> (True, Just (p, v))
                Just pv -> (True, Just pv)
            )
            k
            q

dequeue :: (Ord k) => DelayQueue k v -> IO (Maybe (Either Delay v))
dequeue (DelayQueue queue clock _ _) = do
  time <- getTime clock
  atomicModifyIORef' queue $ \q ->
    case PSQ.minView q of
      Nothing -> (q, Nothing)
      Just (_, p, v, q') | p <= time -> (q', Just (Right v))
      Just (_, p, _, _) -> (q, Just (Left (Delay (p - time))))

cancel :: (Ord k) => DelayQueue k v -> k -> IO Bool
cancel (DelayQueue queue _ _ _) k =
  atomicModifyIORef' queue $
    swap . PSQ.alter (\pv -> (isJust pv, Nothing)) k

length :: DelayQueue k v -> IO Int
length q = PSQ.size <$> readIORef (_queue q)
