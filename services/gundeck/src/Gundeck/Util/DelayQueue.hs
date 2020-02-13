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
import qualified Data.OrdPSQ as PSQ
import Data.Tuple (swap)
import Imports hiding (length)

data DelayQueue k v
  = DelayQueue
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
  return $! DelayQueue queue c d l

enqueue :: Ord k => DelayQueue k v -> k -> v -> IO Bool
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

dequeue :: Ord k => DelayQueue k v -> IO (Maybe (Either Delay v))
dequeue (DelayQueue queue clock _ _) = do
  time <- getTime clock
  atomicModifyIORef' queue $ \q ->
    case PSQ.minView q of
      Nothing -> (q, Nothing)
      Just (_, p, v, q') | p <= time -> (q', Just (Right v))
      Just (_, p, _, _) -> (q, Just (Left (Delay (p - time))))

cancel :: Ord k => DelayQueue k v -> k -> IO Bool
cancel (DelayQueue queue _ _ _) k =
  atomicModifyIORef' queue $
    swap . PSQ.alter (\pv -> (isJust pv, Nothing)) k

length :: DelayQueue k v -> IO Int
length q = PSQ.size <$> readIORef (_queue q)
