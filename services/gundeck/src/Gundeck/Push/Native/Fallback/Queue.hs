{-# LANGUAGE BangPatterns #-}

module Gundeck.Push.Native.Fallback.Queue
    ( Queue
    , Delay (..)
    , Limit (..)
    , Burst (..)
    , queueDelay
    , queueLength
    , newQueue
    , schedule
    , cancel
    , drainQueue
    ) where

import Imports
import Control.Concurrent.Async (Async, async)
import Data.Id
import Gundeck.Types.Notification
import Gundeck.Util.DelayQueue (DelayQueue, Delay (..), Limit (..), Clock (..))

import qualified Control.Concurrent.Async as Async
import qualified Gundeck.Util.DelayQueue  as DelayQueue

data Queue = Queue
    { queueQueue    :: DelayQueue Key (IO ())
    , queueClock    :: Async ()
    , queueConsumer :: Async ()
    }

data Key = Key !UserId !NotificationId
    deriving (Eq, Show, Ord)

newtype Burst = Burst Word16 deriving (Eq, Show)

newQueue :: MonadIO m => Delay -> Limit -> Burst -> m Queue
newQueue delay limit (Burst burst) = liftIO $ do
    c <- newIORef 0
    q <- DelayQueue.new (Clock (readIORef c)) delay limit
    a <- async $ tick c
    t <- async $ consume q 0
    return $ Queue q a t
  where
    oneSecond = 1000000 -- µs

    tick c = do
        threadDelay oneSecond
        atomicModifyIORef' c (\n -> (n + 1, ()))
        tick c

    consume q !n = do
        task <- DelayQueue.dequeue q
        case task of
            Nothing -> do
                threadDelay oneSecond
                consume q 0
            Just (Left (Delay t)) -> do
                threadDelay $ max oneSecond (fromIntegral t * oneSecond)
                consume q 0
            Just (Right io) -> do
                void $ forkIO io
                if n < burst then consume q (n + 1)
                else threadDelay oneSecond >> consume q 0

queueDelay :: Queue -> Delay
queueDelay = DelayQueue.delay . queueQueue

queueLength :: MonadIO m => Queue -> m Int
queueLength = liftIO . DelayQueue.length . queueQueue

schedule :: MonadIO m => Queue -> UserId -> NotificationId -> IO () -> m Bool
schedule q u n = liftIO . DelayQueue.enqueue (queueQueue q) (Key u n)

cancel :: MonadIO m => Queue -> UserId -> NotificationId -> m Bool
cancel q u n = liftIO $ DelayQueue.cancel (queueQueue q) (Key u n)

-- | Wait up to a specified number of seconds for a queue to drain.
drainQueue :: MonadIO m => Queue -> Word16 -> m ()
drainQueue q !d = liftIO $ do
    len <- DelayQueue.length (queueQueue q)
    if len == 0 || d == 0 then do
        Async.cancel (queueConsumer q)
        Async.cancel (queueClock q)
    else do
        threadDelay 1000000
        drainQueue q (d - 1)
