{-# LANGUAGE ScopedTypeVariables #-}

module DelayQueue where

import Imports

import Test.Tasty
import Test.Tasty.QuickCheck

import Gundeck.Util.DelayQueue

import qualified Gundeck.Util.DelayQueue as DelayQueue

tests :: TestTree
tests = testGroup "DelayQueue"
    [ testProperty "enqueue/limit"  enqueueLimitProp
    , testProperty "enqueue/unique" enqueueUniqueProp
    , testProperty "enqueue/cancel" enqueueCancelProp
    , testProperty "dequeue/delay"  dequeueDelayProp
    , testProperty "dequeue/order"  dequeueOrderProp
    ]

-- Properties

enqueueLimitProp :: Positive Int -> Property
enqueueLimitProp (Positive l) = ioProperty $ do
    q  <- DelayQueue.new (Clock (return 1)) (Delay 1) (Limit l)
    r  <- forM [1..l+1] $ \(i :: Int) -> DelayQueue.enqueue q i i
    l' <- DelayQueue.length q
    return $ r  == replicate l True ++ [False]
          && l' == l

enqueueUniqueProp :: Positive Int -> Property
enqueueUniqueProp (Positive n) = ioProperty $ do
    q <- DelayQueue.new (Clock (return 1)) (Delay 1) (Limit (n + 1))
    r <- forM [1..n] $ \(i :: Int) -> DelayQueue.enqueue q (1 :: Int) i
    l <- DelayQueue.length q
    return $ all (== True) r && l == 1

enqueueCancelProp :: Int -> Int -> Property
enqueueCancelProp k v = ioProperty $ do
    q  <- DelayQueue.new (Clock (return 1)) (Delay 1) (Limit 1)
    e  <- DelayQueue.enqueue q k v
    l  <- DelayQueue.length q
    c  <- DelayQueue.cancel q k
    l' <- DelayQueue.length q
    return $ e && c && l == 1 && l' == 0

dequeueDelayProp :: Word16 -> Property
dequeueDelayProp d = ioProperty $ do
    c <- newIORef 0
    q <- DelayQueue.new (Clock (readIORef c)) (Delay (fromIntegral d)) (Limit 1)
    e <- DelayQueue.enqueue q (1 :: Int) (1 :: Int)
    r <- forM [1..d] $ \i -> do
        x <- DelayQueue.dequeue q
        tick c
        let diff = fromIntegral (d - (i - 1))
        return $ x == Just (Left (Delay diff))
    s <- DelayQueue.dequeue q
    return $ e && and r && s == Just (Right 1)

dequeueOrderProp :: Int -> Property
dequeueOrderProp k = ioProperty $ do
    c  <- newIORef 0
    q  <- DelayQueue.new (Clock (readIORef c)) (Delay 1) (Limit 2)
    e1 <- DelayQueue.enqueue q k (1 :: Int)
    tick c
    e2 <- DelayQueue.enqueue q (k-1) (2 :: Int)
    tick c
    d1 <- DelayQueue.dequeue q
    d2 <- DelayQueue.dequeue q
    return $ e1 && e2 && d1 == Just (Right 1) && d2 == Just (Right 2)

-- Utilities

tick :: IORef Word64 -> IO ()
tick c = modifyIORef' c (+1)
