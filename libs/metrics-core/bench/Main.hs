{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Control.Exception (assert)
import Criterion.Main
import Data.Metrics
import Util.Test (withWireTastyPatternEnv)

import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Metrics.Buckets as Buckets

main :: IO ()
main = do
    m <- metrics
    withWireTastyPatternEnv $ defaultMain
        [ bgroup "Counter"
            [ bench "add" $ whnfIO $
                counterAdd 1 (path "add") m
            , bench "value" $ whnfIO $ do
                c <- counterGet (path "value") m
                counterValue c
            , bench "contention" $ whnfIO $ do
                c <- counterGet (path "contention") m
                v <- counterValue c
                _ <- mapConcurrently (\n ->
                    loop contentionIters (counterAdd n (path "contention") m)
                    ) [1..contentionConc]
                v' <- counterValue c
                assert (v' == v + contentionSum) (return v')
            ]
        , bgroup "Gauge"
            [ bench "set" $ whnfIO $
                gaugeSet 1 (path "set") m
            , bench "add" $ whnfIO $
                gaugeAdd 1 (path "add") m
            , bench "value" $ whnfIO $ do
                g <- gaugeGet (path "value") m
                gaugeValue g
            ]
        , bgroup "Buckets"
            [ bench "incr" $ whnfIO $
                bucketsIncr 1 1 1 (path "incr") m
            , bench "contention" $ whnfIO $ do
                b <- bucketsGet 1 1 (path "contention") m
                v <- Buckets.snapshot b
                _ <- mapConcurrently (\n ->
                    loop contentionIters (bucketsIncr 1 1 n (path "contention") m)
                    ) [1..contentionConc]
                v' <- Buckets.snapshot b
                assert (HashMap.insertWith (+) 1 (contentionIters * contentionConc) v == v') (return v')
            ]
        ]

loop :: Word -> IO () -> IO ()
loop 0  _  = return ()
loop !i io = io >> loop (i - 1) io

contentionIters :: Word
contentionIters = 10000

contentionConc :: Word
contentionConc = 100

contentionSum :: Word
contentionSum = sum [i * contentionIters | i <- [1..contentionConc]]
