{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Imports
import Control.Concurrent.Async
import Control.Exception (assert)
import Criterion.Main
import Data.Metrics

main :: IO ()
main = do
    m <- metrics
    defaultMain
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
        ]

loop :: Double -> IO () -> IO ()
loop 0  _  = return ()
loop !i io = io >> loop (i - 1) io

contentionIters :: Double
contentionIters = 10000

contentionConc :: Double
contentionConc = 100

contentionSum :: Double
contentionSum = sum [i * contentionIters | i <- [1..contentionConc]]
