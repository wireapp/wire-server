{-# LANGUAGE OverloadedStrings #-}

module Bench (Bench.benchmark) where

import Imports
import Control.Concurrent.Async
import Criterion
import Criterion.Main
import Data.UUID
import Data.UUID.V4

import qualified Cannon.Dict as D

benchmark :: IO ()
benchmark = defaultMain
    [ bgroup "Cannon.Dict"
        [ bench "slices 1"    $ nfIO (slices 1    1280)
        , bench "slices 10"   $ nfIO (slices 10   1280)
        , bench "slices 100"  $ nfIO (slices 100  1280)
        , bench "slices 1000" $ nfIO (slices 1000 1280)
        ]
    ]

slices :: Int -> Int -> IO ()
slices s n = do
    let nthreads = 8
    d <- D.empty s
    mapM_ wait =<< replicateM nthreads (async $ action d)
    x <- D.size d
    unless (x == nthreads) $
        error (show x)
  where
    action d = do
        uid <- toByteString <$> nextRandom
        dat <- toByteString <$> nextRandom
        replicateM_ n $
            D.insert uid dat d
        tad <- D.lookup uid d
        unless (Just dat == tad) $
            error "Ooops."
