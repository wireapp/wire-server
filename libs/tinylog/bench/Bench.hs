-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion
import Criterion.Main
import Data.Int
import System.Logger

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as L

main :: IO ()
main = defaultMain
    [ bgroup "direct"
        [ bench "msg/8"  (whnf (f $ \s _ _ -> renderDefault s) 8)
        , bench "msg/16" (whnf (f $ \s _ _ -> renderDefault s) 16)
        , bench "msg/32" (whnf (f $ \s _ _ -> renderDefault s) 32)
        ]
    , bgroup "netstr"
        [ bench "msg/8"  (whnf (f $ \_ _ _ -> renderNetstr) 8)
        , bench "msg/16" (whnf (f $ \_ _ _ -> renderNetstr) 16)
        , bench "msg/32" (whnf (f $ \_ _ _ -> renderNetstr) 32)
        ]
    , bgroup "custom"
        [ bench "msg/8"  (whnf (f renderCustom) 8)
        , bench "msg/16" (whnf (f renderCustom) 16)
        , bench "msg/32" (whnf (f renderCustom) 32)
        ]
    , bgroup "direct"
        [ bench "field/8"  (whnf (g $ \s _ _ -> renderDefault s) 8)
        , bench "field/16" (whnf (g $ \s _ _ -> renderDefault s) 16)
        , bench "field/32" (whnf (g $ \s _ _ -> renderDefault s) 32)
        ]
    , bgroup "netstr"
        [ bench "field/8"  (whnf (g $ \_ _ _ -> renderNetstr) 8)
        , bench "field/16" (whnf (g $ \_ _ _ -> renderNetstr) 16)
        , bench "field/32" (whnf (g $ \_ _ _ -> renderNetstr) 32)
        ]
    , bgroup "custom"
        [ bench "field/8"  (whnf (g renderCustom) 8)
        , bench "field/16" (whnf (g renderCustom) 16)
        , bench "field/32" (whnf (g renderCustom) 32)
        ]
    ]

f :: Renderer -> Int -> Int64
f r n = L.length
      . render (r ", " iso8601UTC Trace)
      . foldr1 (.)
      . replicate n
      $ msg (val "hello world" +++ (10000 :: Int) +++ (-42 :: Int64))

g :: Renderer -> Int -> Int64
g r n = L.length
      . render (r ", " iso8601UTC Trace)
      . foldr1 (.)
      . replicate n
      $ "key" .= (val "hello world" +++ (10000 :: Int) +++ (-42 :: Int64))


renderCustom :: Renderer
renderCustom s _ _ = encAll mempty
  where
    encAll !acc    []  = acc
    encAll !acc (b:[]) = acc <> encOne b
    encAll !acc (b:bb) = encAll (acc <> encOne b <> sep) bb

    encOne (Bytes b)   = builderBytes b
    encOne (Field k v) = builderBytes k <> eq <> quo <> builderBytes v <> quo

    eq  = B.char8 '='
    quo = B.char8 '"'
    sep = B.byteString s
