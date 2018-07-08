module Main where

import Test.Tasty

import qualified Bench            as B
import qualified Test.Cannon.Dict as D

main :: IO ()
main = do
    B.benchmark
    defaultMain $ testGroup "Tests" [ D.tests ]
