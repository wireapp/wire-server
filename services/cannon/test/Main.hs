module Main where

import Test.Tasty
import Util.Test (withWireTastyPatternEnv)

import qualified Bench            as B
import qualified Test.Cannon.Dict as D

main :: IO ()
main = do
    B.benchmark
    withWireTastyPatternEnv . defaultMain $ testGroup "Tests" [ D.tests ]
