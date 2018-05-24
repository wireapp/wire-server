module Main (main) where

import Test.Tasty
import Util.Test (withWireTastyPatternEnv)
import ZAuth

main :: IO ()
main = withWireTastyPatternEnv . defaultMain =<< tests
