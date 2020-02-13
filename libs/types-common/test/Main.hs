module Main (main) where

import Imports
import qualified Test.Properties as Properties
import qualified Test.SizedHashMap as SizedHashMap
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Tests" [Properties.tests, SizedHashMap.tests]
