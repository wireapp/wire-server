module Main (main) where

import           Imports
import qualified Test.Properties as Properties
import           Test.Tasty


main :: IO ()
main = defaultMain $ testGroup "Tests" [ Properties.tests ]
