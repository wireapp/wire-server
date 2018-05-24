module Main (main) where

import qualified Test.Properties as Properties
import           Test.Tasty
import           Util.Test (withWireTastyPatternEnv)


main :: IO ()
main = withWireTastyPatternEnv . defaultMain $ testGroup "Tests" [ Properties.tests ]
