module Main (main) where

import qualified Test.Brig.Types.TURN as T
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Tests" [ T.tests ]
