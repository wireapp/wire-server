module Main (main) where

import Imports
import qualified Test.Bonanza.Parser as P
import qualified Test.Bonanza.Streaming as S
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup "Tests" [P.tests, S.tests]
