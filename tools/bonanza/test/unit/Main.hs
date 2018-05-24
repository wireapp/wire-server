module Main (main) where

import qualified Test.Bonanza.Parser    as P
import qualified Test.Bonanza.Streaming as S
import           Test.Tasty
import           Util.Test (withWireTastyPatternEnv)

main :: IO ()
main = withWireTastyPatternEnv . defaultMain $
    testGroup "Tests" [ P.tests, S.tests ]
