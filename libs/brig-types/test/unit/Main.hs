module Main (main) where

import qualified Test.Brig.Types.Common
import qualified Test.Brig.Types.TURN
import qualified Test.Brig.Types.User
import           Test.Tasty
import           Util.Test (withWireTastyPatternEnv)

main :: IO ()
main = withWireTastyPatternEnv . defaultMain $ testGroup "Tests"
    [ Test.Brig.Types.Common.tests
    , Test.Brig.Types.TURN.tests
    , Test.Brig.Types.User.tests
    ]
