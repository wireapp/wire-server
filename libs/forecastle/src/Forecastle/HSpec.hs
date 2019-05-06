module Forecastle.HSpec
    ( testGroup
    , test
    , TestDescription
    ) where

import Imports
import Test.Hspec
import Forecastle

type TestDescription = String
test :: IO e -> TestDescription -> TestM e a -> Spec
test setupEnv desc h = before setupEnv $ specify desc (runTestM_ h)

testGroup :: TestDescription -> [SpecWith a] -> SpecWith a
testGroup desc specs = describe desc (sequence_ specs)

