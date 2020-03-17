module Test.Brig.Types.Team where

import Brig.Types.Team
import Imports
import Test.Brig.Roundtrip (testRoundTrip)
import Test.Tasty
import Test.Tasty.QuickCheck (Arbitrary, arbitrary, arbitrarySizedNatural)

tests :: TestTree
tests = testGroup "Team" $ [testRoundTrip @TeamSize]

instance Arbitrary TeamSize where
  arbitrary = TeamSize <$> arbitrarySizedNatural
