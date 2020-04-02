module Test.Brig.Roundtrip where

import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Aeson.Types (parseEither)
import Data.Typeable (typeOf)
import Imports
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck ((===), Arbitrary, counterexample, testProperty)

testRoundTrip ::
  forall a.
  (Arbitrary a, Typeable a, ToJSON a, FromJSON a, Eq a, Show a) =>
  TestTree
testRoundTrip = testProperty msg trip
  where
    msg = show $ typeOf (undefined :: a)
    trip (v :: a) =
      counterexample (show $ toJSON v) $
        Right v === (parseEither parseJSON . toJSON) v
