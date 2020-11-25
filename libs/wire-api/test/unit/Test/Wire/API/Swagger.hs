module Test.Wire.API.Swagger (tests) where

import Data.Aeson (ToJSON, toJSON)
import Data.Swagger (ToSchema, validatePrettyToJSON)
import Imports
import qualified Test.Tasty as T
import Test.Tasty.QuickCheck (Arbitrary, counterexample, testProperty)
import Type.Reflection (typeRep)
import qualified Wire.API.User as User

tests :: T.TestTree
tests =
  T.localOption (T.Timeout (60 * 1000000) "60s") . T.testGroup "JSON roundtrip tests" $
    [testToJSON @User.UserProfile]

testToJSON :: forall a. (Arbitrary a, Typeable a, ToJSON a, ToSchema a, Show a) => T.TestTree
testToJSON = testProperty msg trip
  where
    msg = show (typeRep @a)
    trip (v :: a) =
      counterexample
        ( fromMaybe ("Schema validation failed, but there were no errors. This looks like a bug in swagger2!") $
            validatePrettyToJSON v
        )
        $ validatePrettyToJSON v == Nothing
