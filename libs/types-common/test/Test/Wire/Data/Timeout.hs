module Test.Wire.Data.Timeout where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import qualified Wire.Data.Timeout as WireTimeout

tests :: TestTree
tests =
  testGroup
    "Wire.Data.Timeout"
    [ testCase "defaults small numbers in JSON de-serialization" testFromJSONDefaultsSmallNumbers,
      testCase "defaults big numbers in JSON de-serialization" testFromJSONDefaultsBigNumbers,
      testCase "defaults decimal numbers in JSON de-serialization" testFromJSONDefaultsBigNumbers
    ]

testFromJSONDefaultsDecimalNumbers :: Assertion
testFromJSONDefaultsDecimalNumbers = assertDefaultedTimeout "0.9"

testFromJSONDefaultsSmallNumbers :: Assertion
testFromJSONDefaultsSmallNumbers = (assertDefaultedTimeout . fromString . show) smallInteger
  where
    smallInteger :: Integer
    smallInteger = toInteger (minBound :: Int64) - 1

testFromJSONDefaultsBigNumbers :: Assertion
testFromJSONDefaultsBigNumbers = (assertDefaultedTimeout . fromString . show) bigInteger
  where
    bigInteger :: Integer
    bigInteger = toInteger (maxBound :: Int64) + 1

assertDefaultedTimeout :: BL.ByteString -> Assertion
assertDefaultedTimeout numberBS =
  let decoded = decode numberBS :: Maybe WireTimeout.Timeout
   in fromJust decoded @?= WireTimeout.Timeout 3600

