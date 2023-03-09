module Test.Wire.API.RawJson (tests) where

import Data.Proxy
import Imports
import Servant.API
import Test.Tasty
import Test.Tasty.QuickCheck
import Wire.API.RawJson

tests :: TestTree
tests =
  testGroup "RawJson" $
    [ testProperty "MimeUnrender lifts any string to RawJson" testMimeUnrender
    ]

testMimeUnrender :: Property
testMimeUnrender =
  forAll
    (arbitrary :: (Gen LByteString))
    ( \t ->
        mimeUnrender (Proxy @JSON) t == (Right . RawJson) t
    )
