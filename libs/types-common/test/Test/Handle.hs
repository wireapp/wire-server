module Test.Handle
  ( tests,
  )
where

import Data.Handle (Handle (fromHandle), parseHandleEither)
import qualified Data.Text as Text
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Handle"
    [ testGroup "serialization" testHandleSerialization
    ]

testHandleSerialization :: [TestTree]
testHandleSerialization =
  [ testCase "parses some example handles" $ do
      let validHandles = ["handle", "--", "__", "..", "0123456789", Text.replicate 256 "a"]
      for_ validHandles $ \h ->
        case parseHandleEither h of
          Right _ -> pure ()
          Left err -> assertFailure $ "valid handle " <> show h <> " not parsed successfully: " <> err,
    testCase "rejects invalid handles" $ do
      let invalidHandles =
            [ "h", -- too short
              Text.replicate 257 "a", -- too long
              "myhändle",
              "myhàndle",
              "myh@ndle",
              "$pecial",
              "some+name",
              "upperCase",
              "with space"
            ]
      for_ invalidHandles $ \h ->
        case parseHandleEither h of
          Left _ -> pure ()
          Right parsed -> assertFailure $ "invalid handle parsed successfully: " <> show (h, parsed),
    testProperty "roundtrip for Handle" $
      \(x :: Handle) ->
        parseHandleEither (fromHandle x) === Right x
  ]
