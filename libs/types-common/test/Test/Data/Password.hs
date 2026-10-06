module Test.Data.Password where

import Data.Maybe
import Data.Misc
import Imports
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Password"
    [ testCase "6 char password length, not accepted" $ do
        Nothing @=? toPlainTextPassword8 (plainTextPassword6Unsafe "123456"),
      testCase "7 char password length, not accepted" $ do
        Nothing @=? toPlainTextPassword8 (plainTextPassword6Unsafe "1234567"),
      testCase "8 char password length, accepted" $ do
        Just (plainTextPassword8Unsafe "12345678") @=? toPlainTextPassword8 (fromJust $ plainTextPassword6 "12345678"),
      testCase "12 char password length, accepted" $ do
        Just (plainTextPassword8Unsafe "123456789abc") @=? toPlainTextPassword8 (fromJust $ plainTextPassword6 "123456789abc")
    ]
