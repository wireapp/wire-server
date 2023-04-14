module Basic where

import App
import qualified Data.Aeson as A
import Imports
import Test.Tasty
import Test.Tasty.Providers (singleTest)
import Text.RawString.QQ

test :: String -> App () -> TestTree
test = singleTest

main :: IO ()
main =
  defaultMain $
    testGroup
      "UnitTests"
      [ test "testJSONUpdate" testJSONUpdate,
        test "testJSONUpdateFailure" testJSONUpdateFailure
      ]

json :: HasCallStack => LByteString -> A.Value
json = fromJust . A.decode

testJSONUpdate :: HasCallStack => App ()
testJSONUpdate = do
  let before =
        json
          [r|
        {
          "foo" : {
             "bar": 2
          }
       }
  |]

  let expected =
        json
          [r|
        {
          "foo" : {
             "bar": "baaz"
          }
       }
  |]

  (before & "foo.bar" %.= ("baaz" :: String)) @%?= expected

  -- test case: when last field doesn't exist

  let expected2 =
        json
          [r|
        {
          "foo" : {
             "bar": 2,
             "quux": 3
          }
       }
  |]

  (before & "foo.quux" %.= (3 :: Int)) @%?= expected2

testJSONUpdateFailure :: HasCallStack => App ()
testJSONUpdateFailure = do
  let before =
        json
          [r|
        {
          "foo" : {
             "bar": 2
          }
       }
  |]

  expectFailure
    (\e -> take 23 e.msg @?= "Field \"quux\" is missing")
    (before & "foo.quux.zok" %.= ("eke" :: String))
