module Aws.Arn where

import Amazonka.Data.Text
import Control.Lens
import Gundeck.Aws.Arn
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Push.V2

tests :: TestTree
tests =
  testGroup
    "Aws.Arn"
    [ testGroup
        "Parser"
        [ testGroup
            "EndpointArn"
            [ testCaseSteps "real world round-trip" realWorldArnTest,
              testCaseSteps "made-up round-trip" madeUpArnTest
            ]
        ]
    ]

realWorldArnTest :: (HasCallStack) => (String -> IO ()) -> Assertion
realWorldArnTest step = do
  step "Given an ARN from a test environment"
  let arnText :: Text = "arn:aws:sns:eu-central-1:091205192927:endpoint/GCM/sven-test-782078216207/ded226c7-45b8-3f6c-9e89-f253340bbb60"
  arnData <-
    either (\e -> assertFailure ("Arn cannot be parsed: " ++ e)) pure (fromText @EndpointArn arnText)

  step "Check that values were parsed correctly"
  (arnData ^. snsRegion) @?= "eu-central-1"
  (arnData ^. snsAccount . to fromAccount) @?= "091205192927"
  (arnData ^. snsTopic . endpointTransport) @?= GCM
  (arnData ^. snsTopic . endpointAppName) @?= "782078216207"
  (arnData ^. snsTopic . endpointId . to (\(EndpointId eId) -> eId)) @?= "ded226c7-45b8-3f6c-9e89-f253340bbb60"

  step "Expect values to be de-serialized correctly"
  (toText arnData) @?= arnText

madeUpArnTest :: (HasCallStack) => (String -> IO ()) -> Assertion
madeUpArnTest step = do
  step "Given an ARN with data to cover untested cases"
  let arnText :: Text = "arn:aws:sns:us-east-2:000000000001:endpoint/APNS/nodash-000000000002/8ffd8d14-db06-4f3a-a3bb-08264b9dbfb0"
  arnData <-
    either (\e -> assertFailure ("Arn cannot be parsed: " ++ e)) pure (fromText @EndpointArn arnText)

  step "Check that values were parsed correctly"
  (arnData ^. snsRegion) @?= "us-east-2"
  (arnData ^. snsAccount . to fromAccount) @?= "000000000001"
  (arnData ^. snsTopic . endpointTransport) @?= APNS
  (arnData ^. snsTopic . endpointAppName) @?= "000000000002"
  (arnData ^. snsTopic . endpointId . to (\(EndpointId eId) -> eId)) @?= "8ffd8d14-db06-4f3a-a3bb-08264b9dbfb0"

  step "Expect values to be de-serialized correctly"
  (toText arnData) @?= arnText
