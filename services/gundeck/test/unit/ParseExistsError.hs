module ParseExistsError where

import Amazonka.Types
import Gundeck.Aws
import Gundeck.Aws.Arn
import Gundeck.Types.Push.V2 (Transport (APNS))
import Imports
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "parseExistsError"
    [ testCase "parse error string from real world example" parseErrorTest
    ]

parseErrorTest :: Assertion
parseErrorTest =
  let e = Just $ ErrorMessage "Invalid parameter: Token Reason: Endpoint arn:aws:sns:eu-west-1:093205192929:endpoint/APNS/staging-com.wire.dev.ent/f90c8f08-a0a1-33bc-aa43-23c94770d936 already exists with the same Token, but different attributes."
      expectedTopic = mkEndpointTopic (ArnEnv "staging") APNS "com.wire.dev.ent" (EndpointId "f90c8f08-a0a1-33bc-aa43-23c94770d936")
      expected = mkSnsArn Ireland (Account "093205192929") expectedTopic
   in parseExistsError e @?= Just expected
