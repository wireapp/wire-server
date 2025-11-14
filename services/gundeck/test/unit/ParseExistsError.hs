-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module ParseExistsError where

import Amazonka.Types
import Gundeck.Aws
import Gundeck.Aws.Arn
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Push.V2 (Transport (APNS))

tests :: TestTree
tests =
  testGroup
    "parseExistsError"
    [ testCase "parse error string from real world example" parseErrorTest,
      testCase "parse errors" parseErrors
    ]

parseErrorTest :: Assertion
parseErrorTest =
  let e = Just $ ErrorMessage "Invalid parameter: Token Reason: Endpoint arn:aws:sns:eu-west-1:093205192929:endpoint/APNS/staging-com.wire.dev.ent/f90c8f08-a0a1-33bc-aa43-23c94770d936 already exists with the same Token, but different attributes."
      expectedTopic = mkEndpointTopic (ArnEnv "staging") APNS "com.wire.dev.ent" (EndpointId "f90c8f08-a0a1-33bc-aa43-23c94770d936")
      expected = mkSnsArn Ireland (Account "093205192929") expectedTopic
   in parseExistsError e @?= Just expected

parseErrors :: Assertion
parseErrors = do
  parseExistsError Nothing @?= Nothing
  parseExistsError ((Just . ErrorMessage) "Invalid parameter: Token Reason: Endpoint") @?= Nothing
  parseExistsError
    ( (Just . ErrorMessage)
        "Invalid parameter: Token Reason: Endpoint NO_ARN already exists with the same Token, but different attributes."
    )
    @?= Nothing
  parseExistsError
    ( (Just . ErrorMessage)
        "Invalid parameter: Token Reason: Endpoint arn:aws:sns:eu-west-1:093205192929:endpoint/APNS/staging-com.wire.dev.ent/f90c8f08-a0a1-33bc-aa43-23c94770d936"
    )
    @?= Nothing
