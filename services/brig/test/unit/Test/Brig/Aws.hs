{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Brig.Aws where

import Brig.AWS.Types
import Data.Aeson hiding (json)
-- import Data.Aeson.QQ (aesonQQ)
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as Text
import Imports
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Aws"
    [ testGroup
        "SES"
        [ testCase "sesnotification simple email" testParsingSimple,
          testCase "sesnotification email with title" testParsing
        ]
    ]

testParsingSimple :: IO ()
testParsingSimple = do
  let jsonBody = customSesNotificationJson "prefix@email.com"
      res :: Maybe SESNotification
      res = decodeStrict jsonBody

  print jsonBody
  print res
  assertBool "can parse SES event as SESNotification" (isJust res)

testParsing :: IO ()
testParsing = do
  let jsonBody = customSesNotificationJson "\\\"title prefix\\\" <prefix@email.com>"
      res :: Maybe SESNotification
      res = decodeStrict jsonBody

  print jsonBody
  print res
  assertBool "can parse SES event as SESNotification" (isJust res)

customSesNotificationJson :: Text -> ByteString
customSesNotificationJson email = Text.encodeUtf8 $ "{\"notificationType\":\"Bounce\",\"bounce\":{\"bounceType\":\"Transient\",\"bounceSubType\":\"General\",\"bouncedRecipients\":[{\"emailAddress\":\"" <> email <> "\"}],\"timestamp\":\"2020-08-10T13:00:37.000Z\",\"feedbackId\":\"01020173d8756050-7d9a1f36-4b8f-424b-b167-83d58c4d8a02-000000\"},\"mail\":{\"timestamp\":\"2020-08-10T13:00:37.906Z\",\"source\":\"Wire <payments@wire.com>\",\"sourceArn\":\"arn:aws:ses:eu-west-1:1234:identity/wire.com\",\"sourceIp\":\"1.2.3.4\",\"sendingAccountId\":\"1234\",\"messageId\":\"01020173d87558cf-ca4a2c12-5133-4e81-9676-a5cd106bfe12-000000\",\"destination\":[\"email@domain.com\"]}}"

-- NOTE: using the following with quasiquoter and 'decode' instead of decodeStrict, the tests also pass. Something is off ...
--
-- customSesNotificationJson1 :: Text -> BL.ByteString
-- customSesNotificationJson1 email =
--   encode
--     [aesonQQ|
--       { "bounce" : {
--               "bounceSubType" : "General",
--               "bounceType" : "Transient",
--               "bouncedRecipients" : [
--                  {
--                     "emailAddress" : #{email}
--                  }
--               ],
--               "feedbackId" : "01020173d8756050-7d9a1f36-4b8f-424b-b167-83d58c4d8a02-000000",
--               "timestamp" : "2020-08-10T13:00:37.000Z"
--            },
--            "mail" : {
--               "destination" : [
--                  "prefix@domain.com"
--               ],
--               "messageId" : "01020173d87558cf-ca4a2c12-5133-4e81-9676-a5cd106bfe12-000000",
--               "sendingAccountId" : "12345677",
--               "source" : "Wire <payments@wire.com>",
--               "sourceArn" : "arn:aws:ses:eu-west-1:093205192929:identitygwire.com",
--               "sourceIp" : "34.243.216.253",
--               "timestamp" : "2020-08-10T13:00:37.906Z"
--            },
--            "notificationType" : "Bounce"
--       } |]
