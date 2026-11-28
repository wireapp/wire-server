-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Brig.Options (tests) where

import Brig.Options
import Data.Aeson (Value, eitherDecode, object, parseJSON, (.=))
import Data.Aeson.Types (parseEither)
import Data.Map.Strict qualified as Map
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.User.EmailAddress (fromEmail)

tests :: TestTree
tests =
  testGroup
    "Brig.Options"
    [ testGroup
        "deriveSsoIdpChangeDetectionEnabled"
        [ testCase "absent inputs disable" $
            deriveSsoIdpChangeDetectionEnabled Nothing @?= False,
          testCase "empty inputs disable" $
            deriveSsoIdpChangeDetectionEnabled (Just (SsoIdpChangeDetectionInputs Map.empty [])) @?= False,
          testCase "domains without allowlist disable" $
            deriveSsoIdpChangeDetectionEnabled (Just (SsoIdpChangeDetectionInputs (Map.singleton "example.com" (object [])) [])) @?= False,
          testCase "allowlist without domains disable" $
            deriveSsoIdpChangeDetectionEnabled (Just (SsoIdpChangeDetectionInputs Map.empty ["AA:BB"])) @?= False,
          testCase "domains and allowlist enable" $
            deriveSsoIdpChangeDetectionEnabled (Just (SsoIdpChangeDetectionInputs (Map.singleton "example.com" (object [])) ["AA:BB"])) @?= True,
          testCase "chart-rendered JSON parses" $
            eitherDecode
              "{\"multiIngressDomainConfigs\":{\"example.com\":{}},\"idpCertFingerprintAllowlist\":[\"AA:BB\"]}"
              @?= Right (SsoIdpChangeDetectionInputs (Map.singleton "example.com" (object [])) ["AA:BB"])
        ],
      testGroup
        "email sender domain validation"
        [ testCase "accepts a sender with a valid domain" $
            case eitherDecode "\"noreply@wire.com\"" :: Either String StricterDomain of
              Left err -> assertFailure err
              Right _ -> pure (),
          testCase "rejects a sender whose final domain label starts with a digit" $
            case eitherDecode "\"noreply@diana.123\"" :: Either String StricterDomain of
              Left _ -> pure ()
              Right _ -> assertFailure "expected invalid sender domain"
        ],
      testGroup
        "meeting email sender"
        [ testCase "falls back to emailSender when meeting is absent" $
            senderOf (generalOpts Nothing) @?= Right "noreply@wire.com",
          testCase "falls back to emailSender when meeting.sender is absent" $
            senderOf (generalOpts (Just (object ["replyTo" .= ("reply@wire.com" :: Text)]))) @?= Right "noreply@wire.com",
          testCase "uses meeting.sender when set" $
            senderOf (generalOpts (Just (object ["sender" .= ("meetings@wire.com" :: Text)]))) @?= Right "meetings@wire.com",
          testCase "rejects a meeting.sender with an invalid domain" $
            assertBool "expected invalid meeting sender to be rejected" . isLeft $
              senderOf (generalOpts (Just (object ["sender" .= ("meetings@diana.123" :: Text)])))
        ],
      testGroup
        "searchBackend"
        [ testCase "elasticsearch parses" $
            (eitherDecode "\"elasticsearch\"" :: Either String SearchBackend) @?= Right SearchBackendElasticSearch,
          testCase "postgres parses" $
            (eitherDecode "\"postgres\"" :: Either String SearchBackend) @?= Right SearchBackendPostgres,
          testCase "invalid value fails to parse" $
            case eitherDecode "\"sqlite\"" :: Either String SearchBackend of
              Left _ -> pure ()
              Right _ -> assertFailure "expected invalid searchBackend to fail parsing"
        ]
    ]
  where
    senderOf :: Value -> Either String Text
    senderOf v = fromEmail . checkedMeetingEmailSender <$> parseEither parseJSON v
    generalOpts :: Maybe Value -> Value
    generalOpts mMeeting =
      object $
        [ "templateDir" .= ("templates" :: Text),
          "emailSender" .= ("noreply@wire.com" :: Text),
          "smsSender" .= ("+123456789" :: Text),
          "templateBranding"
            .= object
              [ k .= ("x" :: Text)
              | k <- ["brand", "brandUrl", "brandLabelUrl", "brandLogoUrl", "brandService", "copyright", "misuse", "legal", "forgot", "support"]
              ]
        ]
          <> maybe [] (\m -> ["meeting" .= m]) mMeeting
