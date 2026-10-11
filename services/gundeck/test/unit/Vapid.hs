-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Vapid where

import Control.Lens ((^.))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as Text
import Gundeck.Env
import Imports
import Test.QuickCheck ((===))
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Wire.API.Push.V2.WebSubscription (VapidPublicKeyResponse (..))

tests :: TestTree
tests =
  testGroup
    "Vapid"
    [ testGroup "parseVapidKeyPair" parseVapidKeyPairTests,
      testGroup "validateVapidSubject" validateVapidSubjectTests,
      testGroup "mkVapidKeyPair" mkVapidKeyPairTests,
      testGroup "VapidPublicKeyResponse" vapidPublicKeyResponseTests
    ]

--------------------------------------------------------------------------------
-- Known-answer test vector: RFC 8291 Appendix A application-server keypair.
-- Reusing it here verifies the entire chain (base64url decode -> scalar
-- range validation -> Q = d*G derivation -> uncompressed point encode) against
-- published, independently-generated values.
--------------------------------------------------------------------------------

-- | The application-server private key from RFC 8291 §5 (base64url, 32 bytes).
rfc8291AsPrivateKey :: Text
rfc8291AsPrivateKey = "yfWPiYE-n46HLnH0KqZOF1fJJU3MYrct3AELtAQ-oRw"

-- | The corresponding public key from RFC 8291 §5 (base64url, 65-byte
-- uncompressed point: 0x04 || X || Y). Parsing the private key above MUST
-- derive exactly this value.
rfc8291AsPublicKey :: Text
rfc8291AsPublicKey = "BP4z9KsN6nGRTbVYI_c7VJSPQTBtkgcy27mlmlMoZIIgDll6e3vCYLocInmYWAmS6TlzAC8wEqKK6PBru3jl7A8"

parseVapidKeyPairTests :: [TestTree]
parseVapidKeyPairTests =
  [ testCase "RFC 8291 Appendix A: derives the expected public key" $ do
      case parseVapidKeyPair rfc8291AsPrivateKey of
        Left e ->
          assertFailure ("expected valid keypair, got: " <> e)
        Right kp ->
          (kp ^. vkpPublicB64) @?= rfc8291AsPublicKey,
    testCase "rejects non-base64url input" $
      assertBool "expected Left" $
        isLeft (parseVapidKeyPair "not valid base64url!!!"),
    testCase "rejects wrong byte length (16 bytes)" $
      assertBool "expected Left" $
        isLeft (parseVapidKeyPair "0123456789abcdef"),
    testCase "rejects the zero scalar (out of range)" $
      -- 32 zero bytes, base64url-encoded (no padding).
      assertBool "expected Left" $
        isLeft (parseVapidKeyPair "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"),
    testCase "rejects the all-0xFF scalar (>= n, out of range)" $
      assertBool "expected Left" $
        isLeft (parseVapidKeyPair "________________________________________8")
  ]

validateVapidSubjectTests :: [TestTree]
validateVapidSubjectTests =
  [ testCase "accepts mailto: subject" $
      validateVapidSubject "mailto:ops@wire.com" @?= Right (),
    testCase "accepts https: subject" $
      validateVapidSubject "https://wire.com" @?= Right (),
    testCase "rejects http: subject (must be https)" $
      assertBool "expected Left" $
        isLeft (validateVapidSubject "http://wire.com"),
    testCase "rejects ftp: subject" $
      assertBool "expected Left" $
        isLeft (validateVapidSubject "ftp://example.com"),
    testCase "rejects a bare string" $
      assertBool "expected Left" $
        isLeft (validateVapidSubject "not a url")
  ]

mkVapidKeyPairTests :: [TestTree]
mkVapidKeyPairTests =
  [ testCase "succeeds with valid subject and key" $ do
      case mkVapidKeyPair "mailto:ops@wire.com" rfc8291AsPrivateKey of
        Left e -> assertFailure ("expected valid keypair, got: " <> e)
        Right kp -> (kp ^. vkpPublicB64) @?= rfc8291AsPublicKey,
    testCase "fails when subject is invalid even if key is valid" $
      assertBool "expected Left" $
        isLeft (mkVapidKeyPair "not-a-url" rfc8291AsPrivateKey),
    testCase "fails when key is invalid even if subject is valid" $
      assertBool "expected Left" $
        isLeft (mkVapidKeyPair "mailto:ops@wire.com" "too-short")
  ]

--------------------------------------------------------------------------------
-- VapidPublicKeyResponse: wire format for GET /push/web/vapid-public-key.

vapidPublicKeyResponseTests :: [TestTree]
vapidPublicKeyResponseTests =
  [ testCase "encodes as {\"key\": ...} with the b64url value" $ do
      let resp = VapidPublicKeyResponse rfc8291AsPublicKey
          encoded = Aeson.encode resp
          expected =
            LBS8.pack $
              "{\"key\":\"" <> Text.unpack rfc8291AsPublicKey <> "\"}"
      encoded @?= expected,
    testCase "decodes back from {\"key\": ...}" $ do
      let blob = Aeson.encode (VapidPublicKeyResponse rfc8291AsPublicKey)
      Aeson.decode blob @?= Just (VapidPublicKeyResponse rfc8291AsPublicKey),
    testCase "end-to-end: derived public key round-trips through the response" $
      case parseVapidKeyPair rfc8291AsPrivateKey of
        Left e ->
          assertFailure
            ("parseVapidKeyPair failed on RFC 8291 fixture: " <> e)
        Right kp ->
          -- This is exactly what 'Gundeck.Push.getVapidPublicKey' does once it
          -- has a 'Just' keypair from 'view vapid'.
          vapidPublicKeyResponseKey (VapidPublicKeyResponse (kp ^. vkpPublicB64))
            @?= rfc8291AsPublicKey,
    -- Property test exercising both the 'GenericUniform Arbitrary' instance
    -- and the deriving-via-'Schema' 'ToJSON'/'FromJSON' chain together: a
    -- field rename, a missing key, or a broken Arbitrary generator would all
    -- surface here.
    testProperty "arbitrary value roundtrips through JSON" $
      \(resp :: VapidPublicKeyResponse) ->
        Aeson.decode (Aeson.encode resp) === Just resp
  ]
