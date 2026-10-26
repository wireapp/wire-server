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

-- | The mock's main job is telling clients their request is wrong, so the
-- parsers are the part worth testing.
module Test.Wire.API.User.Auth.TOTP (tests) where

import Data.Aeson qualified as A
import Data.Text qualified as Text
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Wire.API.User.Auth.TOTP
-- for 'Arbitrary Text' (via Test.QuickCheck.Instances)
import Wire.Arbitrary ()

tests :: TestTree
tests =
  testGroup
    "TOTP"
    [ testGroup "TOTPSecret" secretTests,
      testGroup "TOTPCode" codeTests,
      testGroup "RecoveryCode" recoveryCodeTests,
      testGroup "SecondFactorCode" secondFactorTests,
      testGroup "otpauthURI" uriTests,
      testGroup "secrets do not leak through Show" redactionTests
    ]

--------------------------------------------------------------------------------
-- TOTPSecret

secretTests :: [TestTree]
secretTests =
  [ testCase "encodes unpadded" $
      -- "Hello!\222\173\190\239" is RFC 4648 s10's base32 test vector, which
      -- is 10 bytes and would be padded to 16 characters by a padding encoder.
      assertEqual
        "no '=' in the output"
        "JBSWY3DPEHPK3PXP"
        (totpSecretToBase32 (totpSecretFromBytes "Hello!\222\173\190\239")),
    testCase "accepts lower case" $
      assertEqual
        "lower case decodes to the same secret as upper case"
        (totpSecretFromBase32 "JBSWY3DPEHPK3PXP")
        (totpSecretFromBase32 "jbswy3dpehpk3pxp"),
    testCase "rejects padded input" $
      assertLeft "padded" (totpSecretFromBase32 "JBSWY3DPEHPK3PXP======"),
    testCase "rejects padding even where it would be redundant" $
      assertLeft "trailing '='" (totpSecretFromBase32 "JBSWY3DPEHPK3PXP="),
    testCase "rejects characters outside the base32 alphabet" $
      -- 0, 1 and 8 are not in the RFC 4648 base32 alphabet.
      assertLeft "malformed" (totpSecretFromBase32 "JBSWY3DP01PK3PX8"),
    testCase "rejects an impossible length" $
      -- a base32 string of length 1 (mod 8) encodes no whole number of bytes
      assertLeft "bad length" (totpSecretFromBase32 "JBSWY3DPE"),
    testCase "rejects the empty string" $
      assertLeft "empty" (totpSecretFromBase32 ""),
    testProperty "Arbitrary secrets survive their own parser" $
      \(s :: TOTPSecret) -> totpSecretFromBase32 (totpSecretToBase32 s) == Right s,
    testProperty "Arbitrary secrets need no padding" $
      \(s :: TOTPSecret) -> not (Text.any (== '=') (totpSecretToBase32 s))
  ]

--------------------------------------------------------------------------------
-- TOTPCode

codeTests :: [TestTree]
codeTests =
  [ testCase "accepts six digits, leading zeros included" . void $
      assertRight "000000" (totpCodeFromText "000000"),
    testCase "accepts nine digits" . void $
      assertRight "123456789" (totpCodeFromText "123456789"),
    testCase "rejects non-digits" $
      assertLeft "letters" (totpCodeFromText "12a456"),
    testCase "rejects non-ASCII digits" $
      assertLeft "arabic-indic digits" (totpCodeFromText "\1632\1632\1632\1632\1632\1632"),
    testCase "rejects a too short code" $
      assertLeft "five digits" (totpCodeFromText "12345"),
    testCase "rejects a too long code" $
      assertLeft "ten digits" (totpCodeFromText "1234567890"),
    testCase "rejects the empty string" $
      assertLeft "empty" (totpCodeFromText ""),
    testProperty "Arbitrary codes survive their own parser" $
      \(c :: TOTPCode) -> totpCodeFromText (unsafeRevealTOTPCode c) == Right c
  ]

--------------------------------------------------------------------------------
-- RecoveryCode

recoveryCodeTests :: [TestTree]
recoveryCodeTests =
  [ testCase "accepts the documented shape" . void $
      assertRight "well-formed" (recoveryCodeFromText "3MK7-9WQX-0V2H-8TZB"),
    testCase "accepts lower case and normalises it" $
      assertEqual
        "lower case is the same code as upper case"
        (recoveryCodeFromText "3MK7-9WQX-0V2H-8TZB")
        (recoveryCodeFromText "3mk7-9wqx-0v2h-8tzb"),
    testCase "rejects a missing group" $
      assertLeft "three groups" (recoveryCodeFromText "3MK7-9WQX-0V2H"),
    testCase "rejects a wrong group length" $
      assertLeft "five-character group" (recoveryCodeFromText "3MK79-9WQX-0V2H-8TZB"),
    testCase "rejects missing dashes" $
      assertLeft "undashed" (recoveryCodeFromText "3MK79WQX0V2H8TZB"),
    testCase "rejects letters Crockford leaves out" $
      for_ ["I", "L", "O", "U"] $ \c ->
        assertLeft
          ("non-Crockford letter " <> Text.unpack c)
          (recoveryCodeFromText ("3MK7-9WQX-0V2H-8TZ" <> c)),
    testProperty "Arbitrary recovery codes survive their own parser" $
      \(c :: RecoveryCode) -> recoveryCodeFromText (unsafeRevealRecoveryCode c) == Right c
  ]

--------------------------------------------------------------------------------
-- SecondFactorCode

secondFactorTests :: [TestTree]
secondFactorTests =
  [ testCase "the totp: prefix parses as a TOTP code" $
      case secondFactorCodeFromText "totp:014921" of
        Right (SFTotp _) -> pure ()
        other -> assertFailure ("expected SFTotp, got " <> show other),
    testCase "the recovery: prefix parses as a recovery code" $
      case secondFactorCodeFromText "recovery:3MK7-9WQX-0V2H-8TZB" of
        Right (SFRecovery _) -> pure ()
        other -> assertFailure ("expected SFRecovery, got " <> show other),
    testCase "an unknown prefix is rejected" $
      assertLeft "garbage" (secondFactorCodeFromText "garbage:014921"),
    testCase "a bare TOTP code (no prefix) is rejected" $
      assertLeft "bare" (secondFactorCodeFromText "014921"),
    testProperty "Arbitrary second factors survive the JSON boundary" $
      \(c :: SecondFactorCode) ->
        A.fromJSON (A.toJSON c) == A.Success c
  ]

--------------------------------------------------------------------------------
-- otpauthURI

uriTests :: [TestTree]
uriTests =
  [ testCase "has the right scheme and authority" $ do
      let uri = otpauthURI "alice" secret TOTPSHA1 6 30 "Wire"
      assertBool
        ("uri does not start with otpauth://totp/: " <> Text.unpack uri)
        ("otpauth://totp/" `Text.isPrefixOf` uri),
    testCase "secret parameter matches the input" $ do
      let uri = otpauthURI "alice" secret TOTPSHA1 6 30 "Wire"
      assertBool
        ("uri does not contain the secret: " <> Text.unpack uri)
        (Text.isInfixOf ("secret=" <> totpSecretToBase32 secret) uri),
    testCase "has all required query parameters" $ do
      let uri = otpauthURI "alice" secret TOTPSHA1 6 30 "Wire"
      for_ ["algorithm=SHA1", "digits=6", "period=30", "issuer=Wire"] $ \param ->
        assertBool
          (Text.unpack uri <> " is missing parameter " <> param)
          (Text.isInfixOf (Text.pack param) uri),
    testCase "percent-encodes the label" $ do
      let uri = otpauthURI "a@l/i#c e" secret TOTPSHA1 6 30 "Wire"
      assertBool
        ("uri does not contain the percent-encoded label: " <> Text.unpack uri)
        (Text.isInfixOf ":a%40l%2Fi%23c%20e?" uri),
    testCase "SHA256 and SHA512" $ do
      let uri256 = otpauthURI "alice" secret TOTPSHA256 8 45 "Issuer"
          uri512 = otpauthURI "alice" secret TOTPSHA512 7 60 "Issuer"
      assertBool "SHA256" (Text.isInfixOf "algorithm=SHA256" uri256)
      assertBool "SHA512" (Text.isInfixOf "algorithm=SHA512" uri512)
      assertBool "digits 8" (Text.isInfixOf "digits=8" uri256)
      assertBool "digits 7" (Text.isInfixOf "digits=7" uri512)
      assertBool "period 45" (Text.isInfixOf "period=45" uri256)
      assertBool "period 60" (Text.isInfixOf "period=60" uri512)
      assertBool "issuer" (Text.isInfixOf "issuer=Issuer" uri256)
  ]
  where
    secret :: TOTPSecret
    secret = totpSecretFromBytes "\191\159\222\173\190\239Hello!"

--------------------------------------------------------------------------------
-- redaction

-- | A secret that lands in a log line, a test failure or an exception message
-- is a secret that leaked.  These three types are the ones that carry
-- authentication material, so their 'Show' must not reproduce any of it.
redactionTests :: [TestTree]
redactionTests =
  [ testCase "TOTPSecret" $ do
      let s = totpSecretFromBytes "Hello!\222\173\190\239"
      assertNoSubstring (Text.unpack (totpSecretToBase32 s)) (show s)
      assertNoSubstring "Hello!" (show s),
    testCase "TOTPCode" $ do
      c <- assertRight "code" (totpCodeFromText "014921")
      assertNoSubstring "014921" (show c),
    testCase "RecoveryCode" $ do
      c <- assertRight "recovery code" (recoveryCodeFromText "3MK7-9WQX-0V2H-8TZB")
      assertNoSubstring "3MK7" (show c)
      assertNoSubstring "8TZB" (show c),
    testCase "SecondFactorCode" $ do
      c <- assertRight "second factor" (secondFactorCodeFromText "totp:014921")
      assertNoSubstring "014921" (show c),
    testCase "ReauthBody" $ do
      c <- assertRight "second factor" (secondFactorCodeFromText "recovery:3MK7-9WQX-0V2H-8TZB")
      -- a whole payload, because a derived 'Show' on a record walks into its
      -- fields: if one of them had a non-redacting 'Show', this is where it
      -- would show up.
      let b = ReauthBody {password = Nothing, totpCode = Just c}
      assertNoSubstring "3MK7" (show b)
  ]

--------------------------------------------------------------------------------
-- helpers

assertLeft :: (Show a) => String -> Either String a -> Assertion
assertLeft what = \case
  Left _ -> pure ()
  Right v -> assertFailure (what <> ": expected a parse error, got " <> show v)

assertRight :: String -> Either String a -> IO a
assertRight what = \case
  Right v -> pure v
  Left e -> assertFailure (what <> ": expected success, got " <> e)

assertNoSubstring :: String -> String -> Assertion
assertNoSubstring needle haystack =
  assertBool
    ("shown value leaks " <> show needle <> ": " <> haystack)
    (not (needle `isInfixOf` haystack))
