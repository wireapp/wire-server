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

-- | Tests for "Gundeck.Push.Web.Crypto" (RFC 8291 encryption + RFC 8292 VAPID).
--
-- The centrepiece is the RFC 8291 §5 known-answer test: with the published
-- application-server/user-agent keypair, salt, auth secret and plaintext, the
-- implementation MUST reproduce the exact 144-byte aes128gcm body from the RFC.
module WebPushCrypto
  ( tests,
  )
where

import Control.Lens ((^.))
import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (AEADMode (AEAD_GCM), AuthTag, aeadDecrypt, aeadFinalize, aeadInit, cipherInit)
import Crypto.ECC (Curve_P256R1)
import Crypto.Error (CryptoFailable (..))
import Crypto.Hash (SHA256 (..))
import Crypto.KDF.HKDF qualified as HKDF
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import Crypto.PubKey.ECC.DH qualified as ECC.DH
import Crypto.PubKey.ECC.Prim qualified as ECC.Prim
import Crypto.PubKey.ECC.Types qualified as ECC.Types
import Crypto.PubKey.ECDSA qualified as ECDSA
import Crypto.Random (getRandomBytes)
import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteArray (convert)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Lazy qualified as LBS
import Data.Proxy (Proxy (..))
import Data.Text.Encoding (encodeUtf8)
import Gundeck.Env (parseVapidKeyPair)
import Gundeck.Env qualified as Env
import Gundeck.Push.Web.Crypto
  ( AsEphemeralKey (..),
    CryptoError (..),
    EncryptedBody (..),
    Salt (..),
    VapidHeaders (..),
    encryptPayload,
    encryptPayloadWith,
    signVapid,
  )
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Push.V2.WebSubscription
  ( P256dhKey (..),
    WebPushKeys (..),
    mkAuthSecret,
    mkP256dhKey,
  )

--------------------------------------------------------------------------------
-- P-256 curve (value-level), used by the reference receiver in this test.

p256Curve :: ECC.Types.Curve
p256Curve = ECC.Types.getCurveByName ECC.Types.SEC_p256r1

tests :: TestTree
tests =
  testGroup
    "WebPushCrypto"
    [ testGroup "RFC 8291" rfc8291Tests,
      testGroup "RFC 8292 VAPID" vapidTests,
      testGroup "p256dh validation" p256dhValidationTests
    ]

--------------------------------------------------------------------------------
-- RFC 8291 §5 / Appendix A test vectors (public, non-secret).

rfc8291Plaintext :: ByteString
rfc8291Plaintext = "When I grow up, I want to be a watermelon"

rfc8291UaPublicB64 :: ByteString
rfc8291UaPublicB64 = "BCVxsr7N_eNgVRqvHtD0zTZsEc6-VV-JvLexhqUzORcxaOzi6-AYWXvTBHm4bjyPjs7Vd8pZGH6SRpkNtoIAiw4"

rfc8291UaPrivateB64 :: ByteString
rfc8291UaPrivateB64 = "q1dXpw3UpT5VOmu_cf_v6ih07Aems3njxI-JWgLcM94"

rfc8291AsPrivateB64 :: ByteString
rfc8291AsPrivateB64 = "yfWPiYE-n46HLnH0KqZOF1fJJU3MYrct3AELtAQ-oRw"

rfc8291AsPublicB64 :: ByteString
rfc8291AsPublicB64 = "BP4z9KsN6nGRTbVYI_c7VJSPQTBtkgcy27mlmlMoZIIgDll6e3vCYLocInmYWAmS6TlzAC8wEqKK6PBru3jl7A8"

rfc8291AuthSecretB64 :: ByteString
rfc8291AuthSecretB64 = "BTBZMqHH6r4Tts7J_aSIgg"

rfc8291SaltB64 :: ByteString
rfc8291SaltB64 = "DGv6ra1nlYgDCS1FRnbzlw"

-- The complete 144-byte aes128gcm body (86 header + 58 ciphertext+tag) from
-- RFC 8291 §5, base64url without padding.
rfc8291ExpectedBodyB64 :: ByteString
rfc8291ExpectedBodyB64 =
  "DGv6ra1nlYgDCS1FRnbzlwAAEABBBP4z9KsN6nGRTbVYI_c7VJSPQTBtkgcy27mlmlMoZIIgD\
  \ll6e3vCYLocInmYWAmS6TlzAC8wEqKK6PBru3jl7A_yl95bQpu6cVPTpK4Mqgkf1CXztLVBSt\
  \2Ks3oZwbuwXPXLWyouBWLVWGNWQexSgSxsj_Qulcy4a-fN"

rfc8291Keys :: WebPushKeys
rfc8291Keys =
  let p256dh = unsafeRight (mkP256dhKey (b64 rfc8291UaPublicB64))
      auth = unsafeRight (mkAuthSecret (b64 rfc8291AuthSecretB64))
   in WebPushKeys p256dh auth

rfc8291Tests :: [TestTree]
rfc8291Tests =
  [ testCase "§5 known-answer: reproduces the exact published ciphertext" $ do
      let asKey = AsEphemeralKey (b64 rfc8291AsPrivateB64) (b64 rfc8291AsPublicB64)
          salt = Salt (b64 rfc8291SaltB64)
          expected = b64 rfc8291ExpectedBodyB64
      case encryptPayloadWith rfc8291Keys asKey salt rfc8291Plaintext of
        Left err -> assertFailure ("expected encryption to succeed, got: " <> show err)
        Right (EncryptedBody body) -> body @?= expected,
    testCase "§5 roundtrip: reference receiver recovers the plaintext" $ do
      let asKey = AsEphemeralKey (b64 rfc8291AsPrivateB64) (b64 rfc8291AsPublicB64)
          salt = Salt (b64 rfc8291SaltB64)
          uaPriv = os2ip (b64 rfc8291UaPrivateB64)
          authSecret = b64 rfc8291AuthSecretB64
      case encryptPayloadWith rfc8291Keys asKey salt rfc8291Plaintext of
        Left err -> assertFailure ("encrypt failed: " <> show err)
        Right (EncryptedBody body) ->
          case referenceDecrypt authSecret uaPriv body of
            Left e -> assertFailure ("decrypt failed: " <> e)
            Right recovered -> recovered @?= rfc8291Plaintext,
    testCase "fresh-key roundtrip: encrypt then decrypt with random keys" $ do
      uaPriv <- ECC.DH.generatePrivate p256Curve
      let uaPub = encodeUncompressed (ECC.Prim.pointBaseMul p256Curve uaPriv)
      Right p256dh <- pure (mkP256dhKey uaPub)
      authSecret <- getRandomBytes 16
      Right auth <- pure (mkAuthSecret authSecret)
      ephemScalar <- ECC.DH.generatePrivate p256Curve
      let asPub = encodeUncompressed (ECC.Prim.pointBaseMul p256Curve ephemScalar)
          asKey = AsEphemeralKey (i2ospOf_ 32 ephemScalar) asPub
      saltBs <- getRandomBytes 16
      let salt = Salt saltBs
          payload = "hello, web push"
      case encryptPayloadWith (WebPushKeys p256dh auth) asKey salt payload of
        Left err -> assertFailure ("encrypt failed: " <> show err)
        Right (EncryptedBody body) ->
          case referenceDecrypt authSecret uaPriv body of
            Left e -> assertFailure ("decrypt failed: " <> e)
            Right recovered -> recovered @?= payload,
    -- I1: exercises the production IO entry point (encryptPayload) and
    -- mkAsEphemeralKey (pointBaseMul + encodeUncompressedPoint), which the
    -- injectable-core tests above bypass. A regression in the ephemeral-key
    -- derivation would silently break browser decryption.
    testCase "encryptPayload (IO wrapper) roundtrips" $ do
      uaPriv <- ECC.DH.generatePrivate p256Curve
      let uaPub = encodeUncompressed (ECC.Prim.pointBaseMul p256Curve uaPriv)
      Right p256dh <- pure (mkP256dhKey uaPub)
      authSecret <- getRandomBytes 16
      Right auth <- pure (mkAuthSecret authSecret)
      let payload = "hello via IO wrapper"
      res <- encryptPayload (WebPushKeys p256dh auth) payload
      case res of
        Left err -> assertFailure ("encryptPayload failed: " <> show err)
        Right (EncryptedBody body) ->
          case referenceDecrypt authSecret uaPriv body of
            Left e -> assertFailure ("decrypt failed: " <> e)
            Right recovered -> recovered @?= payload
  ]

--------------------------------------------------------------------------------
-- Reference receiver: an independent re-implementation of the RFC 8291 decrypt
-- side, as a browser push service would do. Used only for roundtrip tests.

referenceDecrypt :: ByteString -> Integer -> ByteString -> Either String ByteString
referenceDecrypt authSecret uaPriv body
  | BS.length body < 86 = Left "body shorter than 86-byte header"
  | otherwise =
      let (header, ctTag) = BS.splitAt 86 body
          salt = BS.take 16 header
          -- header: salt(16) || rs(4) || idlen(1) || as_public(65)
          asPub = BS.drop 21 header
          asPoint = decodePoint asPub
          uaPub = encodeUncompressed (ECC.Prim.pointBaseMul p256Curve uaPriv)
          shared = ECC.DH.getShared p256Curve uaPriv asPoint
          (ct, tag) = BS.splitAt (BS.length ctTag - 16) ctTag
          prkKey = HKDF.extract @SHA256 authSecret shared
          keyInfo = "WebPush: info" <> BS.singleton 0 <> uaPub <> asPub
          ikm :: ByteString
          ikm = HKDF.expand @SHA256 prkKey keyInfo 32
          prk = HKDF.extract @SHA256 salt ikm
          cek :: ByteString
          cek = HKDF.expand @SHA256 prk cekInfo 16
          nonce :: ByteString
          nonce = HKDF.expand @SHA256 prk nonceInfo 12
       in case aesGcmDecrypt cek nonce ct tag of
            Just plain -> Right (BS.init plain)
            Nothing -> Left "AES-GCM authentication tag mismatch"
  where
    decodePoint bs =
      let x = os2ip (BS.take 32 (BS.drop 1 bs))
          y = os2ip (BS.drop 33 bs)
       in ECC.Types.Point x y

cekInfo :: ByteString
cekInfo = "Content-Encoding: aes128gcm" <> BS.singleton 0

nonceInfo :: ByteString
nonceInfo = "Content-Encoding: nonce" <> BS.singleton 0

aesGcmDecrypt :: ByteString -> ByteString -> ByteString -> ByteString -> Maybe ByteString
aesGcmDecrypt cek nonce ct tagBs = case (cipherInit cek :: CryptoFailable AES128) of
  CryptoFailed _ -> Nothing
  CryptoPassed cipher -> case aeadInit AEAD_GCM cipher nonce of
    CryptoFailed _ -> Nothing
    CryptoPassed aead ->
      let (pt, aead') = aeadDecrypt aead ct
          computed :: AuthTag
          computed = aeadFinalize aead' 16
       in if (convert computed :: ByteString) == tagBs then Just pt else Nothing

--------------------------------------------------------------------------------
-- RFC 8292 VAPID tests

vapidTests :: [TestTree]
vapidTests =
  [ testCase "signVapid: JWT signature verifies against the derived public key" $ do
      let kp = unsafeRight (parseVapidKeyPair rfc8291AsPrivateKeyText)
          subject = "mailto:ops@wire.com"
          audience = "https://fcm.googleapis.com"
      VapidHeaders {vhAuthorization} <- signVapid kp subject audience
      let authBs = encodeUtf8 vhAuthorization
      assertBool "authorization has 'vapid t=' prefix" ("vapid t=" `BS.isPrefixOf` authBs)
      let jwt = extractJwt vhAuthorization
          (headerB64, payloadB64, sigB64) = splitDot3 jwt
          signingInput = headerB64 <> "." <> payloadB64
      sigBs <- eitherFail (B64U.decodeUnpadded sigB64)
      let (rBs, sBs) = BS.splitAt 32 sigBs
      sig <- cryptoFail (ECDSA.signatureFromIntegers (Proxy @Curve_P256R1) (os2ip rBs, os2ip sBs))
      assertBool "signature verifies against vkpPublic" $
        ECDSA.verify (Proxy @Curve_P256R1) SHA256 (kp ^. Env.vkpPublic) sig signingInput,
    -- I2: assert the JWT carries the RFC 8292 §3-mandated claims (aud, sub,
    -- exp) with correct values, and the ES256 header. A regression dropping
    -- @aud@ would pass signature verification but be rejected by push services.
    testCase "signVapid: JWT carries aud/sub/exp claims and ES256 header" $ do
      let kp = unsafeRight (parseVapidKeyPair rfc8291AsPrivateKeyText)
          subject = "mailto:ops@wire.com"
          audience = "https://fcm.googleapis.com"
      VapidHeaders {vhAuthorization} <- signVapid kp subject audience
      let jwt = extractJwt vhAuthorization
          (headerB64, payloadB64, _sigB64) = splitDot3 jwt
      -- Header: {"alg":"ES256","typ":"JWT"}
      hdrBs <- eitherFail (B64U.decodeUnpadded headerB64)
      hdr <- decodeObject hdrBs
      KeyMap.lookup (Key.fromText "alg") hdr @?= Just (String "ES256")
      KeyMap.lookup (Key.fromText "typ") hdr @?= Just (String "JWT")
      -- Payload: aud, sub, exp
      payBs <- eitherFail (B64U.decodeUnpadded payloadB64)
      claims <- decodeObject payBs
      KeyMap.lookup (Key.fromText "aud") claims @?= Just (String audience)
      KeyMap.lookup (Key.fromText "sub") claims @?= Just (String subject)
      assertBool "exp claim present" (isJust (KeyMap.lookup (Key.fromText "exp") claims)),
    testCase "signVapid: Crypto-Key header carries the server public key" $ do
      let kp = unsafeRight (parseVapidKeyPair rfc8291AsPrivateKeyText)
      VapidHeaders {vhCryptoKey} <- signVapid kp "mailto:ops@wire.com" "https://example.com"
      encodeUtf8 vhCryptoKey @?= ("p256ecdsa=" <> encodeUtf8 (kp ^. Env.vkpPublicB64))
  ]

rfc8291AsPrivateKeyText :: Text
rfc8291AsPrivateKeyText = "yfWPiYE-n46HLnH0KqZOF1fJJU3MYrct3AELtAQ-oRw"

extractJwt :: Text -> ByteString
extractJwt auth =
  let rest = BS.drop (BS.length "vapid t=") (encodeUtf8 auth)
   in -- rest = "<jwt>,k=<kid>"; take up to the comma.
      BS.takeWhile (/= 0x2c) rest

splitDot3 :: ByteString -> (ByteString, ByteString, ByteString)
splitDot3 s =
  let (a, r1) = bsBreak (== 0x2e) s
      (b, r2) = bsBreak (== 0x2e) r1
   in (a, b, r2)
  where
    bsBreak p xs = (BS.takeWhile (not . p) xs, BS.drop 1 (BS.dropWhile (not . p) xs))

--------------------------------------------------------------------------------
-- p256dh validation tests

p256dhValidationTests :: [TestTree]
p256dhValidationTests =
  [ testCase "rejects a 64-byte p256dh (wrong length)" $ do
      let asKey = AsEphemeralKey (b64 rfc8291AsPrivateB64) (b64 rfc8291AsPublicB64)
          salt = Salt (b64 rfc8291SaltB64)
          -- Bypass mkP256dhKey (which enforces 65 bytes) to feed the raw guard.
          tooShort = P256dhKey (BS.init (b64 rfc8291UaPublicB64))
          auth = unsafeRight (mkAuthSecret (b64 rfc8291AuthSecretB64))
      case encryptPayloadWith (WebPushKeys tooShort auth) asKey salt rfc8291Plaintext of
        Left (CryptoInvalidP256dhLength 64) -> pure ()
        other -> assertFailure ("expected CryptoInvalidP256dhLength 64, got: " <> show other),
    testCase "rejects a 65-byte p256dh with wrong format byte (not 0x04)" $ do
      let asKey = AsEphemeralKey (b64 rfc8291AsPrivateB64) (b64 rfc8291AsPublicB64)
          salt = Salt (b64 rfc8291SaltB64)
          -- 65 bytes, but first byte 0x02 (compressed form) instead of 0x04.
          badFormat = BS.singleton 0x02 <> BS.drop 1 (b64 rfc8291UaPublicB64)
          auth = unsafeRight (mkAuthSecret (b64 rfc8291AuthSecretB64))
          p256dh = P256dhKey badFormat
      case encryptPayloadWith (WebPushKeys p256dh auth) asKey salt rfc8291Plaintext of
        Left CryptoInvalidP256dhFormat -> pure ()
        other -> assertFailure ("expected CryptoInvalidP256dhFormat, got: " <> show other),
    testCase "rejects a point not on the P-256 curve" $ do
      let asKey = AsEphemeralKey (b64 rfc8291AsPrivateB64) (b64 rfc8291AsPublicB64)
          salt = Salt (b64 rfc8291SaltB64)
          -- (0x04 || X=1 || Y=1): valid 65-byte length, not on the curve.
          offCurve = BS.singleton 0x04 <> pad32 1 <> pad32 1
          auth = unsafeRight (mkAuthSecret (b64 rfc8291AuthSecretB64))
          p256dh = P256dhKey offCurve
      case encryptPayloadWith (WebPushKeys p256dh auth) asKey salt rfc8291Plaintext of
        Left CryptoPointNotOnCurve -> pure ()
        Left e -> assertFailure ("expected CryptoPointNotOnCurve, got: " <> show e)
        Right _ -> assertFailure "expected CryptoPointNotOnCurve, but encryption succeeded",
    testCase "rejects plaintext larger than 3993 bytes (RFC 8291 §4)" $ do
      let asKey = AsEphemeralKey (b64 rfc8291AsPrivateB64) (b64 rfc8291AsPublicB64)
          salt = Salt (b64 rfc8291SaltB64)
          oversized = BS.replicate 3994 0x41
      case encryptPayloadWith rfc8291Keys asKey salt oversized of
        Left (CryptoPlaintextTooLarge 3994) -> pure ()
        other -> assertFailure ("expected CryptoPlaintextTooLarge 3994, got: " <> show other)
  ]

--------------------------------------------------------------------------------
-- Small helpers

pad32 :: Integer -> ByteString
pad32 = i2ospOf_ 32

encodeUncompressed :: ECC.Types.Point -> ByteString
encodeUncompressed = \case
  ECC.Types.Point x y -> BS.singleton 0x04 <> i2ospOf_ 32 x <> i2ospOf_ 32 y
  ECC.Types.PointO -> BS.empty

b64 :: ByteString -> ByteString
b64 t = case B64U.decodeUnpadded t of
  Right bs -> bs
  Left e -> error ("bad base64url in test vector: " <> e)

-- | Unwrap a known-good 'Right' from a test vector; crashes on 'Left' (which
-- would indicate a malformed test vector, not a code failure).
unsafeRight :: Either String a -> a
unsafeRight (Right a) = a
unsafeRight (Left e) = error ("test vector invariant violated: " <> e)

eitherFail :: Either String a -> IO a
eitherFail = either (assertFailure . ("decode failed: " <>)) pure

-- | Decode a JSON bytestring that is expected to be a flat object; fails the
-- test case if decoding yields anything other than a JSON object.
decodeObject :: ByteString -> IO (KeyMap.KeyMap Value)
decodeObject bs = case Aeson.decode (LBS.fromStrict bs) of
  Just (Object o) -> pure o
  _ -> assertFailure "expected JSON object"

cryptoFail :: CryptoFailable a -> IO a
cryptoFail (CryptoFailed e) = assertFailure ("crypto-failable failed: " <> show e)
cryptoFail (CryptoPassed a) = pure a
