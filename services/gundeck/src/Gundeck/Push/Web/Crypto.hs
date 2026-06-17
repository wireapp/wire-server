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

-- | RFC 8291 (Message Encryption for Web Push) and RFC 8292 (VAPID — Voluntary
-- Application Server Identification) primitives, implemented with @crypton@.
--
-- This module is the correctness-critical crypto core of web push delivery. It
-- produces the @aes128gcm@ content-coded body that gundeck POSTs to a browser
-- push-service endpoint (RFC 8030), and signs the per-request VAPID JWT that
-- authenticates gundeck to the push service.
--
-- == Design: pure core + IO wrapper
--
-- The public 'encryptPayload' runs in 'IO' because it generates a fresh
-- ephemeral ECDH keypair and 16-byte salt per message (RFC 8291 §3.1). A
-- deterministic, fully-injectable pure core — 'encryptPayloadWith' — underpins
-- it so the RFC 8291 §5 known-answer test can reproduce the exact published
-- ciphertext from fixed test vectors.
module Gundeck.Push.Web.Crypto
  ( -- * RFC 8291: aes128gcm content encryption
    encryptPayload,
    encryptPayloadWith,
    EncryptedBody (..),

    -- * RFC 8292: VAPID JWT signing
    signVapid,
    VapidHeaders (..),

    -- * Injection points (for the RFC 8291 KAT)
    AsEphemeralKey (..),
    mkAsEphemeralKey,
    Salt (..),
    randomSalt,

    -- * Errors
    CryptoError (..),
  )
where

import Control.Lens ((^.))
import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (AEADMode (AEAD_GCM), aeadEncrypt, aeadFinalize, aeadInit, cipherInit)
import Crypto.ECC (Curve_P256R1)
import Crypto.Error (CryptoFailable (..), onCryptoFailure)
import Crypto.Hash (SHA256 (..))
import Crypto.KDF.HKDF qualified as HKDF
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import Crypto.PubKey.ECC.DH qualified as ECC.DH
import Crypto.PubKey.ECC.Prim qualified as ECC.Prim
import Crypto.PubKey.ECC.Types qualified as ECC.Types
import Crypto.PubKey.ECDSA qualified as ECDSA
import Crypto.Random (MonadRandom, getRandomBytes)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteArray (convert)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Lazy qualified as LBS
import Data.Proxy (Proxy (..))
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Gundeck.Env (VapidKeyPair)
import Gundeck.Env qualified as Env
import Imports
import Wire.API.Push.V2.WebSubscription
  ( AuthSecret (..),
    P256dhKey (..),
    WebPushKeys,
    wpkAuth,
    wpkP256dh,
  )

--------------------------------------------------------------------------------
-- Public types

-- | The 86-byte header + single encrypted record (ciphertext + 16-byte GCM tag)
-- produced by RFC 8291 aes128gcm content coding. This is the exact body POSTed
-- to the push-service endpoint with @Content-Encoding: aes128gcm@.
newtype EncryptedBody = EncryptedBody
  { encryptedBodyBytes :: ByteString
  }
  deriving stock (Eq, Show)

-- | A 16-byte random salt (RFC 8291 §3.4). Carried in the first 16 octets of
-- the aes128gcm header.
newtype Salt = Salt
  { saltBytes :: ByteString
  }
  deriving stock (Eq, Show)

-- | The application server's ephemeral ECDH P-256 keypair for a single push
-- message (RFC 8291 §3.1). In production 'encryptPayload' generates a fresh one
-- per message; for the RFC 8291 KAT it is injected from the published test
-- vector via 'encryptPayloadWith'.
data AsEphemeralKey = AsEphemeralKey
  { -- | The 32-byte raw P-256 scalar (big-endian), base64url in RFC §5.
    asekPrivateScalar :: !ByteString,
    -- | The 65-byte uncompressed public point (@0x04 || X || Y@).
    asekPublicBytes :: !ByteString
  }
  deriving stock (Eq, Show)

-- | The two HTTP header values RFC 8292 requires on a web push request:
--
-- @Authorization: vapid t=<jwt>,k=<kid>@
-- @Crypto-Key: p256ecdsa=<pub>@
data VapidHeaders = VapidHeaders
  { vhAuthorization :: !Text,
    vhCryptoKey :: !Text
  }
  deriving stock (Eq, Show)

-- | Failures surfaced by the pure crypto core. The IO wrappers re-throw these
-- as an 'ErrorCall' / are surfaced by callers; the shape is kept explicit so
-- the KAT can assert on the specific failure mode (e.g. curve rejection).
data CryptoError
  = -- | The recipient @p256dh@ key was not the expected 65 bytes.
    CryptoInvalidP256dhLength !Int
  | -- | The @p256dh@ is 65 bytes but lacks the @0x04@ uncompressed-format
    -- prefix (e.g. a compressed point or garbage).
    CryptoInvalidP256dhFormat
  | -- | The @p256dh@ point does not satisfy the P-256 curve equation
    -- (invalid-curve defense, RFC 8291 §7).
    CryptoPointNotOnCurve
  | -- | The @p256dh@ point is the point at infinity.
    CryptoPointAtInfinity
  | -- | Plaintext exceeds the single-record limit (RFC 8291 §4).
    CryptoPlaintextTooLarge !Int
  | -- | The derived CEK could not initialise an AES-128 cipher.
    CryptoCipherInitFailed
  | -- | AES-128-GCM AEAD initialisation failed.
    CryptoAeadInitFailed
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Constants (RFC 8291 §3.4, §4)

-- | P-256 curve in crypton's value-level ECC API.
p256Curve :: ECC.Types.Curve
p256Curve = ECC.Types.getCurveByName ECC.Types.SEC_p256r1

-- | Record size (@rs@) in the aes128gcm header. RFC 8291 §4 mandates a single
-- record; @rs@ must exceed plaintext + 1 (delimiter) + 16 (tag). 4096 is the
-- conventional value and the one all browser push services expect.
recordSize :: Integer
recordSize = 4096

-- | Maximum plaintext for a single-record message (RFC 8291 §4): the push
-- service budget of 4096 octets minus the 86-byte header, 1 padding delimiter
-- octet, and the 16-octet AEAD tag.
maxPlaintextLength :: Int
maxPlaintextLength = 3993

--------------------------------------------------------------------------------
-- RFC 8291: encryption

-- | Encrypt a web push payload (RFC 8291), generating a fresh ephemeral ECDH
-- keypair and 16-byte salt. This is what dispatch calls per message.
encryptPayload ::
  (MonadRandom m) =>
  WebPushKeys ->
  ByteString ->
  m (Either CryptoError EncryptedBody)
encryptPayload keys plaintext = do
  scalar <- ECC.DH.generatePrivate p256Curve
  let asKey = mkAsEphemeralKey scalar
  salt <- randomSalt
  pure $! encryptPayloadWith keys asKey salt plaintext

-- | Deterministic, fully-injectable pure core of RFC 8291 encryption. Follows
-- RFC 8291 §3.4 pseudocode verbatim. This is what the RFC 8291 §5 known-answer
-- test exercises with the published test vectors.
encryptPayloadWith ::
  WebPushKeys ->
  AsEphemeralKey ->
  Salt ->
  ByteString ->
  Either CryptoError EncryptedBody
encryptPayloadWith keys asKey (Salt salt) plaintext
  | BS.length plaintext > maxPlaintextLength =
      Left (CryptoPlaintextTooLarge (BS.length plaintext))
  | otherwise = do
      let P256dhKey uaPubBs = keys ^. wpkP256dh
          AuthSecret authBs = keys ^. wpkAuth
      uaPoint <- decodeValidatedP256Point uaPubBs
      let asPubBs = asekPublicBytes asKey
          ephemScalar = os2ip (asekPrivateScalar asKey)
          -- ECDH shared secret (RFC 8291 §3.1): ECDH(as_private, ua_public).
          shared = ECC.DH.getShared p256Curve ephemScalar uaPoint
          -- HKDF to combine ECDH + auth secrets (RFC 8291 §3.3-3.4).
          prkKey = HKDF.extract @SHA256 authBs shared
          -- key_info = "WebPush: info" || 0x00 || ua_public || as_public.
          -- HKDF.expand appends the 0x01 counter byte internally.
          keyInfo = "WebPush: info" <> BS.singleton 0 <> uaPubBs <> asPubBs
          ikm :: ByteString
          ikm = HKDF.expand @SHA256 prkKey keyInfo 32
          -- RFC 8188 CEK/nonce derivation (HKDF with the random salt).
          prk = HKDF.extract @SHA256 salt ikm
          cekInfo = "Content-Encoding: aes128gcm" <> BS.singleton 0
          nonceInfo = "Content-Encoding: nonce" <> BS.singleton 0
          cek :: ByteString
          cek = HKDF.expand @SHA256 prk cekInfo 16
          nonceBs :: ByteString
          nonceBs = HKDF.expand @SHA256 prk nonceInfo 12
      -- AES-128-GCM over (plaintext || 0x02); single record, seq=0, so the
      -- nonce is used as-is (RFC 8291 §3.4 final note).
      ctTag <- aesGcmEncrypt cek nonceBs (plaintext <> BS.singleton 0x02)
      let header = salt <> rsBytes <> BS.singleton 65 <> asPubBs
      pure $! EncryptedBody (header <> ctTag)
  where
    -- rs (record size) as 4 big-endian octets.
    rsBytes :: ByteString
    rsBytes = i2ospOf_ 4 recordSize

-- | Decode a 65-byte uncompressed P-256 point (@0x04 || X || Y@) and run the
-- three validation steps from RFC 8291 §7 / X9.62 §4.3.7: reject the point at
-- infinity, check coordinate ranges implicitly via curve-equation membership.
decodeValidatedP256Point :: ByteString -> Either CryptoError ECC.Types.Point
decodeValidatedP256Point bs
  | BS.length bs /= 65 = Left (CryptoInvalidP256dhLength (BS.length bs))
  | BS.index bs 0 /= 0x04 = Left CryptoInvalidP256dhFormat
  | otherwise =
      let x = os2ip (BS.take 32 (BS.drop 1 bs))
          y = os2ip (BS.drop 33 bs)
          pt = ECC.Types.Point x y
       in if ECC.Prim.isPointAtInfinity pt
            then Left CryptoPointAtInfinity
            else
              if ECC.Prim.isPointValid p256Curve pt
                then Right pt
                else Left CryptoPointNotOnCurve

-- | AES-128-GCM encrypt with no associated data, returning ciphertext || tag.
aesGcmEncrypt ::
  ByteString ->
  ByteString ->
  ByteString ->
  Either CryptoError ByteString
aesGcmEncrypt cek nonce msg =
  onCryptoFailure
    (const (Left CryptoCipherInitFailed))
    ( \cipher ->
        onCryptoFailure
          (const (Left CryptoAeadInitFailed))
          ( \aead ->
              let (ct, aead') = aeadEncrypt aead msg
                  tag = convert (aeadFinalize aead' 16) :: ByteString
               in Right (ct <> tag)
          )
          (aeadInit AEAD_GCM cipher nonce)
    )
    (cipherInit cek :: CryptoFailable AES128)

--------------------------------------------------------------------------------
-- Ephemeral key construction

-- | Build an 'AsEphemeralKey' from a generated private scalar, deriving the
-- public point via scalar base-point multiplication.
mkAsEphemeralKey :: Integer -> AsEphemeralKey
mkAsEphemeralKey scalar =
  let pubPoint = ECC.Prim.pointBaseMul p256Curve scalar
      pubBs = encodeUncompressedPoint pubPoint
      scalarBs = i2ospOf_ 32 scalar
   in AsEphemeralKey scalarBs pubBs

-- | Serialise a P-256 point as the 65-byte uncompressed form @0x04 || X || Y@.
encodeUncompressedPoint :: ECC.Types.Point -> ByteString
encodeUncompressedPoint pt = case pt of
  ECC.Types.Point x y -> BS.singleton 0x04 <> i2ospOf_ 32 x <> i2ospOf_ 32 y
  ECC.Types.PointO -> BS.empty

-- | Generate a fresh 16-byte salt (RFC 8291 §3.4) from the system CSPRNG.
randomSalt :: (MonadRandom m) => m Salt
randomSalt = Salt <$> getRandomBytes 16

--------------------------------------------------------------------------------
-- RFC 8292: VAPID signing

-- | Produce the VAPID 'VapidHeaders' for a web push request (RFC 8292 §2-3):
-- an ES256-signed JWT carrying @aud@, @exp@ (now + 12h), @sub@, plus the
-- server's static public key. The private key comes from 'VapidKeyPair';
-- the public key is already base64url-encoded in '_vkpPublicB64'.
signVapid ::
  VapidKeyPair ->
  -- | @sub@ject: a @mailto:@ or @https:@ URL identifying the operator.
  Text ->
  -- | @aud@ience: the push-service endpoint origin.
  Text ->
  IO VapidHeaders
signVapid kp subject audience = do
  now <- getPOSIXTime
  let expiry :: Integer
      expiry = round now + (12 * 60 * 60)
      headerJson =
        Aeson.object
          [ "alg" .= ("ES256" :: Text),
            "typ" .= ("JWT" :: Text)
          ]
      payloadJson =
        Aeson.object
          [ "aud" .= audience,
            "exp" .= expiry,
            "sub" .= subject
          ]
      headerBs = LBS.toStrict (Aeson.encode headerJson)
      payloadBs = LBS.toStrict (Aeson.encode payloadJson)
      signingInput = b64url headerBs <> "." <> b64url payloadBs
  sig <- ECDSA.sign (Proxy @Curve_P256R1) (kp ^. Env.vkpPrivate) SHA256 signingInput
  let (r, s) = ECDSA.signatureToIntegers (Proxy @Curve_P256R1) sig
      -- JOSE ES256 requires the raw R||S (64 bytes), NOT DER.
      sigBs = i2ospOf_ 32 r <> i2ospOf_ 32 s
      jwt = signingInput <> "." <> b64url sigBs
      pubB64 = kp ^. Env.vkpPublicB64
   in pure
        VapidHeaders
          { vhAuthorization = "vapid t=" <> decodeUtf8 jwt <> ",k=" <> pubB64,
            vhCryptoKey = "p256ecdsa=" <> pubB64
          }

b64url :: ByteString -> ByteString
b64url = B64U.encodeUnpadded
