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

module Ssl.Util
  ( -- * Public Key Pinning
    verifyFingerprint,

    -- ** RSA-specific
    rsaFingerprint,
    verifyRsaFingerprint,

    -- * Cipher suites
    rsaCiphers,

    -- * Network
    withVerifiedSslConnection,
  )
where

import Control.Exception hiding (catch)
import Data.ByteString.Builder
import Data.Byteable (constEqBytes)
import Data.Dynamic (fromDynamic)
import Data.Time.Clock (getCurrentTime)
import Imports
import Network.HTTP.Client.Internal
import OpenSSL.BN (integerToMPI)
import OpenSSL.EVP.Digest (digestLBS)
import OpenSSL.EVP.Internal
import OpenSSL.EVP.PKey
import OpenSSL.EVP.Verify (VerifyStatus (..))
import OpenSSL.RSA
import OpenSSL.Session as SSL
import OpenSSL.X509 as X509

-- Cipher Suites ------------------------------------------------------------

-- | A small list of strong cipher suites for use with 'contextSetCiphers'
-- that includes only a selected subset of those based on RSA signatures over
-- ephemeral DH key exchanges (for perfect forward secrecy) and are thus
-- compatible with the RSA public key pinning implemented by the functions
-- 'rsaFingerprint' and 'verifyRsaFingerprint'.
--
-- As in TLS 1.3 [1], only AEAD cipher suites are included, specifically only
-- AES-GCM and CHACHA20-POLY1305. Thereby preference is applied as follows:
--
--  * Elliptic curve DH variants are preferred over "classic" finite
--    field variants for efficiency.
--  * AES variants are preferred over ChaCha20 variants for performance,
--    assuming AES-NI support [2].
--  * AES-256 is preferred over AES-128 "because we can" and performance
--    is not significantly worse, though the comparable key sizes needed for
--    RSA and DH to achieve a comparable level of security to 256 bit
--    symmetric keys are typically not used (see [3]).
--
-- This list requires on both ends of a connection either a TLS 1.2
-- implementation that includes RFC5288 [4] (e.g. OpenSSL 1.0.1+) or a
-- TLS 1.3 implementation that includes at least the mandatory cipher
-- suites. For a list of OpenSSL cipher suites and how they map to TLS
-- names, see also [5].
--
-- References:
--
-- [1] https://tlswg.github.io/tls13-spec/#rfc.appendix.A.4
-- [2] https://calomel.org/aesni_ssl_performance.html
-- [3] https://www.keylength.com/en/3/
-- [4] https://tools.ietf.org/html/rfc5288#section-3
-- [5] https://www.openssl.org/docs/manmaster/apps/ciphers.html
rsaCiphers :: String
rsaCiphers =
  showString "ECDHE-RSA-AES256-GCM-SHA384," -- TLS 1.3
    . showString "ECDHE-RSA-AES128-GCM-SHA256," -- TLS 1.3 (mandatory)
    . showString "ECDHE-RSA-CHACHA20-POLY1305," -- TLS 1.3
    . showString "DHE-RSA-AES256-GCM-SHA384," -- TLS 1.2 / TLS 1.3
    . showString "DHE-RSA-AES128-GCM-SHA256," -- TLS 1.2 / TLS 1.3
    . showString "DHE-RSA-CHACHA20-POLY1305" -- TLS 1.3
    $ ""

-- Public Key Pinning ----------------------------------------------------
--
-- Overview: https://www.owasp.org/index.php/Certificate_and_Public_Key_Pinning

-- | Exception thrown by 'verifyFingerprint'.
data PinPubKeyException
  = -- | No peer certificate was found.
    PinMissingCert
  | -- | A peer certificate failed validation (e.g. signature or expiry).
    PinInvalidCert
  | -- | The peer certificate does not contain a valid public key.
    PinInvalidPubKey
  | -- | The public key fingerprint of the peer certificate
    -- did not match any of the pinned fingerprints.
    PinFingerprintMismatch
  deriving (Eq, Show)

instance Exception PinPubKeyException

-- | Verify the fingerprint of the public key taken from the peer certificate
-- of the given 'SSL' connection against a list of /pinned/ fingerprints.
--
-- To use this function with 'opensslManagerSettingsWith'', the 'VerificationMode'
-- must be set to 'VerifyNone'. Certificate validation is still performed by OpenSSL
-- but the TLS handshake won't be aborted early, giving this function a chance
-- to check for a self-signed certificate after evaluating OpenSSL's verification
-- result using 'getVerifyResult'.
verifyFingerprint ::
  -- | Compute the fingerprint of the peer's public key.
  (SomePublicKey -> IO (Maybe ByteString)) ->
  -- | The list of /pinned/ fingerprints.
  [ByteString] ->
  -- | The 'SSL' connection from which to obtain the peer
  -- certificate and public key.
  SSL ->
  IO ()
verifyFingerprint hash fprs ssl = do
  cert <- SSL.getPeerCertificate ssl >>= maybe (throwIO PinMissingCert) pure
  pkey <- X509.getPublicKey cert
  mfpr <- hash pkey
  case mfpr of
    Nothing -> throwIO PinInvalidPubKey
    Just fp -> do
      unless (any (constEqBytes fp) fprs) $
        throwIO PinFingerprintMismatch
      vok <- SSL.getVerifyResult ssl
      unless vok $ do
        -- Check if the certificate is self-signed. Exceptions can be thrown if
        -- the signature could not be checked at all because it was ill-formed,
        -- the certificate or the request was not complete or some other error
        -- occurred. See -1 as return value in
        -- https://www.openssl.org/docs/man3.1/man3/X509_verify.html.
        self <- verifyX509 cert pkey
        unless (self == VerifySuccess) $
          throwIO PinInvalidCert
        -- For completeness, perform a date check as well.
        now <- getCurrentTime
        notBefore <- X509.getNotBefore cert
        notAfter <- X509.getNotAfter cert
        unless (now >= notBefore && now <= notAfter) $
          throwIO PinInvalidCert

-- [Note: Hostname verification]

-- RSA ------------------------------------------------------------------------

-- | Compute a simple (non-standard) fingerprint of an RSA
-- public key for use with 'verifyRsaFingerprint' with the given
-- 'Digest'.
rsaFingerprint :: RSAKey k => Digest -> k -> IO ByteString
rsaFingerprint d k = fmap (digestLBS d . toLazyByteString) $ do
  let s = rsaSize k
  n <- integerToMPI (rsaN k)
  e <- integerToMPI (rsaE k)
  pure $! intDec s <> byteString n <> byteString e

-- | 'verifyFingerprint' specialised to 'RSAPubKey's using 'rsaFingerprint'.
verifyRsaFingerprint :: Digest -> [ByteString] -> SSL -> IO ()
verifyRsaFingerprint d = verifyFingerprint $ \pk ->
  case toPublicKey pk of
    Nothing -> pure Nothing
    Just k -> Just <$> rsaFingerprint d (k :: RSAPubKey)

-- [Note: Hostname verification]
-- Ideally, we would like to perform proper hostname verification, which
-- is not done automatically by OpenSSL [1]. However, the necessary APIs
-- are not yet available via HsOpenSSL. Note though that public key pinning
-- is already supposed to thwart attacks based on a lack of or incorrect
-- hostname verification (see [2] for many common attacks and mistakes).
--
-- [1] https://wiki.openssl.org/index.php/Hostname_validation
-- [2] https://www.cs.utexas.edu/~shmat/shmat_ccs12.pdf

-- Utilities -----------------------------------------------------------------

-- | Get an SSL connection that has definitely had its fingerprints checked
-- (internally it just grabs a connection from a pool and does verification
-- if it's a fresh one).
--
-- Throws an error for other types of connections.
withVerifiedSslConnection ::
  -- | A function to verify fingerprints given an SSL connection
  (SSL -> IO ()) ->
  Manager ->
  -- | Request builder
  (Request -> Request) ->
  -- | This callback will be passed a modified
  --   request that always uses the verified
  --   connection
  (Request -> IO a) ->
  IO a
withVerifiedSslConnection verify man reqBuilder act =
  withConnection' req man Reuse $ \mConn -> do
    -- If we see this connection for the first time, verify fingerprints
    let conn = managedResource mConn
        seen = managedReused mConn
    unless seen $ case fromDynamic @SSL (connectionRaw conn) of
      Nothing -> error ("withVerifiedSslConnection: only SSL allowed: " <> show req)
      Just ssl -> verify ssl
    -- Make a request using this connection and return it back to the
    -- pool (that's what 'Reuse' is for)
    act req {connectionOverride = Just mConn}
  where
    req = reqBuilder defaultRequest
