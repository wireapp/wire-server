-- | Extended version of http-client-openssl:
--     https://github.com/snoyberg/http-client/tree/master/http-client-openssl
--     (c) 2013 Michael Snoyman
module Network.HTTP.Client.OpenSSL
    ( -- * Settings
      opensslManagerSettings
    , opensslManagerSettingsWith

      -- * Public Key Pinning
    , verifyFingerprint
      -- ** RSA-specific
    , rsaFingerprint
    , verifyRsaFingerprint

      -- * Cipher suites
    , rsaCiphers

      -- * Re-exports
    , withOpenSSL
    ) where

import Control.Exception
import Control.Monad
import Data.Byteable (constEqBytes)
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.Foldable (for_)
import Data.Monoid
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Client (ManagerSettings (managerTlsConnection))
import Network.HTTP.Client.Internal
import Network.Socket  as Net
import OpenSSL
import OpenSSL.BN (integerToMPI)
import OpenSSL.EVP.Digest (Digest, digestLBS)
import OpenSSL.EVP.PKey (toPublicKey, SomePublicKey)
import OpenSSL.EVP.Verify (VerifyStatus (..))
import OpenSSL.RSA
import OpenSSL.Session as SSL
import OpenSSL.X509 as X509

import qualified Network.HTTP.Client as Client

-- Settings -----------------------------------------------------------------

opensslManagerSettings :: SSLContext -> ManagerSettings
opensslManagerSettings ctx = opensslManagerSettingsWith' ctx Nothing

opensslManagerSettingsWith :: SSLContext -> (SSL -> IO ()) -> ManagerSettings
opensslManagerSettingsWith ctx = opensslManagerSettingsWith' ctx . Just

-- | Install a callback function into a 'Manager' that is called for
-- every newly established 'SSL' connection, prior to yielding it to the
-- 'Manager' for use.
opensslManagerSettingsWith' :: SSLContext -> Maybe (SSL -> IO ()) -> ManagerSettings
opensslManagerSettingsWith' ctx fn = Client.defaultManagerSettings
    { managerTlsConnection = pure $ \_ h p ->
        bracketOnError
            (newSocket stdHints h p)
            (Net.close . fst)
            (\(sock, addr) -> do
                ssl <- newSSL ctx sock addr
                for_ fn $ \f -> f ssl
                toConnection ssl sock)
    }

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
     . showString "DHE-RSA-AES256-GCM-SHA384,"   -- TLS 1.2 / TLS 1.3
     . showString "DHE-RSA-AES128-GCM-SHA256,"   -- TLS 1.2 / TLS 1.3
     . showString "DHE-RSA-CHACHA20-POLY1305"    -- TLS 1.3
     $ ""

-- Public Key Pinning ----------------------------------------------------
--
-- Overview: https://www.owasp.org/index.php/Certificate_and_Public_Key_Pinning

-- | Exception thrown by 'verifyFingerprint'.
data PinPubKeyException
    = PinMissingCert
        -- ^ No peer certificate was found.
    | PinInvalidCert
        -- ^ A peer certificate failed validation (e.g. signature or expiry).
    | PinInvalidPubKey
        -- ^ The peer certificate does not contain a valid public key.
    | PinFingerprintMismatch
        -- ^ The public key fingerprint of the peer certificate
        -- did not match any of the pinned fingerprints.
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
verifyFingerprint
    :: (SomePublicKey -> IO (Maybe ByteString))
        -- ^ Compute the fingerprint of the peer's public key.
    -> [ByteString]
        -- ^ The list of /pinned/ fingerprints.
    -> SSL
        -- ^ The 'SSL' connection from which to obtain the peer
        -- certificate and public key.
    -> IO ()
verifyFingerprint hash fprs ssl = do
    cert <- SSL.getPeerCertificate ssl >>= maybe (throwIO PinMissingCert) return
    pkey <- X509.getPublicKey cert
    mfpr <- hash pkey
    case mfpr of
        Nothing -> throwIO PinInvalidPubKey
        Just fp -> do
            unless (any (constEqBytes fp) fprs) $
                throwIO PinFingerprintMismatch
            vok <- SSL.getVerifyResult ssl
            unless vok $ do
                -- Check if the certificate is self-signed.
                self <- verifyX509 cert pkey
                unless (self == VerifySuccess) $
                    throwIO PinInvalidCert
                -- For completeness, perform a date check as well.
                now <- getCurrentTime
                notBefore <- X509.getNotBefore cert
                notAfter  <- X509.getNotAfter  cert
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
    return $! intDec s <> byteString n <> byteString e

-- | 'verifyFingerprint' specialised to 'RSAPubKey's using 'rsaFingerprint'.
verifyRsaFingerprint :: Digest -> [ByteString] -> SSL -> IO ()
verifyRsaFingerprint d = verifyFingerprint $ \pk ->
    case toPublicKey pk of
        Nothing -> return Nothing
        Just  k -> Just <$> rsaFingerprint d (k :: RSAPubKey)

-- [Note: Hostname verification]
-- Ideally, we would like to perform proper hostname verification, which
-- is not done automatically by OpenSSL [1]. However, the necessary APIs
-- are not yet available via HsOpenSSL. Note though that public key pinning
-- is already supposed to thwart attacks based on a lack of or incorrect
-- hostname verification (see [2] for many common attacks and mistakes).
--
-- [1] https://wiki.openssl.org/index.php/Hostname_validation
-- [2] https://www.cs.utexas.edu/~shmat/shmat_ccs12.pdf

-- Internal -----------------------------------------------------------------

newSocket :: AddrInfo -> HostName -> Int -> IO (Socket, SockAddr)
newSocket hints hst prt = do
    (ai:_) <- Net.getAddrInfo (Just hints) (Just hst) (Just $ show prt)
    s <- Net.socket (addrFamily ai) (addrSocketType ai)  (addrProtocol ai)
    return (s, addrAddress ai)

newSSL :: SSLContext -> Socket -> SockAddr -> IO SSL
newSSL ctx sock addr = do
    Net.connect sock addr
    ssl <- SSL.connection ctx sock
    SSL.connect ssl
    return ssl

toConnection :: SSL -> Socket -> IO Connection
toConnection ssl sock =
    makeConnection
        (SSL.read ssl 32752)
        (SSL.write ssl)
        (Net.close sock)
        ssl

stdHints :: AddrInfo
stdHints = Net.defaultHints
    { Net.addrFlags      = [AI_ADDRCONFIG, AI_NUMERICSERV]
    , Net.addrFamily     = AF_INET
    , Net.addrSocketType = Stream
    }

