{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
-- SPDX-License-Identifier: AGPL-3.0-only

module Testlib.Ingress.TLS where

import Control.Exception
import Control.Monad
import Crypto.Hash (SHA256 (..))
import Crypto.Number.Serialize (i2ospOf_)
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Generate as ECC
import Crypto.PubKey.ECC.Types
import qualified Crypto.Store.PKCS8 as PKCS8
import qualified Crypto.Store.X509 as Store
import Data.ASN1.BinaryEncoding (DER (..))
import Data.ASN1.Encoding (encodeASN1')
import Data.ASN1.Types (ASN1 (..), ASN1ConstructionType (Sequence), getObjectID)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.Hourglass (Elapsed (..), timeConvert)
import Data.IORef
import Data.List (find)
import Data.String (fromString)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.X509
import Data.X509.CertificateStore (makeCertificateStore)
import qualified Network.TLS as TLS
import Prelude

data CertificateMaterial = CertificateMaterial
  { certificate :: SignedCertificate,
    credential :: TLS.Credential,
    certificatePEM :: BS.ByteString,
    privateKeyPEM :: BS.ByteString
  }

data Expectation = Accept | Reject
  deriving stock (Eq, Show)

data Probe = Probe
  { version :: TLS.Version,
    cipher :: TLS.Cipher,
    group :: TLS.Group,
    expectation :: Expectation
  }
  deriving stock (Eq, Show)

data Outcome = Negotiated TLS.Information Value | PeerRejected TLS.AlertDescription
  deriving stock (Show)

generateCertificate :: [String] -> IO CertificateMaterial
generateCertificate hosts = do
  (public, private) <- ECC.generate (getCurveByName SEC_p256r1)
  now <- floor <$> getPOSIXTime
  point <- case ECDSA.public_q public of
    Point x y -> pure $ SerializedPoint (BS.cons 4 (i2ospOf_ 32 x <> i2ospOf_ 32 y))
    PointO -> ioError (userError "Generated invalid EC public key")
  let key = PrivKeyEC (PrivKeyEC_Named SEC_p256r1 (ECDSA.private_d private))
      name = DistinguishedName [(getObjectID DnCommonName, fromString "wire-ingress-regression")]
      algorithm = SignatureALG HashSHA256 PubKeyALG_EC
      unsigned =
        Certificate
          { certVersion = 2,
            certSerial = now,
            certSignatureAlg = algorithm,
            certIssuerDN = name,
            certSubjectDN = name,
            certValidity = (timeConvert $ Elapsed $ fromIntegral (now - 60), timeConvert $ Elapsed $ fromIntegral (now + 86400)),
            certPubKey = PubKeyEC (PubKeyEC_Named SEC_p256r1 point),
            certExtensions = Extensions $ Just [extensionEncode True (ExtBasicConstraints True Nothing), extensionEncode False (ExtSubjectAltName $ map AltNameDNS hosts)]
          }
      sign bytes = do
        ECDSA.Signature r s <- ECDSA.sign private SHA256 bytes
        pure (encodeASN1' DER [Start Sequence, IntVal r, IntVal s, End Sequence], algorithm)
  certificate <- objectToSignedExactF sign unsigned
  pure
    CertificateMaterial
      { certificate,
        credential = (CertificateChain [certificate], key),
        certificatePEM = Store.writeSignedObjectToMemory [certificate],
        privateKeyPEM = PKCS8.writeKeyFileToMemory PKCS8.PKCS8Format [PKCS8.keyPairFromPrivKey key]
      }

-- Rejections in this matrix happen before ServerHello, so their fatal alert
-- records are plaintext. Observe the received record, not an exception's prose:
-- local configuration failures and EOF must never count as server rejection.
peerAlert :: TLS.Header -> BS.ByteString -> Maybe TLS.AlertDescription
peerAlert (TLS.Header TLS.ProtocolType_Alert _ _) bytes = case BS.unpack bytes of
  [2, 40] -> Just TLS.HandshakeFailure
  [2, 70] -> Just TLS.ProtocolVersion
  [2, 71] -> Just TLS.InsufficientSecurity
  _ -> Nothing
peerAlert _ _ = Nothing

checkOutcome :: Probe -> Outcome -> Bool
checkOutcome spec (PeerRejected alert) = spec.expectation == Reject && alert `elem` [TLS.HandshakeFailure, TLS.ProtocolVersion, TLS.InsufficientSecurity]
checkOutcome spec (Negotiated info _) = spec.expectation == Accept && TLS.infoVersion info == spec.version && TLS.cipherID (TLS.infoCipher info) == TLS.cipherID spec.cipher && TLS.infoSupportedGroup info == Just spec.group

probe :: TLS.Backend -> CertificateMaterial -> Maybe CertificateMaterial -> String -> String -> Probe -> IO Outcome
probe backend server client host authority specification = do
  let defaults = TLS.defaultParamsClient host BS.empty
      parameters =
        defaults
          { TLS.clientSupported =
              (TLS.clientSupported defaults)
                { TLS.supportedVersions = [specification.version],
                  TLS.supportedCiphers = [specification.cipher],
                  TLS.supportedGroups = [specification.group],
                  TLS.supportedGroupsTLS13 = [[specification.group]]
                },
            TLS.clientShared = (TLS.clientShared defaults) {TLS.sharedCAStore = makeCertificateStore [server.certificate]},
            TLS.clientHooks = (TLS.clientHooks defaults) {TLS.onCertificateRequest = const (pure $ (.credential) <$> client)}
          }
  context <- TLS.contextNew backend parameters
  receivedAlert <- newIORef Nothing
  TLS.contextHookSetLogging
    context
    TLS.defaultLogging
      { TLS.loggingIORecv = \header bytes -> forM_ (peerAlert header bytes) (writeIORef receivedAlert . Just)
      }
  result <- try @TLS.TLSException (TLS.handshake context)
  case result of
    Left failure -> readIORef receivedAlert >>= maybe (throwIO failure) (pure . PeerRejected)
    Right () -> do
      info <- TLS.contextGetInformation context >>= maybe (ioError $ userError "TLS handshake returned no negotiated parameters") pure
      TLS.sendData context $ LBS.fromStrict $ BS8.pack ("GET / HTTP/1.1\r\nHost: " <> authority <> "\r\nX-SSL-Certificate: spoofed\r\nConnection: close\r\n\r\n")
      response <- readHTTPResponse context
      pure (Negotiated info response)

-- The synthetic upstream deliberately supplies Content-Length. Stop at that
-- boundary rather than weakening TLS EOF checks to accommodate abrupt closes.
readHTTPResponse :: TLS.Context -> IO Value
readHTTPResponse context = loop BS.empty
  where
    loop bytes = do
      unless (BS.length bytes <= 1024 * 1024) $ ioError (userError "Oversized ingress response")
      let (headers, rest) = BS.breakSubstring "\r\n\r\n" bytes
      if BS.null rest
        then more bytes
        else do
          unless ("HTTP/1.1 200 " `BS.isPrefixOf` headers) $ ioError (userError "Ingress did not return HTTP 200")
          let fields = map (BS8.map toLower) (BS8.lines headers)
          lengthLine <- maybe (ioError $ userError "Ingress response has no Content-Length") pure (find (BS.isPrefixOf "content-length:") fields)
          size <- case BS8.readInt (BS8.dropWhile (== ' ') $ BS.drop 15 lengthLine) of
            Just (n, trailing) | n >= 0 && n <= 1024 * 1024 && BS8.all (`elem` [' ', '\r']) trailing -> pure n
            _ -> ioError (userError "Invalid ingress Content-Length")
          let body = BS.drop 4 rest
          if BS.length body < size then more bytes else either (ioError . userError) pure (eitherDecodeStrict' $ BS.take size body)
    more bytes = do
      next <- TLS.recvData context
      when (BS.null next) $ ioError (userError "Truncated ingress HTTP response")
      loop (bytes <> next)
