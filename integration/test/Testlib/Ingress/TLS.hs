{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
-- SPDX-License-Identifier: AGPL-3.0-only

module Testlib.Ingress.TLS
  ( CertificateMaterial (certificate),
    generateCertificate,
    credential,
    certificatePEM,
    privateKeyPEM,
    Parameters (..),
    Probe (..),
    Request (..),
    Negotiated (..),
    Outcome (..),
    BackendResponse (..),
    probe,
    validateHandshake,
    peerAlert,
    ResponseDecoder,
    DecodeResult (..),
    responseDecoder,
    feedResponse,
  )
where

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
import Data.Foldable (toList)
import Data.Hourglass (Elapsed (..), timeConvert)
import Data.IORef
import Data.List.NonEmpty (NonEmpty)
import Data.String (fromString)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.X509
import Data.X509.CertificateStore (makeCertificateStore)
import qualified Network.TLS as TLS
import Prelude

-- The key and certificate are the source of truth; serialized copies are derived.
-- No Show instance: an identity contains private key material.
data CertificateMaterial = CertificateMaterial
  { certificate :: SignedCertificate,
    privateKey :: PrivKey
  }

credential :: CertificateMaterial -> TLS.Credential
credential identity = (CertificateChain [identity.certificate], identity.privateKey)

certificatePEM, privateKeyPEM :: CertificateMaterial -> BS.ByteString
certificatePEM = Store.writeSignedObjectToMemory . pure . (.certificate)
privateKeyPEM = PKCS8.writeKeyFileToMemory PKCS8.PKCS8Format . pure . PKCS8.keyPairFromPrivKey . (.privateKey)

data Parameters = Parameters
  { version :: TLS.Version,
    cipher :: TLS.Cipher,
    group :: TLS.Group
  }
  deriving stock (Eq, Show)

data Probe = Accept Parameters | Reject Parameters
  deriving stock (Eq, Show)

data Request = Request
  { serverName :: String,
    authority :: String,
    clientIdentity :: Maybe CertificateMaterial
  }

data Negotiated = Negotiated
  { version :: TLS.Version,
    cipher :: TLS.Cipher,
    group :: Maybe TLS.Group
  }
  deriving stock (Eq, Show)

data BackendResponse = BackendResponse
  { marker :: Text,
    upstream :: Text,
    certificate :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON BackendResponse where
  parseJSON = withObject "ingress response" $ \o ->
    BackendResponse <$> o .: "marker" <*> o .: "upstream" <*> o .: "certificate"

data Outcome = Established Negotiated BackendResponse | PeerRejected TLS.AlertDescription
  deriving stock (Show)

validateHandshake :: Probe -> Outcome -> Either String ()
validateHandshake (Reject _) (PeerRejected alert)
  | alert `elem` [TLS.HandshakeFailure, TLS.ProtocolVersion, TLS.InsufficientSecurity] = Right ()
validateHandshake (Accept expected) (Established actual _)
  | expected.version == actual.version,
    TLS.cipherID expected.cipher == TLS.cipherID actual.cipher,
    Just expected.group == actual.group =
      Right ()
validateHandshake expected actual = Left $ "Expected " <> show expected <> "; received " <> show actual

generateCertificate :: NonEmpty String -> IO CertificateMaterial
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
            certExtensions = Extensions $ Just [extensionEncode True (ExtBasicConstraints True Nothing), extensionEncode False (ExtSubjectAltName $ map AltNameDNS (toList hosts))]
          }
      sign bytes = do
        ECDSA.Signature r s <- ECDSA.sign private SHA256 bytes
        pure (encodeASN1' DER [Start Sequence, IntVal r, IntVal s, End Sequence], algorithm)
  certificate <- objectToSignedExactF sign unsigned
  pure CertificateMaterial {certificate, privateKey = key}

-- Only a fatal peer alert counts as rejection. EOF, local errors and failed
-- certificate validation must never satisfy a negative test.
peerAlert :: TLS.Header -> BS.ByteString -> Maybe TLS.AlertDescription
peerAlert (TLS.Header TLS.ProtocolType_Alert _ _) bytes = case BS.unpack bytes of
  [2, 40] -> Just TLS.HandshakeFailure
  [2, 70] -> Just TLS.ProtocolVersion
  [2, 71] -> Just TLS.InsufficientSecurity
  _ -> Nothing
peerAlert _ _ = Nothing

clientParameters :: CertificateMaterial -> Request -> Parameters -> TLS.ClientParams
clientParameters server request parameters =
  defaults
    { TLS.clientSupported =
        (TLS.clientSupported defaults)
          { TLS.supportedVersions = [parameters.version],
            TLS.supportedCiphers = [parameters.cipher],
            TLS.supportedGroups = [parameters.group],
            TLS.supportedGroupsTLS13 = [[parameters.group]]
          },
      TLS.clientShared = (TLS.clientShared defaults) {TLS.sharedCAStore = makeCertificateStore [server.certificate]},
      TLS.clientHooks = (TLS.clientHooks defaults) {TLS.onCertificateRequest = const (pure $ credential <$> request.clientIdentity)}
    }
  where
    defaults = TLS.defaultParamsClient request.serverName BS.empty

probe :: TLS.Backend -> CertificateMaterial -> Request -> Parameters -> IO Outcome
probe backend server request parameters = do
  context <- TLS.contextNew backend (clientParameters server request parameters)
  -- The TLS logging API is callback-based; keep mutation confined to this adapter.
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
      info <- TLS.contextGetInformation context >>= maybe (ioError $ userError "Missing negotiated TLS parameters") pure
      TLS.sendData context $
        LBS.fromStrict $
          BS8.pack
            ("GET / HTTP/1.1\r\nHost: " <> request.authority <> "\r\nX-SSL-Certificate: spoofed\r\nConnection: close\r\n\r\n")
      Established (Negotiated (TLS.infoVersion info) (TLS.infoCipher info) (TLS.infoSupportedGroup info)) <$> receiveResponse context

-- A bounded incremental decoder for the fixture's Content-Length responses.
-- Keeping this pure lets tests exercise arbitrary TLS record boundaries.
data ResponseDecoder = Headers BS.ByteString | Body Int BS.ByteString
  deriving stock (Eq, Show)

data DecodeResult = Await ResponseDecoder | Complete BackendResponse
  deriving stock (Eq, Show)

responseDecoder :: ResponseDecoder
responseDecoder = Headers BS.empty

feedResponse :: ResponseDecoder -> BS.ByteString -> Either String DecodeResult
feedResponse (Headers previous) chunk = do
  let bytes = previous <> chunk
      (headers, rest) = BS.breakSubstring "\r\n\r\n" bytes
  when (BS.length headers > 16384) $ Left "Oversized HTTP headers"
  if BS.null rest
    then pure (Await $ Headers bytes)
    else do
      unless ("HTTP/1.1 200 " `BS.isPrefixOf` headers) $ Left "Ingress did not return HTTP 200"
      let fields = map (BS8.map toLower) (drop 1 $ BS8.lines headers)
          lengths = [BS8.dropWhile (== ' ') $ BS.drop 15 field | field <- fields, "content-length:" `BS.isPrefixOf` field]
      when (any (BS.isPrefixOf "transfer-encoding:") fields) $ Left "Unexpected Transfer-Encoding"
      size <- case lengths of
        [value] -> case BS8.readInteger value of
          Just (n, trailing) | n >= 0 && n <= 1048576 && BS8.all (`elem` [' ', '\r']) trailing -> Right (fromInteger n)
          _ -> Left "Invalid Content-Length"
        _ -> Left "Expected exactly one Content-Length"
      feedResponse (Body size BS.empty) (BS.drop 4 rest)
feedResponse (Body size previous) chunk
  | BS.length bytes > size = Left "Unexpected bytes after HTTP response"
  | BS.length bytes < size = Right (Await $ Body size bytes)
  | otherwise = Complete <$> eitherDecodeStrict' bytes
  where
    bytes = previous <> chunk

receiveResponse :: TLS.Context -> IO BackendResponse
receiveResponse context = go responseDecoder
  where
    go decoder = do
      bytes <- TLS.recvData context
      when (BS.null bytes) $ ioError (userError "Truncated ingress HTTP response")
      decoded <- either (ioError . userError) pure (feedResponse decoder bytes)
      case decoded of
        Await next -> go next
        Complete response -> pure response
