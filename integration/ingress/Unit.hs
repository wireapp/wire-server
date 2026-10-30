{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Either
import Data.Maybe (isNothing)
import Data.X509 (CertificateChain (..))
import Data.X509.CertificateStore (makeCertificateStore)
import Data.X509.Validation (exceptionValidationCache, validateDefault)
import qualified Network.TLS as TLS
import Testlib.Ingress
import qualified Testlib.Ingress.Kubernetes as Kube
import Testlib.Ingress.TLS
import Prelude

main :: IO ()
main = do
  let alert = TLS.Header TLS.ProtocolType_Alert TLS.TLS12 2
  ensure (peerAlert alert (BS.pack [2, 40]) == Just TLS.HandshakeFailure) "Fatal alert not recognised"
  forM_ [BS.empty, BS.pack [1, 40], BS.pack [2, 42]] $ \bytes ->
    ensure (isNothing (peerAlert alert bytes)) "Unrelated failure treated as policy rejection"
  ensure (isNothing (peerAlert (TLS.Header TLS.ProtocolType_AppData TLS.TLS12 2) (BS.pack [2, 40]))) "Application data treated as alert"
  ensure (Kube.decodeForwardFrame 10443 [] (BS.pack [0, 203, 40]) == Right (0, True, BS.empty)) "Port-forward initialization failed"
  ensure (Kube.decodeForwardFrame 10443 [0] (BS.pack [0, 1, 2]) == Right (0, False, BS.pack [1, 2])) "Port-forward payload changed"
  forM_ [BS.empty, BS.pack [2, 203, 40], BS.pack [0, 80, 0]] $ \bytes ->
    ensure (isLeft $ Kube.decodeForwardFrame 10443 [] bytes) "Invalid forwarding frame accepted"
  material <- generateCertificate ["api.ingress.test"]
  ensure (TLS.credentialLoadX509FromMemory material.certificatePEM material.privateKeyPEM == Right material.credential) "Certificate PEM roundtrip failed"
  other <- generateCertificate ["api.other.test"]
  let validate store host = validateDefault (makeCertificateStore store) (exceptionValidationCache []) (host, "") (CertificateChain [material.certificate])
  valid <- validate [material.certificate] "api.ingress.test"
  ensure (null valid) "Generated certificate fails validation"
  wrongHost <- validate [material.certificate] "api.other.test"
  ensure (not $ null wrongHost) "Wrong certificate hostname accepted"
  wrongCA <- validate [other.certificate] "api.ingress.test"
  ensure (not $ null wrongCA) "Untrusted certificate accepted"
  forM_ [DefaultTLS, CustomTLS12, FIPS] $ \profile -> case profileProbes profile of
    first : rest -> do
      ensure (first.expectation == Accept) "Profile must establish connectivity first"
      ensure (any ((== Reject) . (.expectation)) rest) "Profile lacks negative checks"
      ensure (not $ checkOutcome first (PeerRejected TLS.HandshakeFailure)) "Positive probe accepted rejection"
    [] -> ensure False "Empty profile"
  putStrLn "Ingress native unit checks passed"
