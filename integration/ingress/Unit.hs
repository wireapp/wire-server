module Main where

import Control.Monad
import Data.Either
import System.Exit
import Testlib.Ingress
import Prelude

main :: IO ()
main = do
  let accept = Probe TLS13 tls13AES "P-256" Accept
      reject = accept {expectation = Reject}
      success = (ExitSuccess, "HTTP/1.1 200 OK\nwire-ingress-regression-ok", "CONNECTION ESTABLISHED\nVerification: OK\nProtocol version: TLSv1.3\nCiphersuite: TLS_AES_128_GCM_SHA256")
  ensure (isRight $ checkHandshake accept success) "Valid handshake rejected"
  ensure (isLeft $ checkHandshake reject success) "Successful TLS treated as rejection"
  forM_ ["Connection refused", "no cipher match", "unexpected eof", ""] $ \message ->
    ensure (isLeft $ checkHandshake reject (ExitFailure 1, "", message)) "Transport failure treated as TLS rejection"
  ensure (isRight $ checkHandshake reject (ExitFailure 1, "", "ssl/tls alert handshake failure")) "TLS alert not recognised"
  ensure (isLeft $ checkHandshake accept (ExitSuccess, "HTTP/1.1 503", "")) "Unavailable backend accepted"
  forM_ [DefaultTLS, CustomTLS12, FIPS] $ \profile -> case profileProbes profile of
    first : rest -> do
      ensure (first.expectation == Accept) "Profile must establish connectivity first"
      ensure (any ((== Reject) . (.expectation)) rest) "Profile lacks negative checks"
    [] -> ensure False "Empty profile"
  putStrLn "Ingress unit checks passed"
