{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
-- SPDX-License-Identifier: AGPL-3.0-only

module Main where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Pair, parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Either
import Data.Foldable (toList)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word8)
import Data.X509 (CertificateChain (..))
import Data.X509.CertificateStore (makeCertificateStore)
import Data.X509.Validation (exceptionValidationCache, validateDefault)
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as Cipher
import Test.Hspec (describe, hspec, it, shouldBe, shouldContain, shouldReturn, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonNegative (..))
import qualified Testlib.Ingress.Kubeconfig as Config
import qualified Testlib.Ingress.Kubernetes as Kube
import Testlib.Ingress.Model
import Testlib.Ingress.TLS
import Prelude

main :: IO ()
main = hspec $ do
  describe "scenario model" $ do
    it "covers twelve distinct Gateway/profile/domain combinations" $ do
      length scenarios `shouldBe` 12
      length (nub $ map scenarioName scenarios) `shouldBe` 12
    forM_ scenarios $ \scenario -> it (scenarioName scenario) $ do
      let routes = toList (endpoints scenario)
      forM_ [API Primary, Webapp Primary, Extra] $ \endpoint -> routes `shouldContain` [endpoint]
      (Federation `elem` routes) `shouldBe` (scenario.mode == SingleDomain)
      (API Secondary `elem` routes) `shouldBe` (scenario.mode == MultipleDomains)
      (Webapp Secondary `elem` routes) `shouldBe` (scenario.mode == MultipleDomains)
      (Companion `elem` routes) `shouldBe` (scenario.profile == FIPS)
      forM_ routes $ \endpoint ->
        hostname endpoint `shouldSatisfy` (`elem` certificateHosts (endpointDomain endpoint))
    it "rejects unsafe namespace prefixes" $
      forM_ ["", "-test", "test-", "test/other", "UPPER", replicate 29 'a'] $ \prefix ->
        validateNamespacePrefix prefix `shouldSatisfy` isLeft

  describe "TLS expectations" $ do
    forM_ [DefaultTLS, CustomTLS12, FIPS] $ \profile -> it (show profile <> " proves connectivity before rejection checks") $ do
      let plan = profilePlan profile
          parameters = plan.connectivity
          actual = Negotiated parameters.version parameters.cipher (Just parameters.group)
          established = Established actual backendResponse
      validateHandshake (Accept parameters) established `shouldBe` Right ()
      validateHandshake (Reject parameters) established `shouldSatisfy` isLeft
      validateHandshake (Accept parameters) (PeerRejected TLS.HandshakeFailure) `shouldSatisfy` isLeft
      plan.assertions `shouldSatisfy` any (\case Reject _ -> True; _ -> False)
    it "checks the negotiated version, cipher and group independently" $ do
      let parameters = (profilePlan DefaultTLS).connectivity
          actual = Negotiated parameters.version parameters.cipher (Just parameters.group)
      forM_ [Negotiated TLS.TLS12 actual.cipher actual.group, Negotiated actual.version Cipher.cipher13_AES_256_GCM_SHA384 actual.group, Negotiated actual.version actual.cipher (Just TLS.X25519), Negotiated actual.version actual.cipher Nothing] $ \wrong ->
        validateHandshake (Accept parameters) (Established wrong backendResponse) `shouldSatisfy` isLeft
    it "accepts only the expected peer policy alerts" $ do
      let spec = Reject (profilePlan DefaultTLS).connectivity
      forM_ [TLS.HandshakeFailure, TLS.ProtocolVersion, TLS.InsufficientSecurity] $ \alert ->
        validateHandshake spec (PeerRejected alert) `shouldBe` Right ()
      validateHandshake spec (PeerRejected TLS.BadCertificate) `shouldSatisfy` isLeft
    it "does not mistake EOF, warning alerts or application data for rejection" $ do
      let alert = TLS.Header TLS.ProtocolType_Alert TLS.TLS12 2
      peerAlert alert (BS.pack [2, 40]) `shouldBe` Just TLS.HandshakeFailure
      forM_ [BS.empty, BS.pack [1, 40], BS.pack [2, 42]] $ \bytes -> peerAlert alert bytes `shouldBe` Nothing
      peerAlert (TLS.Header TLS.ProtocolType_AppData TLS.TLS12 2) (BS.pack [2, 40]) `shouldBe` Nothing

  describe "backend response validation" $ do
    it "checks the marker and exact upstream, not merely HTTP success" $ do
      validateBackendResponse (API Primary) Nothing backendResponse `shouldBe` Right ()
      validateBackendResponse (API Primary) Nothing (BackendResponse "wrong-marker" "api" "") `shouldSatisfy` isLeft
      validateBackendResponse (Webapp Secondary) Nothing backendResponse `shouldSatisfy` isLeft
    it "rejects spoofed and incorrectly forwarded federation certificates" $ do
      let response = BackendResponse "wire-ingress-regression-ok" "federation"
      validateBackendResponse Federation Nothing (response "") `shouldBe` Right ()
      validateBackendResponse Federation Nothing (response "spoofed") `shouldSatisfy` isLeft
      validateBackendResponse Federation (Just "test+certificate") (response "test%2Bcertificate") `shouldBe` Right ()
      validateBackendResponse Federation (Just "test+certificate") (response "some-other-certificate") `shouldSatisfy` isLeft

  describe "incremental HTTP decoder" $ do
    it "decodes complete and single-byte-fragmented responses" $ do
      decodeChunks [responseBytes] `shouldBe` Right backendResponse
      decodeChunks (map BS.singleton $ BS.unpack responseBytes) `shouldBe` Right backendResponse
    prop "is independent of TLS record boundaries" $ \(NonNegative cut) ->
      let (before, after) = BS.splitAt (cut `mod` (BS.length responseBytes + 1)) responseBytes
       in decodeChunks [before, after] == Right backendResponse
    it "rejects truncation" $ decodeChunks [BS.init responseBytes] `shouldSatisfy` isLeft
    it "rejects oversized, ambiguous or malformed framing"
      $ forM_
        [ "HTTP/1.1 200 OK\r\nContent-Length: 1048577\r\n\r\n",
          "HTTP/1.1 200 OK\r\nContent-Length: -1\r\n\r\n",
          "HTTP/1.1 200 OK\r\nContent-Length: 0\r\nContent-Length: 0\r\n\r\n",
          "HTTP/1.1 200 OK\r\nContent-Length: 0\r\nTransfer-Encoding: chunked\r\n\r\n",
          "HTTP/1.1 503 Unavailable\r\nContent-Length: 0\r\n\r\n",
          "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\n{}",
          BS.replicate 16385 65,
          responseBytes <> "extra"
        ]
      $ \bytes -> feedResponse responseDecoder bytes `shouldSatisfy` isLeft

  describe "Kubernetes port-forward state machine" $ do
    it "allows either channel's announcement to arrive first" $ do
      let dataFirst = Kube.receiveFrame 10443 Kube.initialChannels (BS.pack [0, 203, 40]) >>= \(state, _) -> Kube.receiveFrame 10443 state (BS.pack [1, 203, 40])
          errorFirst = Kube.receiveFrame 10443 Kube.initialChannels (BS.pack [1, 203, 40]) >>= \(state, _) -> Kube.receiveFrame 10443 state (BS.pack [0, 203, 40])
      dataFirst `shouldBe` errorFirst
      dataFirst `shouldSatisfy` isRight
    prop "preserves all payload bytes after initialization" $ \(bytes :: [Word8]) ->
      let payload = BS.pack bytes
          result = Kube.receiveFrame 10443 Kube.initialChannels (BS.pack [0, 203, 40]) >>= \(state, _) -> Kube.receiveFrame 10443 state (BS.cons 0 payload)
       in fmap snd result == Right payload
    it "rejects invalid channels, wrong ports and remote errors" $ do
      Kube.receiveFrame 10443 Kube.initialChannels BS.empty `shouldBe` Left Kube.InvalidChannel
      Kube.receiveFrame 10443 Kube.initialChannels (BS.pack [2, 203, 40]) `shouldBe` Left Kube.InvalidChannel
      Kube.receiveFrame 10443 Kube.initialChannels (BS.pack [0, 80, 0]) `shouldBe` Left Kube.IncorrectPort
      let result = Kube.receiveFrame 10443 Kube.initialChannels (BS.pack [1, 203, 40]) >>= \(state, _) -> Kube.receiveFrame 10443 state "\1failure"
      result `shouldBe` Left Kube.RemoteForwardError

  describe "Kubernetes resource ownership" $ do
    it "accepts the requested scope with optional matching metadata" $ do
      Kube.validateScope Kube.Cluster False Nothing `shouldBe` Right ()
      Kube.validateScope (Kube.Namespace "fixture") True Nothing `shouldBe` Right ()
      Kube.validateScope (Kube.Namespace "fixture") True (Just "fixture") `shouldBe` Right ()
    it "rejects resources outside the fixture's ownership" $ do
      Kube.validateScope (Kube.Namespace "fixture") True (Just "someone-else") `shouldSatisfy` isLeft
      Kube.validateScope (Kube.Namespace "fixture") False Nothing `shouldSatisfy` isLeft
      Kube.validateScope Kube.Cluster True Nothing `shouldSatisfy` isLeft
      Kube.validateScope Kube.Cluster False (Just "fixture") `shouldSatisfy` isLeft

  describe "Kubernetes response decoding" $ do
    it "waits for absent status and rejects malformed status" $ do
      statusCondition (object ["metadata" .= object ["name" .= String "pod"]]) `shouldBe` Right False
      isLeft (statusCondition $ object ["metadata" .= object ["name" .= String "pod"], "status" .= String "broken"]) `shouldBe` True
    it "does not accept stale observed generations" $ do
      statusCondition (statusResource 1) `shouldBe` Right False
      statusCondition (statusResource 2) `shouldBe` Right True

  describe "kubeconfig decoding" $ do
    it "accepts certificate and token configurations without doing IO" $ do
      isRight (decodeConfig [] ["token" .= String "synthetic-token"]) `shouldBe` True
      isRight (decodeConfig [] ["client-certificate-data" .= String "AQID", "client-key-data" .= String "AQID"]) `shouldBe` True
    it "rejects malformed security options rather than treating them as absent" $ do
      forM_ [Bool True, String "true", object []] $ \insecure ->
        isLeft (decodeConfig ["insecure-skip-tls-verify" .= insecure] ["token" .= String "synthetic-token"]) `shouldBe` True
    it "rejects incomplete credentials and unsupported authentication" $ do
      forM_ [[], ["client-certificate-data" .= String "AQID"], ["client-key-data" .= String "AQID"], ["token" .= String "synthetic-token", "exec" .= object []]] $ \user ->
        isLeft (decodeConfig [] user) `shouldBe` True
    it "rejects malformed base64 certificate data" $
      isLeft (decodeConfig ["certificate-authority-data" .= String "!not-base64"] ["token" .= String "synthetic-token"]) `shouldBe` True

  describe "native certificate material" $
    it "round-trips PEM and enforces hostname and trust" $ do
      identity <- generateCertificate ("api.ingress.test" :| [])
      other <- generateCertificate ("api.other.test" :| [])
      (TLS.credentialLoadX509FromMemory (certificatePEM identity) (privateKeyPEM identity) == Right (credential identity)) `shouldBe` True
      let validate store host = validateDefault (makeCertificateStore store) (exceptionValidationCache []) (host, "") (CertificateChain [identity.certificate])
      validate [identity.certificate] "api.ingress.test" `shouldReturn` []
      validate [identity.certificate] "api.other.test" >>= (`shouldSatisfy` (not . null))
      validate [other.certificate] "api.ingress.test" >>= (`shouldSatisfy` (not . null))

backendResponse :: BackendResponse
backendResponse = BackendResponse "wire-ingress-regression-ok" "api" ""

responseBytes :: BS.ByteString
responseBytes = "HTTP/1.1 200 OK\r\nContent-Length: " <> BS8.pack (show $ BS.length body) <> "\r\n\r\n" <> body
  where
    body = "{\"marker\":\"wire-ingress-regression-ok\",\"upstream\":\"api\",\"certificate\":\"\"}"

decodeChunks :: [BS.ByteString] -> Either String BackendResponse
decodeChunks = go responseDecoder
  where
    go _ [] = Left "Truncated response"
    go decoder (chunk : chunks) =
      feedResponse decoder chunk >>= \case
        Await next -> go next chunks
        Complete response | all BS.null chunks -> Right response
        Complete _ -> Left "Trailing response bytes"

statusCondition :: Value -> Either String Bool
statusCondition = fmap (Kube.hasCondition "Ready") . parseEither (parseJSON @Kube.ResourceStatus)

statusResource :: Int -> Value
statusResource observed =
  object
    [ "metadata" .= object ["name" .= String "pod", "generation" .= (2 :: Int)],
      "status" .= object ["conditions" .= [object ["type" .= String "Ready", "status" .= String "True", "observedGeneration" .= observed]]]
    ]

decodeConfig :: [Pair] -> [Pair] -> Either String Config.ConnectionConfig
decodeConfig cluster user =
  parseEither Config.parseConfig $
    object
      [ "current-context" .= String "test",
        "contexts" .= [object ["name" .= String "test", "context" .= object ["cluster" .= String "test", "user" .= String "test"]]],
        "clusters" .= [object ["name" .= String "test", "cluster" .= object (["server" .= String "https://cluster.invalid", "certificate-authority-data" .= String "AQID"] <> cluster)]],
        "users" .= [object ["name" .= String "test", "user" .= object user]]
      ]
