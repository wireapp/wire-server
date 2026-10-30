{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
-- SPDX-License-Identifier: AGPL-3.0-only

module Testlib.Ingress where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import Control.Monad.Codensity
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (isAsciiLower, isDigit)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as Cipher
import Network.URI (unEscapeString)
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process
import System.Timeout
import qualified Testlib.Ingress.Kubernetes as Kube
import Testlib.Ingress.TLS (Expectation (..), Probe (..))
import qualified Testlib.Ingress.TLS as NativeTLS
import Prelude

-- The scenario space is a product of closed types, rather than string flags.
data GatewayOwnership = ChartGateway | ExternalGateway
  deriving stock (Eq, Show, Enum, Bounded, Generic)

data TLSProfile = DefaultTLS | CustomTLS12 | FIPS
  deriving stock (Eq, Show, Enum, Bounded, Generic)

data IngressMode = SingleDomain | MultipleDomains
  deriving stock (Eq, Show, Enum, Bounded, Generic)

data Upstream = API | Federation | Webapp | Extra | Companion | SecondaryAPI | SecondaryWebapp
  deriving stock (Eq, Show, Generic)

data Settings = Settings
  { kubernetes :: Kube.Client,
    namespacePrefix :: String,
    gatewayClass :: String,
    controllerNamespace :: String,
    xdsNameSchemeV2 :: Bool
  }

data Ingress = Ingress
  { kubernetes :: Kube.Client,
    target :: Kube.ForwardTarget,
    primaryCertificate :: NativeTLS.CertificateMaterial,
    secondaryCertificate :: NativeTLS.CertificateMaterial,
    upstreams :: [Upstream]
  }

apiHost, federationHost, webappHost, extraHost, companionHost :: String
apiHost = "api.ingress.example.test"
federationHost = "federation.example.test"
webappHost = "webapp.ingress.example.test"
extraHost = "extra.example.test"
companionHost = "companion.example.test"

hostname :: Upstream -> String
hostname = \case
  API -> apiHost
  Federation -> federationHost
  Webapp -> webappHost
  Extra -> extraHost
  Companion -> companionHost
  SecondaryAPI -> "api.other.example.test"
  SecondaryWebapp -> "webapp.other.example.test"

aes128, aes256, tls13AES :: TLS.Cipher
aes128 = Cipher.cipher_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256
aes256 = Cipher.cipher_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384
tls13AES = Cipher.cipher13_AES_128_GCM_SHA256

-- Every rejection is preceded by a working, certificate-verified HTTP request.
profileProbes :: TLSProfile -> [Probe]
profileProbes = \case
  DefaultTLS -> [Probe TLS.TLS13 tls13AES TLS.P256 Accept, Probe TLS.TLS12 aes128 TLS.P256 Reject]
  CustomTLS12 ->
    [ Probe TLS.TLS12 aes128 TLS.P256 Accept,
      Probe TLS.TLS12 aes256 TLS.P256 Reject,
      Probe TLS.TLS13 tls13AES TLS.P256 Reject
    ]
  FIPS ->
    [ Probe TLS.TLS13 tls13AES TLS.P256 Accept,
      Probe TLS.TLS12 aes128 TLS.P256 Accept,
      Probe TLS.TLS12 aes256 TLS.P256 Accept,
      -- TLS 1.2 supported_groups also constrains the P-256 certificate;
      -- TLS 1.3 separates certificate signatures from key agreement groups.
      Probe TLS.TLS13 tls13AES TLS.P384 Accept,
      Probe TLS.TLS13 Cipher.cipher13_CHACHA20_POLY1305_SHA256 TLS.P256 Reject,
      Probe TLS.TLS13 tls13AES TLS.X25519 Reject
    ]

scenarioName :: GatewayOwnership -> TLSProfile -> IngressMode -> String
scenarioName ownership profile mode =
  (case ownership of ChartGateway -> "managed"; ExternalGateway -> "external")
    <> "-"
    <> (case profile of DefaultTLS -> "default"; CustomTLS12 -> "tls12"; FIPS -> "fips")
    <> (case mode of SingleDomain -> ""; MultipleDomains -> "-multi")

ensure :: Bool -> String -> IO ()
ensure condition message = unless condition $ throwIO (userError message)

bounded :: Int -> IO a -> IO a
bounded seconds action = timeout (seconds * 1000000) action >>= maybe (throwIO $ userError "Command timed out") pure

-- Helm's Go template engine is the only subprocess; Kubernetes lifecycle and
-- transport remain native Haskell.
renderChart :: Kube.Client -> String -> FilePath -> IO ()
renderChart client namespace valuesFile = do
  (status, output, errors) <- bounded 60 $ readProcessWithExitCode "helm" ["template", "regression", "charts/wire-ingress", "-n", namespace, "-f", valuesFile] ""
  ensure (status == ExitSuccess) ("Helm rendering failed: " <> errors)
  manifests <- either (ioError . userError . Yaml.prettyPrintParseException) pure (Yaml.decodeAllEither' $ BS8.pack output)
  forM_ (filter (/= Null) manifests) $ apply client namespace

apply :: Kube.Client -> String -> Value -> IO ()
apply client namespace value = void $ Kube.create client (Just namespace) value

resource :: String -> String -> String -> Value -> Value
resource api kind name spec = object ["apiVersion" .= api, "kind" .= kind, "metadata" .= object ["name" .= name], "spec" .= spec]

gatewayResource :: String -> String -> Value -> Value
gatewayResource = resource "gateway.networking.k8s.io/v1"

-- Like withModifiedBackend, resource lifetime is continuation-scoped. Unlike a
-- Wire backend this fixture needs no Cassandra, RabbitMQ or service migrations.
withModifiedIngress :: Settings -> GatewayOwnership -> TLSProfile -> IngressMode -> (Ingress -> IO a) -> IO a
withModifiedIngress settings ownership profile mode action = runCodensity fixture action
  where
    client = settings.kubernetes
    fixture = do
      directory <- Codensity $ withSystemTempDirectory "wire-ingress-haskell-"
      namespace <- Codensity $ Kube.withNamespace client (settings.namespacePrefix <> "-" <> scenarioName ownership profile mode)
      primaryCertificate <- liftIO $ NativeTLS.generateCertificate (map hostname [API, Federation, Webapp, Extra, Companion])
      secondaryCertificate <- liftIO $ NativeTLS.generateCertificate (map hostname [SecondaryAPI, SecondaryWebapp])
      -- Diagnostics run while resources still exist, before bracket cleanup.
      Codensity $ \continue -> (runCodensity (deploy directory namespace primaryCertificate secondaryCertificate) continue) `onException` diagnostics client namespace
    deploy directory namespace primaryCertificate secondaryCertificate = do
      liftIO $ do
        installBackend client namespace primaryCertificate
        when (mode == MultipleDomains) $ installSecret client namespace "secondary-tls" secondaryCertificate
        when (ownership == ExternalGateway) $ installExternalGateway settings namespace
        let config = chartValues settings namespace ownership profile mode
            valuesFile = directory </> "values.json"
        LBS.writeFile valuesFile (encode config)
        renderChart client namespace valuesFile
        installRoute client namespace "extra" "regression-wire-ingress" "extra" extraHost
        Kube.waitProgrammed client namespace "gateways" "regression"
        Kube.waitProgrammed client namespace "listenersets" "regression-wire-ingress"
      extraUpstreams <-
        if profile /= FIPS
          then pure []
          else do
            companion <- Codensity $ Kube.withNamespace client (namespace <> "-companion")
            liftIO $ do
              installBackend client companion primaryCertificate
              apply client companion $
                gatewayResource "ListenerSet" "companion" $
                  object
                    [ "parentRef" .= object ["name" .= String "regression", "namespace" .= namespace, "kind" .= String "Gateway", "group" .= String "gateway.networking.k8s.io"],
                      "listeners" .= [object ["name" .= String "companion", "hostname" .= companionHost, "port" .= (443 :: Int), "protocol" .= String "HTTPS", "tls" .= object ["mode" .= String "Terminate", "certificateRefs" .= [object ["name" .= String "server-tls"]]]]]
                    ]
              installRoute client companion "companion" "companion" "companion" companionHost
              Kube.waitProgrammed client companion "listenersets" "companion"
            pure [Companion]
      target <- liftIO $ Kube.forwardTarget client settings.controllerNamespace namespace
      pure Ingress {kubernetes = client, target, primaryCertificate, secondaryCertificate, upstreams = (case mode of SingleDomain -> [API, Federation, Webapp, Extra]; MultipleDomains -> [API, Webapp, SecondaryAPI, SecondaryWebapp, Extra]) <> extraUpstreams}

chartValues :: Settings -> String -> GatewayOwnership -> TLSProfile -> IngressMode -> Value
chartValues settings namespace ownership profile mode =
  object
    [ "FIPS_202205_tls_profile" .= (profile == FIPS),
      "gateway"
        .= object
          ( [ "name" .= String "regression",
              "className" .= settings.gatewayClass,
              "create" .= (ownership == ChartGateway),
              "serviceType" .= String "ClusterIP",
              "envoyProxy" .= object ["replicas" .= (1 :: Int)],
              "listeners" .= object ["https" .= object ["hostname" .= String "*.ingress.example.test"]],
              "allowedListenerNamespaces" .= [namespace <> "-companion"],
              "extraHttpsListeners" .= [object ["name" .= String "extra", "hostname" .= extraHost]],
              "patchPolicies" .= object ["xdsNameSchemeV2" .= settings.xdsNameSchemeV2]
            ]
              <> ["tls" .= object ["minVersion" .= String "1.2", "maxVersion" .= String "1.2", "ciphers" .= [String "ECDHE-ECDSA-AES128-GCM-SHA256"], "ecdhCurves" .= [String "P-256"]] | profile == CustomTLS12]
          ),
      "config"
        .= ( case mode of
               SingleDomain -> object ["dns" .= object ["https" .= apiHost, "federator" .= federationHost, "webapp" .= webappHost]]
               MultipleDomains ->
                 object
                   [ "domains"
                       .= [ object ["name" .= String "primary", "base" .= String "ingress.example.test", "dns" .= object ["https" .= apiHost, "webapp" .= webappHost]],
                            object ["name" .= String "secondary", "base" .= String "other.example.test", "sectionName" .= String "secondary-https", "dns" .= object ["https" .= hostname SecondaryAPI, "webapp" .= hostname SecondaryWebapp], "tls" .= object ["secretName" .= String "secondary-tls"]]
                          ]
                   ]
           ),
      "tls" .= object ["secret" .= object ["create" .= False, "nameOverride" .= String "server-tls"]],
      "federator" .= object ["enabled" .= (mode == SingleDomain), "tls" .= object ["useCertManager" .= False, "secretName" .= String "server-tls"]],
      "webapp" .= object ["enabled" .= True],
      "websockets" .= object ["enabled" .= False],
      "fakeS3" .= object ["enabled" .= False]
    ]

installExternalGateway :: Settings -> String -> IO ()
installExternalGateway settings namespace = do
  apply settings.kubernetes namespace $
    resource "gateway.envoyproxy.io/v1alpha1" "EnvoyProxy" "regression" $
      object
        ["provider" .= object ["type" .= String "Kubernetes", "kubernetes" .= object ["envoyService" .= object ["type" .= String "ClusterIP"], "envoyDeployment" .= object ["replicas" .= (1 :: Int)]]]]
  apply settings.kubernetes namespace $
    gatewayResource "Gateway" "regression" $
      object
        [ "gatewayClassName" .= settings.gatewayClass,
          "infrastructure" .= object ["parametersRef" .= object ["group" .= String "gateway.envoyproxy.io", "kind" .= String "EnvoyProxy", "name" .= String "regression"]],
          "allowedListeners" .= object ["namespaces" .= object ["from" .= String "Selector", "selector" .= object ["matchExpressions" .= [object ["key" .= String "kubernetes.io/metadata.name", "operator" .= String "In", "values" .= [namespace, namespace <> "-companion"]]]]]],
          "listeners" .= [object ["name" .= String "placeholder", "port" .= (65535 :: Int), "protocol" .= String "HTTP", "allowedRoutes" .= object ["namespaces" .= object ["from" .= String "Selector", "selector" .= object ["matchExpressions" .= [object ["key" .= String "kubernetes.io/metadata.name", "operator" .= String "DoesNotExist"]]]]]]]
        ]

installSecret :: Kube.Client -> String -> String -> NativeTLS.CertificateMaterial -> IO ()
installSecret client namespace name material =
  apply client namespace $
    object
      [ "apiVersion" .= String "v1",
        "kind" .= String "Secret",
        "metadata" .= object ["name" .= name],
        "type" .= String "kubernetes.io/tls",
        "stringData" .= object ["tls.crt" .= Text.pack (BS8.unpack material.certificatePEM), "tls.key" .= Text.pack (BS8.unpack material.privateKeyPEM)]
      ]

installBackend :: Kube.Client -> String -> NativeTLS.CertificateMaterial -> IO ()
installBackend client namespace material = do
  installSecret client namespace "server-tls" material
  apply client namespace $ object ["apiVersion" .= String "v1", "kind" .= String "ConfigMap", "metadata" .= object ["name" .= String "federator-ca"], "data" .= object ["ca.crt" .= Text.pack (BS8.unpack material.certificatePEM)]]
  fixture <- Yaml.decodeFileThrow "charts/wire-ingress/tests/backend.yaml"
  items <- Kube.field @[Value] "items" fixture
  mapM_ (apply client namespace) items
  forM_ ["api", "federation", "webapp"] $ \name ->
    Kube.waitFor ("deployment/" <> name) $ do
      deployment <- Kube.get client ("/apis/apps/v1/namespaces/" <> Kube.segment namespace <> "/deployments/" <> name)
      status <- Kube.field @Value "status" deployment
      pure $ fromMaybe 0 (Kube.optionalField @Int "availableReplicas" status) >= 1

installRoute :: Kube.Client -> String -> String -> String -> String -> String -> IO ()
installRoute client namespace name listener section host =
  apply client namespace $
    gatewayResource "HTTPRoute" name $
      object
        [ "parentRefs" .= [object ["name" .= listener, "kind" .= String "ListenerSet", "sectionName" .= section]],
          "hostnames" .= [host],
          "rules" .= [object ["backendRefs" .= [object ["name" .= String "nginz", "port" .= (8080 :: Int)]]]]
        ]

probeIngress :: Ingress -> Upstream -> Probe -> Bool -> IO ()
probeIngress ingress upstream specification clientCertificate = bounded 15 $ do
  putStrLn $ "    " <> show upstream <> " " <> show specification <> if clientCertificate then " with client certificate" else ""
  let host = hostname upstream
      server = if upstream `elem` [SecondaryAPI, SecondaryWebapp] then ingress.secondaryCertificate else ingress.primaryCertificate
      client = if clientCertificate then Just ingress.primaryCertificate else Nothing
      authority = host <> if clientCertificate then "." else ""
  outcome <- Kube.withForward ingress.kubernetes ingress.target $ \backend -> NativeTLS.probe backend server client host authority specification
  ensure (NativeTLS.checkOutcome specification outcome) ("Unexpected TLS result for " <> host <> ": " <> show outcome)
  case outcome of
    NativeTLS.PeerRejected _ -> pure ()
    NativeTLS.Negotiated _ value -> do
      marker <- Kube.field @String "marker" value
      ensure (marker == "wire-ingress-regression-ok") "Unexpected ingress response"
      actual <- Kube.field @String "upstream" value
      let expected = case upstream of Federation -> "federation"; Webapp -> "webapp"; SecondaryWebapp -> "webapp"; _ -> "api"
      ensure (actual == expected) "Ingress routed to the wrong upstream"
      when (upstream == Federation) $ do
        forwarded <- Kube.field @String "certificate" value
        let expectedCertificate = if clientCertificate then BS8.unpack ingress.primaryCertificate.certificatePEM else ""
        ensure (unEscapeString forwarded == expectedCertificate) "Wrong forwarded federation certificate"

-- Retry only the first positive probe while Envoy converges, never rejection
-- assertions (which could otherwise hide an intermittently permissive policy).
eventually :: IO a -> IO a
eventually = go (30 :: Int)
  where
    go remaining action =
      action `catch` \(err :: IOException) ->
        if remaining == 0 then throwIO err else threadDelay 1000000 >> go (remaining - 1) action

checkProfile :: TLSProfile -> Ingress -> IO ()
checkProfile profile ingress = forM_ ingress.upstreams $ \upstream -> do
  case profileProbes profile of
    [] -> throwIO $ userError "Profile has no probes"
    first : rest -> do
      eventually $ probeIngress ingress upstream first False
      mapM_ (\probe -> probeIngress ingress upstream probe False) rest
      when (upstream == Federation) $ probeIngress ingress upstream first True
  putStrLn $ "  PASS " <> show upstream <> ": " <> show profile

diagnostics :: Kube.Client -> String -> IO ()
diagnostics client namespace = forM_ ["/api/v1/", "/apis/gateway.networking.k8s.io/v1/", "/apis/gateway.envoyproxy.io/v1alpha1/"] $ \api -> do
  let kinds = case api of
        "/api/v1/" -> ["pods", "events"]
        "/apis/gateway.networking.k8s.io/v1/" -> ["gateways", "listenersets", "httproutes"]
        _ -> ["clienttrafficpolicies", "envoypatchpolicies"]
  forM_ kinds $ \kind -> do
    result <- try @IOException $ Kube.get client (api <> "namespaces/" <> Kube.segment namespace <> "/" <> kind)
    case result of
      Left _ -> putStrLn ("Could not read diagnostics for " <> kind)
      Right value -> do
        items <- Kube.field @[Value] "items" value
        forM_ items $ \item ->
          LBS.putStrLn $
            encode $
              object
                [ "kind" .= kind,
                  "name" .= (Kube.optionalField @Value "metadata" item >>= Kube.optionalField @String "name"),
                  "status" .= Kube.optionalField @Value "status" item,
                  "reason" .= Kube.optionalField @String "reason" item,
                  "message" .= Kube.optionalField @String "message" item
                ]

readSettings :: IO Settings
readSettings = do
  kubernetes <- Kube.loadClient
  namespacePrefix <- environment "WIRE_INGRESS_TEST_NAMESPACE" "test-wire-ingress-h"
  ensure (not (null namespacePrefix) && length namespacePrefix <= 28 && all (\c -> isAsciiLower c || isDigit c || c == '-') namespacePrefix && head namespacePrefix /= '-' && last namespacePrefix /= '-') "Namespace prefix must be a DNS label of at most 28 characters"
  gatewayClass <- environment "WIRE_INGRESS_GATEWAY_CLASS" "envoy"
  controllerNamespace <- environment "ENVOY_GATEWAY_NAMESPACE" "envoy-gateway-system"
  scheme <- environment "WIRE_INGRESS_XDS_NAME_SCHEME_V2" "false"
  ensure (scheme `elem` ["true", "false"]) "WIRE_INGRESS_XDS_NAME_SCHEME_V2 must be true or false"
  pure Settings {kubernetes, namespacePrefix, gatewayClass, controllerNamespace, xdsNameSchemeV2 = scheme == "true"}
  where
    environment name fallback = fromMaybe fallback <$> lookupEnv name

runIngressTests :: IO ()
runIngressTests = do
  hSetBuffering stdout LineBuffering
  settings <- readSettings
  void $ Kube.get settings.kubernetes ("/apis/gateway.networking.k8s.io/v1/gatewayclasses/" <> Kube.segment settings.gatewayClass)
  forM_ [minBound .. maxBound] $ \ownership ->
    forM_ [minBound .. maxBound] $ \profile ->
      forM_ [minBound .. maxBound] $ \mode -> do
        let name = scenarioName ownership profile mode
        putStrLn $ "RUN " <> name
        withModifiedIngress settings ownership profile mode (checkProfile profile)
        putStrLn $ "PASS " <> name
