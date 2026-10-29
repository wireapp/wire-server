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
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (isAsciiLower, isDigit)
import Data.List (intercalate, isInfixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Network.URI (unEscapeString)
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process
import System.Timeout
import Text.Read (readMaybe)
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

data TLSVersion = TLS12 | TLS13
  deriving stock (Eq, Show)

data Expectation = Accept | Reject
  deriving stock (Eq, Show)

data Probe = Probe
  { version :: TLSVersion,
    cipher :: String,
    group :: String,
    expectation :: Expectation
  }
  deriving stock (Eq, Show)

data Settings = Settings
  { namespacePrefix :: String,
    gatewayClass :: String,
    controllerNamespace :: String,
    xdsNameSchemeV2 :: Bool
  }

data Ingress = Ingress
  { port :: String,
    certificateDirectory :: FilePath,
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

aes128, aes256, tls13AES :: String
aes128 = "ECDHE-ECDSA-AES128-GCM-SHA256"
aes256 = "ECDHE-ECDSA-AES256-GCM-SHA384"
tls13AES = "TLS_AES_128_GCM_SHA256"

-- Every rejection is preceded by a working, certificate-verified HTTP request.
profileProbes :: TLSProfile -> [Probe]
profileProbes = \case
  DefaultTLS -> [Probe TLS13 tls13AES "P-256" Accept, Probe TLS12 aes128 "P-256" Reject]
  CustomTLS12 ->
    [ Probe TLS12 aes128 "P-256" Accept,
      Probe TLS12 aes256 "P-256" Reject,
      Probe TLS13 tls13AES "P-256" Reject
    ]
  FIPS ->
    [ Probe TLS13 tls13AES "P-256" Accept,
      Probe TLS12 aes128 "P-256" Accept,
      Probe TLS12 aes256 "P-256" Accept,
      -- TLS 1.2 supported_groups also constrains the P-256 certificate;
      -- TLS 1.3 separates certificate signatures from key agreement groups.
      Probe TLS13 tls13AES "P-384" Accept,
      Probe TLS13 "TLS_CHACHA20_POLY1305_SHA256" "P-256" Reject,
      Probe TLS13 tls13AES "X25519" Reject
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

commandResult :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
commandResult executable arguments input = bounded 200 $ readProcessWithExitCode executable arguments input

command :: FilePath -> [String] -> String -> IO String
command executable arguments input = do
  (status, output, errors) <- commandResult executable arguments input
  ensure (status == ExitSuccess) $ unwords (executable : arguments) <> "\n" <> output <> errors
  pure output

kubectl :: [String] -> IO String
kubectl arguments = command "kubectl" ("--request-timeout=20s" : arguments) ""

apply :: String -> Value -> IO ()
apply namespace value = void $ command "kubectl" ["--request-timeout=20s", "-n", namespace, "apply", "-f", "-"] (LBS.unpack $ encode value)

resource :: String -> String -> String -> Value -> Value
resource api kind name spec = object ["apiVersion" .= api, "kind" .= kind, "metadata" .= object ["name" .= name], "spec" .= spec]

gatewayResource :: String -> String -> Value -> Value
gatewayResource = resource "gateway.networking.k8s.io/v1"

-- Like withModifiedBackend, resource lifetime is continuation-scoped. Unlike a
-- Wire backend this fixture needs no Cassandra, RabbitMQ or service migrations.
withNamespace :: String -> Codensity IO String
withNamespace namespace = Codensity $ bracket acquire release
  where
    acquire = kubectl ["create", "namespace", namespace] >> pure namespace
    release ns = void $ kubectl ["delete", "namespace", ns, "--wait=false"]

withModifiedIngress :: Settings -> GatewayOwnership -> TLSProfile -> IngressMode -> (Ingress -> IO a) -> IO a
withModifiedIngress settings ownership profile mode action = runCodensity fixture action
  where
    fixture = do
      directory <- Codensity $ withSystemTempDirectory "wire-ingress-haskell-"
      namespace <- withNamespace (settings.namespacePrefix <> "-" <> scenarioName ownership profile mode)
      liftIO $ createCertificate directory
      -- Diagnostics run while resources still exist, before bracket cleanup.
      Codensity $ \continue -> (runCodensity (deploy directory namespace) continue) `onException` diagnostics namespace
    deploy directory namespace = do
      liftIO $ do
        installBackend namespace directory
        when (mode == MultipleDomains) $ void $ kubectl ["-n", namespace, "create", "secret", "tls", "secondary-tls", "--cert", directory </> "secondary-cert.pem", "--key", directory </> "secondary-key.pem"]
        when (ownership == ExternalGateway) $ installExternalGateway settings namespace
        let config = chartValues settings namespace ownership profile mode
            valuesFile = directory </> "values.json"
        LBS.writeFile valuesFile (encode config)
        void $ command "helm" ["install", "regression", "charts/wire-ingress", "-n", namespace, "-f", valuesFile] ""
        installRoute namespace "extra" "regression-wire-ingress" "extra" extraHost
        waitProgrammed namespace "gateway/regression"
        waitProgrammed namespace "listenerset/regression-wire-ingress"
      extraUpstreams <-
        if profile /= FIPS
          then pure []
          else do
            companion <- withNamespace (namespace <> "-companion")
            liftIO $ do
              installBackend companion directory
              apply companion $
                gatewayResource "ListenerSet" "companion" $
                  object
                    [ "parentRef" .= object ["name" .= String "regression", "namespace" .= namespace, "kind" .= String "Gateway", "group" .= String "gateway.networking.k8s.io"],
                      "listeners" .= [object ["name" .= String "companion", "hostname" .= companionHost, "port" .= (443 :: Int), "protocol" .= String "HTTPS", "tls" .= object ["mode" .= String "Terminate", "certificateRefs" .= [object ["name" .= String "server-tls"]]]]]
                    ]
              installRoute companion "companion" "companion" "companion" companionHost
              waitProgrammed companion "listenerset/companion"
            pure [Companion]
      port <- Codensity $ withPortForward settings namespace
      pure Ingress {port, certificateDirectory = directory, upstreams = (case mode of SingleDomain -> [API, Federation, Webapp, Extra]; MultipleDomains -> [API, Webapp, SecondaryAPI, SecondaryWebapp, Extra]) <> extraUpstreams}

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
              <> ["tls" .= object ["minVersion" .= String "1.2", "maxVersion" .= String "1.2", "ciphers" .= [aes128], "ecdhCurves" .= [String "P-256"]] | profile == CustomTLS12]
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
  apply namespace $
    resource "gateway.envoyproxy.io/v1alpha1" "EnvoyProxy" "regression" $
      object
        ["provider" .= object ["type" .= String "Kubernetes", "kubernetes" .= object ["envoyService" .= object ["type" .= String "ClusterIP"], "envoyDeployment" .= object ["replicas" .= (1 :: Int)]]]]
  apply namespace $
    gatewayResource "Gateway" "regression" $
      object
        [ "gatewayClassName" .= settings.gatewayClass,
          "infrastructure" .= object ["parametersRef" .= object ["group" .= String "gateway.envoyproxy.io", "kind" .= String "EnvoyProxy", "name" .= String "regression"]],
          "allowedListeners" .= object ["namespaces" .= object ["from" .= String "Selector", "selector" .= object ["matchExpressions" .= [object ["key" .= String "kubernetes.io/metadata.name", "operator" .= String "In", "values" .= [namespace, namespace <> "-companion"]]]]]],
          "listeners" .= [object ["name" .= String "placeholder", "port" .= (65535 :: Int), "protocol" .= String "HTTP", "allowedRoutes" .= object ["namespaces" .= object ["from" .= String "Selector", "selector" .= object ["matchExpressions" .= [object ["key" .= String "kubernetes.io/metadata.name", "operator" .= String "DoesNotExist"]]]]]]]
        ]

createCertificate :: FilePath -> IO ()
createCertificate directory = do
  create "" [API, Federation, Webapp, Extra, Companion]
  create "secondary-" [SecondaryAPI, SecondaryWebapp]
  where
    create prefix hosts =
      void $
        command
          "openssl"
          [ "req",
            "-x509",
            "-newkey",
            "ec",
            "-pkeyopt",
            "ec_paramgen_curve:P-256",
            "-nodes",
            "-days",
            "1",
            "-subj",
            "/CN=wire-ingress-regression",
            "-addext",
            "subjectAltName=" <> intercalate "," (map (("DNS:" <>) . hostname) hosts),
            "-keyout",
            directory </> (prefix <> "key.pem"),
            "-out",
            directory </> (prefix <> "cert.pem")
          ]
          ""

installBackend :: String -> FilePath -> IO ()
installBackend namespace directory = do
  void $ kubectl ["-n", namespace, "create", "secret", "tls", "server-tls", "--cert", directory </> "cert.pem", "--key", directory </> "key.pem"]
  void $ kubectl ["-n", namespace, "create", "configmap", "federator-ca", "--from-file=ca.crt=" <> (directory </> "cert.pem")]
  void $ kubectl ["-n", namespace, "apply", "-f", "charts/wire-ingress/tests/backend.yaml"]
  void $ kubectl ["-n", namespace, "rollout", "status", "deployment", "--timeout=180s"]

installRoute :: String -> String -> String -> String -> String -> IO ()
installRoute namespace name listener section host =
  apply namespace $
    gatewayResource "HTTPRoute" name $
      object
        [ "parentRefs" .= [object ["name" .= listener, "kind" .= String "ListenerSet", "sectionName" .= section]],
          "hostnames" .= [host],
          "rules" .= [object ["backendRefs" .= [object ["name" .= String "nginz", "port" .= (8080 :: Int)]]]]
        ]

waitProgrammed :: String -> String -> IO ()
waitProgrammed namespace name = void $ kubectl ["-n", namespace, "wait", name, "--for=condition=Programmed", "--timeout=180s"]

withPortForward :: Settings -> String -> (String -> IO a) -> IO a
withPortForward settings namespace action = do
  let selector = "gateway.envoyproxy.io/owning-gateway-namespace=" <> namespace <> ",gateway.envoyproxy.io/owning-gateway-name=regression"
  services <- words <$> kubectl ["-n", settings.controllerNamespace, "get", "service", "-l", selector, "-o", "jsonpath={.items[*].metadata.name}"]
  service <- case services of
    [service] -> pure service
    _ -> throwIO $ userError ("Expected one dedicated proxy Service, got " <> show services)
  withCreateProcess (proc "kubectl" ["-n", settings.controllerNamespace, "port-forward", "service/" <> service, ":443", "--address=127.0.0.1"]) {std_out = CreatePipe} $ \_ output _ _ -> do
    outputHandle <- maybe (throwIO $ userError "Missing port-forward stdout") pure output
    line <- bounded 30 $ hGetLine outputHandle
    let parsed = stripPrefix "Forwarding from 127.0.0.1:" line >>= readMaybe @Int . takeWhile isDigit
    port <- maybe (throwIO $ userError ("Unexpected port-forward output: " <> line)) (pure . show) parsed
    action port

-- Keep transport failures separate from genuine server-side TLS rejection.
checkHandshake :: Probe -> (ExitCode, String, String) -> Either String ()
checkHandshake probe (status, output, errors)
  | valid = Right ()
  | otherwise = Left (show probe <> "\n" <> output <> errors)
  where
    combined = output <> errors
    contains = (`isInfixOf` combined)
    valid = case probe.expectation of
      Accept -> status == ExitSuccess && all contains ["CONNECTION ESTABLISHED", "Verification: OK", "HTTP/1.1 200", "wire-ingress-regression-ok", "Protocol version: " <> (case probe.version of TLS12 -> "TLSv1.2"; TLS13 -> "TLSv1.3"), "Ciphersuite: " <> probe.cipher]
      Reject -> status /= ExitSuccess && not (contains "CONNECTION ESTABLISHED") && any contains ["alert handshake failure", "alert protocol version", "alert insufficient security"]

probeIngress :: Ingress -> Upstream -> Probe -> Bool -> IO ()
probeIngress ingress upstream probe clientCertificate = do
  let directory = ingress.certificateDirectory
      host = hostname upstream
      flags = case probe.version of TLS12 -> ["-tls1_2", "-cipher"]; TLS13 -> ["-tls1_3", "-ciphersuites"]
      clientFlags = if clientCertificate then ["-cert", directory </> "cert.pem", "-key", directory </> "key.pem"] else []
      request = "GET / HTTP/1.1\r\nHost: " <> host <> (if clientCertificate then "." else "") <> "\r\nX-SSL-Certificate: spoofed\r\nConnection: close\r\n\r\n"
  result@(_, output, _) <-
    bounded 10 $
      commandResult
        "openssl"
        ( [ "s_client",
            "-connect",
            "127.0.0.1:" <> ingress.port,
            "-servername",
            host,
            "-verify_hostname",
            host,
            "-verify_return_error",
            "-CAfile",
            directory </> (if upstream `elem` [SecondaryAPI, SecondaryWebapp] then "secondary-cert.pem" else "cert.pem"),
            "-brief",
            "-ign_eof",
            "-ignore_unexpected_eof"
          ]
            <> flags
            <> [probe.cipher, "-groups", probe.group]
            <> clientFlags
        )
        request
  either (throwIO . userError) pure $ checkHandshake probe result
  when (probe.expectation == Accept) $ do
    let body = unlines $ drop 1 $ dropWhile (not . (`elem` ["", "\r"])) $ lines output
    value <- either (throwIO . userError) pure (eitherDecode @Value $ LBS.pack body)
    let expectedUpstream = case upstream of Federation -> "federation"; Webapp -> "webapp"; SecondaryWebapp -> "webapp"; _ -> "api"
    ensure (field "upstream" value == Just expectedUpstream) ("Wrong upstream: " <> body)
    when (upstream == Federation) $ do
      expectedCertificate <- if clientCertificate then readFile (directory </> "cert.pem") else pure ""
      ensure ((unEscapeString <$> field "certificate" value) == Just expectedCertificate) ("Wrong forwarded certificate: " <> body)
  where
    field key (Object fields) = case KeyMap.lookup key fields of Just (String text) -> Just (Text.unpack text); _ -> Nothing
    field _ _ = Nothing

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

diagnostics :: String -> IO ()
diagnostics namespace = forM_ ["gateway", "listenerset", "httproute", "clienttrafficpolicy", "envoypatchpolicy", "pods", "events"] $ \kind -> do
  (_, output, errors) <- commandResult "kubectl" ["--request-timeout=20s", "-n", namespace, "get", kind, "-o", "yaml"] ""
  putStrLn (output <> errors)

readSettings :: IO Settings
readSettings = do
  namespacePrefix <- environment "WIRE_INGRESS_TEST_NAMESPACE" "test-wire-ingress-h"
  ensure (not (null namespacePrefix) && length namespacePrefix <= 28 && all (\c -> isAsciiLower c || isDigit c || c == '-') namespacePrefix && head namespacePrefix /= '-' && last namespacePrefix /= '-') "Namespace prefix must be a DNS label of at most 28 characters"
  gatewayClass <- environment "WIRE_INGRESS_GATEWAY_CLASS" "envoy"
  controllerNamespace <- environment "ENVOY_GATEWAY_NAMESPACE" "envoy-gateway-system"
  scheme <- environment "WIRE_INGRESS_XDS_NAME_SCHEME_V2" "false"
  ensure (scheme `elem` ["true", "false"]) "WIRE_INGRESS_XDS_NAME_SCHEME_V2 must be true or false"
  pure Settings {namespacePrefix, gatewayClass, controllerNamespace, xdsNameSchemeV2 = scheme == "true"}
  where
    environment name fallback = fromMaybe fallback <$> lookupEnv name

runIngressTests :: IO ()
runIngressTests = do
  hSetBuffering stdout LineBuffering
  settings <- readSettings
  void $ kubectl ["get", "gatewayclass", settings.gatewayClass]
  forM_ [minBound .. maxBound] $ \ownership ->
    forM_ [minBound .. maxBound] $ \profile ->
      forM_ [minBound .. maxBound] $ \mode -> do
        let name = scenarioName ownership profile mode
        putStrLn $ "RUN " <> name
        withModifiedIngress settings ownership profile mode (checkProfile profile)
        putStrLn $ "PASS " <> name
