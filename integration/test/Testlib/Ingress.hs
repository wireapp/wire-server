{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
-- SPDX-License-Identifier: AGPL-3.0-only

module Testlib.Ingress (runIngressTests, withModifiedIngress, Ingress, checkProfile) where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import Control.Monad.Codensity
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Yaml as Yaml
import qualified Network.TLS as TLS
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import qualified Testlib.Ingress.Kubernetes as Kube
import qualified Testlib.Ingress.Manifests as Manifest
import Testlib.Ingress.Model
import Testlib.Ingress.TLS (Probe (..))
import qualified Testlib.Ingress.TLS as NativeTLS
import Prelude

data Target = Target
  { endpoint :: Endpoint,
    identity :: NativeTLS.CertificateMaterial
  }

-- A test needs a scoped connection and its targets, not the Kubernetes client
-- or the mechanics of locating a proxy pod.
data Ingress = Ingress
  { withConnection :: forall a. (TLS.Backend -> IO a) -> IO a,
    targets :: NonEmpty Target
  }

newtype AssertionFailure = AssertionFailure String
  deriving stock (Show)

instance Exception AssertionFailure

expect :: Either String a -> IO a
expect = either (throwIO . AssertionFailure) pure

bounded :: Int -> String -> IO a -> IO a
bounded seconds description action =
  timeout (seconds * 1000000) action >>= maybe (ioError $ userError $ "Timed out: " <> description) pure

-- Helm is only the chart's template interpreter. Feed values on stdin rather
-- than creating a temporary file, then apply the resulting resources natively.
renderChart :: String -> Value -> IO [Value]
renderChart namespace values = do
  let input = Text.unpack $ Text.decodeUtf8 $ LBS.toStrict $ encode values
  (status, output, errors) <-
    bounded 60 "Helm rendering" $
      readProcessWithExitCode "helm" ["template", "regression", "charts/wire-ingress", "-n", namespace, "-f", "-"] input
  unless (status == ExitSuccess) $ throwIO (AssertionFailure $ "Helm rendering failed: " <> errors)
  filter (/= Null) <$> expect (either (Left . Yaml.prettyPrintParseException) Right $ Yaml.decodeAllEither' $ Text.encodeUtf8 $ Text.pack output)

-- Codensity composes the same continuation-scoped lifetimes as
-- withModifiedBackend. Diagnostics run inside each namespace's lifetime.
withModifiedIngress :: Kube.Client -> Settings -> Scenario -> (Ingress -> IO a) -> IO a
withModifiedIngress client settings scenario = runCodensity fixture
  where
    scopedNamespace name = Codensity $ \continue ->
      Kube.withNamespace client name $ \namespace -> continue namespace `onException` Kube.diagnostics client namespace
    fixture = do
      namespace <- scopedNamespace (settings.namespacePrefix <> "-" <> scenarioName scenario)
      primary <- liftIO $ NativeTLS.generateCertificate (certificateHosts Primary)
      secondary <- liftIO $ NativeTLS.generateCertificate (certificateHosts Secondary)
      liftIO $ do
        installBackend client namespace primary
        when (scenario.mode == MultipleDomains) $ Kube.apply client namespace (Manifest.tlsSecret "secondary-tls" secondary)
        when (scenario.ownership == ExternalGateway) $
          traverse_ (Kube.apply client namespace) (Manifest.externalGateway settings namespace)
        manifests <- renderChart namespace (Manifest.chartValues settings namespace scenario)
        traverse_ (Kube.apply client namespace) manifests
        Kube.apply client namespace Manifest.extraRoute
        Kube.waitProgrammed client namespace "gateways" "regression"
        Kube.waitProgrammed client namespace "listenersets" "regression-wire-ingress"
      when (scenario.profile == FIPS) $ do
        companion <- scopedNamespace (namespace <> "-companion")
        liftIO $ do
          installBackend client companion primary
          Kube.apply client companion (Manifest.companionListener namespace)
          Kube.apply client companion Manifest.companionRoute
          Kube.waitProgrammed client companion "listenersets" "companion"
      target <- liftIO $ Kube.forwardTarget client settings.controllerNamespace namespace
      let identityFor = \case Primary -> primary; Secondary -> secondary
      pure
        Ingress
          { withConnection = Kube.withForward client target,
            targets = (\endpoint -> Target endpoint (identityFor $ endpointDomain endpoint)) <$> endpoints scenario
          }

installBackend :: Kube.Client -> String -> NativeTLS.CertificateMaterial -> IO ()
installBackend client namespace identity = do
  traverse_ (Kube.apply client namespace) [Manifest.tlsSecret "server-tls" identity, Manifest.caBundle identity]
  fixture <- Yaml.decodeFileThrow "charts/wire-ingress/tests/backend.yaml"
  items <- expect $ parseEither (withObject "backend fixture" (.: "items")) fixture
  traverse_ (Kube.apply client namespace) (items :: [Value])
  traverse_ (Kube.waitDeployment client namespace) ["api", "federation", "webapp"]

checkProfile :: TLSProfile -> Ingress -> IO ()
checkProfile profile ingress = forM_ ingress.targets $ \target -> do
  let plan = profilePlan profile
      host = hostname target.endpoint
      request = NativeTLS.Request host host Nothing
      check = checkProbe ingress target
  eventually $ check request (Accept plan.connectivity)
  traverse_ (check request) plan.assertions
  case target.endpoint of
    Federation -> check request {NativeTLS.authority = host <> ".", NativeTLS.clientIdentity = Just target.identity} (Accept plan.connectivity)
    _ -> pure ()
  putStrLn $ "  PASS " <> show target.endpoint <> ": " <> show profile

checkProbe :: Ingress -> Target -> NativeTLS.Request -> Probe -> IO ()
checkProbe ingress target request specification = bounded 15 "TLS probe" $ do
  putStrLn $ "    " <> show target.endpoint <> " " <> show specification
  let parameters = case specification of Accept p -> p; Reject p -> p
  outcome <- withConnection ingress $ \backend -> NativeTLS.probe backend target.identity request parameters
  expect (NativeTLS.validateHandshake specification outcome)
  case outcome of
    NativeTLS.PeerRejected _ -> pure ()
    NativeTLS.Established _ response ->
      expect $
        validateBackendResponse target.endpoint (Text.decodeUtf8 . NativeTLS.certificatePEM <$> request.clientIdentity) response

-- Only connectivity gets retries. Assertions and negative probes fail once.
eventually :: IO a -> IO a
eventually action = bounded 60 "Envoy readiness" loop
  where
    loop = action `catch` \(_ :: IOException) -> threadDelay 1000000 >> loop

readSettings :: IO Settings
readSettings = do
  namespacePrefix <- environment "WIRE_INGRESS_TEST_NAMESPACE" "test-wire-ingress-h" >>= expect . validateNamespacePrefix
  gatewayClass <- environment "WIRE_INGRESS_GATEWAY_CLASS" "envoy"
  controllerNamespace <- environment "ENVOY_GATEWAY_NAMESPACE" "envoy-gateway-system"
  scheme <- environment "WIRE_INGRESS_XDS_NAME_SCHEME_V2" "false"
  xdsNameSchemeV2 <- expect $ case scheme of
    "true" -> Right True
    "false" -> Right False
    _ -> Left "WIRE_INGRESS_XDS_NAME_SCHEME_V2 must be true or false"
  pure Settings {namespacePrefix, gatewayClass, controllerNamespace, xdsNameSchemeV2}
  where
    environment name fallback = fromMaybe fallback <$> lookupEnv name

runIngressTests :: IO ()
runIngressTests = do
  hSetBuffering stdout LineBuffering
  settings <- readSettings
  client <- Kube.loadClient
  Kube.checkGatewayClass client settings.gatewayClass
  forM_ scenarios $ \scenario -> do
    let name = scenarioName scenario
    putStrLn $ "RUN " <> name
    withModifiedIngress client settings scenario (checkProfile scenario.profile)
    putStrLn $ "PASS " <> name
