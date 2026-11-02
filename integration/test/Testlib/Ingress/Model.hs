{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
-- SPDX-License-Identifier: AGPL-3.0-only

-- | The test matrix is data. No cluster access or resource lifetime lives here.
module Testlib.Ingress.Model
  ( GatewayOwnership (..),
    TLSProfile (..),
    IngressMode (..),
    Scenario (..),
    Domain (..),
    Endpoint (..),
    Settings (..),
    ProbePlan (..),
    scenarios,
    scenarioName,
    endpoints,
    hostname,
    endpointDomain,
    upstreamName,
    certificateHosts,
    profilePlan,
    validateNamespacePrefix,
    validateBackendResponse,
  )
where

import Control.Monad (unless, when)
import Data.Char (isAsciiLower, isDigit)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as Cipher
import Network.URI (unEscapeString)
import Testlib.Ingress.TLS (BackendResponse (..), Parameters (..), Probe (..))
import Prelude

data GatewayOwnership = ChartGateway | ExternalGateway
  deriving stock (Eq, Show, Enum, Bounded)

data TLSProfile = DefaultTLS | CustomTLS12 | FIPS
  deriving stock (Eq, Show, Enum, Bounded)

data IngressMode = SingleDomain | MultipleDomains
  deriving stock (Eq, Show, Enum, Bounded)

data Scenario = Scenario
  { ownership :: GatewayOwnership,
    profile :: TLSProfile,
    mode :: IngressMode
  }
  deriving stock (Eq, Show)

data Domain = Primary | Secondary
  deriving stock (Eq, Show)

data Endpoint = API Domain | Webapp Domain | Federation | Extra | Companion
  deriving stock (Eq, Show)

data Settings = Settings
  { namespacePrefix :: String,
    gatewayClass :: String,
    controllerNamespace :: String,
    xdsNameSchemeV2 :: Bool
  }

-- A plan always starts by proving connectivity with an accepted handshake.
-- Rejection checks cannot be used as readiness checks, even accidentally.
data ProbePlan = ProbePlan
  { connectivity :: Parameters,
    assertions :: [Probe]
  }
  deriving stock (Eq, Show)

scenarios :: [Scenario]
scenarios = Scenario <$> [minBound .. maxBound] <*> [minBound .. maxBound] <*> [minBound .. maxBound]

scenarioName :: Scenario -> String
scenarioName scenario =
  (case scenario.ownership of ChartGateway -> "managed"; ExternalGateway -> "external")
    <> "-"
    <> (case scenario.profile of DefaultTLS -> "default"; CustomTLS12 -> "tls12"; FIPS -> "fips")
    <> (case scenario.mode of SingleDomain -> ""; MultipleDomains -> "-multi")

endpoints :: Scenario -> NonEmpty Endpoint
endpoints scenario = API Primary :| (routes <> [Companion | scenario.profile == FIPS])
  where
    routes = case scenario.mode of
      SingleDomain -> [Federation, Webapp Primary, Extra]
      MultipleDomains -> [Webapp Primary, API Secondary, Webapp Secondary, Extra]

hostname :: Endpoint -> String
hostname = \case
  API Primary -> "api.ingress.example.test"
  API Secondary -> "api.other.example.test"
  Webapp Primary -> "webapp.ingress.example.test"
  Webapp Secondary -> "webapp.other.example.test"
  Federation -> "federation.example.test"
  Extra -> "extra.example.test"
  Companion -> "companion.example.test"

endpointDomain :: Endpoint -> Domain
endpointDomain = \case
  API domain -> domain
  Webapp domain -> domain
  _ -> Primary

upstreamName :: Endpoint -> Text
upstreamName = \case
  Federation -> "federation"
  Webapp _ -> "webapp"
  _ -> "api"

certificateHosts :: Domain -> NonEmpty String
certificateHosts =
  fmap hostname . \case
    Primary -> API Primary :| [Federation, Webapp Primary, Extra, Companion]
    Secondary -> API Secondary :| [Webapp Secondary]

profilePlan :: TLSProfile -> ProbePlan
profilePlan = \case
  DefaultTLS -> ProbePlan tls13 [Reject tls12]
  CustomTLS12 -> ProbePlan tls12 [Reject tls12AES256, Reject tls13]
  FIPS ->
    ProbePlan
      tls13
      [ Accept tls12,
        Accept tls12AES256,
        -- TLS 1.2 also constrains the certificate's curve; TLS 1.3 separates it
        -- from the key agreement group, so P-384 is tested with TLS 1.3.
        Accept tls13 {group = TLS.P384},
        Reject tls13 {cipher = Cipher.cipher13_CHACHA20_POLY1305_SHA256},
        Reject tls13 {group = TLS.X25519}
      ]
  where
    tls13 = Parameters TLS.TLS13 Cipher.cipher13_AES_128_GCM_SHA256 TLS.P256
    tls12 = Parameters TLS.TLS12 Cipher.cipher_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256 TLS.P256
    tls12AES256 = tls12 {cipher = Cipher.cipher_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384}

validateNamespacePrefix :: String -> Either String String
validateNamespacePrefix prefix
  | null prefix || length prefix > 28 = invalid
  | any (\c -> not (isAsciiLower c || isDigit c || c == '-')) prefix = invalid
  | take 1 prefix == "-" || take 1 (reverse prefix) == "-" = invalid
  | otherwise = Right prefix
  where
    invalid = Left "Namespace prefix must be a DNS label of at most 28 characters"

-- TLS establishes the peer; the fixture response establishes the route and
-- verifies that federation's forwarded certificate came from that connection.
validateBackendResponse :: Endpoint -> Maybe Text -> BackendResponse -> Either String ()
validateBackendResponse endpoint clientCertificate response = do
  unless (response.marker == "wire-ingress-regression-ok") $ Left "Unexpected ingress response marker"
  unless (response.upstream == upstreamName endpoint) $ Left "Ingress routed to the wrong upstream"
  when (endpoint == Federation) $
    unless (Text.pack (unEscapeString $ Text.unpack response.certificate) == fromMaybe "" clientCertificate) $
      Left "Wrong forwarded federation certificate"
