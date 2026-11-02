{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
-- SPDX-License-Identifier: AGPL-3.0-only

-- | Serialization at the Helm/Kubernetes boundary; no resource effects.
module Testlib.Ingress.Manifests (chartValues, externalGateway, extraRoute, companionRoute, companionListener, tlsSecret, caBundle) where

import Data.Aeson
import qualified Data.Text.Encoding as Text
import Testlib.Ingress.Model
import qualified Testlib.Ingress.TLS as TLS
import Prelude

resource :: String -> String -> String -> Value -> Value
resource api kind name spec = object ["apiVersion" .= api, "kind" .= kind, "metadata" .= object ["name" .= name], "spec" .= spec]

gatewayResource :: String -> String -> Value -> Value
gatewayResource = resource "gateway.networking.k8s.io/v1"

chartValues :: Settings -> String -> Scenario -> Value
chartValues settings namespace scenario =
  object
    [ "FIPS_202205_tls_profile" .= (scenario.profile == FIPS),
      "gateway"
        .= object
          ( [ "name" .= String "regression",
              "className" .= settings.gatewayClass,
              "create" .= (scenario.ownership == ChartGateway),
              "serviceType" .= String "ClusterIP",
              "envoyProxy" .= object ["replicas" .= (1 :: Int)],
              "listeners" .= object ["https" .= object ["hostname" .= String "*.ingress.example.test"]],
              "allowedListenerNamespaces" .= [namespace <> "-companion"],
              "extraHttpsListeners" .= [object ["name" .= String "extra", "hostname" .= (hostname Extra)]],
              "patchPolicies" .= object ["xdsNameSchemeV2" .= settings.xdsNameSchemeV2]
            ]
              <> ["tls" .= object ["minVersion" .= String "1.2", "maxVersion" .= String "1.2", "ciphers" .= [String "ECDHE-ECDSA-AES128-GCM-SHA256"], "ecdhCurves" .= [String "P-256"]] | scenario.profile == CustomTLS12]
          ),
      "config"
        .= ( case scenario.mode of
               SingleDomain -> object ["dns" .= object ["https" .= hostname (API Primary), "federator" .= (hostname Federation), "webapp" .= hostname (Webapp Primary)]]
               MultipleDomains ->
                 object
                   [ "domains"
                       .= [ object ["name" .= String "primary", "base" .= String "ingress.example.test", "dns" .= object ["https" .= hostname (API Primary), "webapp" .= hostname (Webapp Primary)]],
                            object ["name" .= String "secondary", "base" .= String "other.example.test", "sectionName" .= String "secondary-https", "dns" .= object ["https" .= hostname (API Secondary), "webapp" .= hostname (Webapp Secondary)], "tls" .= object ["secretName" .= String "secondary-tls"]]
                          ]
                   ]
           ),
      "tls" .= object ["secret" .= object ["create" .= False, "nameOverride" .= String "server-tls"]],
      "federator" .= object ["enabled" .= (scenario.mode == SingleDomain), "tls" .= object ["useCertManager" .= False, "secretName" .= String "server-tls"]],
      "webapp" .= object ["enabled" .= True],
      "websockets" .= object ["enabled" .= False],
      "fakeS3" .= object ["enabled" .= False]
    ]

externalGateway :: Settings -> String -> [Value]
externalGateway settings namespace =
  [ resource "gateway.envoyproxy.io/v1alpha1" "EnvoyProxy" "regression" $
      object
        ["provider" .= object ["type" .= String "Kubernetes", "kubernetes" .= object ["envoyService" .= object ["type" .= String "ClusterIP"], "envoyDeployment" .= object ["replicas" .= (1 :: Int)]]]],
    gatewayResource "Gateway" "regression" $
      object
        [ "gatewayClassName" .= settings.gatewayClass,
          "infrastructure" .= object ["parametersRef" .= object ["group" .= String "gateway.envoyproxy.io", "kind" .= String "EnvoyProxy", "name" .= String "regression"]],
          "allowedListeners" .= object ["namespaces" .= object ["from" .= String "Selector", "selector" .= object ["matchExpressions" .= [object ["key" .= String "kubernetes.io/metadata.name", "operator" .= String "In", "values" .= [namespace, namespace <> "-companion"]]]]]],
          "listeners" .= [object ["name" .= String "placeholder", "port" .= (65535 :: Int), "protocol" .= String "HTTP", "allowedRoutes" .= object ["namespaces" .= object ["from" .= String "Selector", "selector" .= object ["matchExpressions" .= [object ["key" .= String "kubernetes.io/metadata.name", "operator" .= String "DoesNotExist"]]]]]]]
        ]
  ]

extraRoute, companionRoute :: Value
extraRoute = route "extra" "regression-wire-ingress" "extra" (hostname Extra)
companionRoute = route "companion" "companion" "companion" (hostname Companion)

route :: String -> String -> String -> String -> Value
route name listener section host =
  gatewayResource "HTTPRoute" name $
    object
      [ "parentRefs" .= [object ["name" .= listener, "kind" .= String "ListenerSet", "sectionName" .= section]],
        "hostnames" .= [host],
        "rules" .= [object ["backendRefs" .= [object ["name" .= String "nginz", "port" .= (8080 :: Int)]]]]
      ]

companionListener :: String -> Value
companionListener namespace =
  gatewayResource "ListenerSet" "companion" $
    object
      [ "parentRef" .= object ["name" .= String "regression", "namespace" .= namespace, "kind" .= String "Gateway", "group" .= String "gateway.networking.k8s.io"],
        "listeners"
          .= [ object
                 [ "name" .= String "companion",
                   "hostname" .= hostname Companion,
                   "port" .= (443 :: Int),
                   "protocol" .= String "HTTPS",
                   "tls" .= object ["mode" .= String "Terminate", "certificateRefs" .= [object ["name" .= String "server-tls"]]]
                 ]
             ]
      ]

tlsSecret :: String -> TLS.CertificateMaterial -> Value
tlsSecret name identity =
  object
    [ "apiVersion" .= String "v1",
      "kind" .= String "Secret",
      "metadata" .= object ["name" .= name],
      "type" .= String "kubernetes.io/tls",
      "stringData"
        .= object
          [ "tls.crt" .= Text.decodeUtf8 (TLS.certificatePEM identity),
            "tls.key" .= Text.decodeUtf8 (TLS.privateKeyPEM identity)
          ]
    ]

caBundle :: TLS.CertificateMaterial -> Value
caBundle identity =
  object
    [ "apiVersion" .= String "v1",
      "kind" .= String "ConfigMap",
      "metadata" .= object ["name" .= String "federator-ca"],
      "data" .= object ["ca.crt" .= Text.decodeUtf8 (TLS.certificatePEM identity)]
    ]
