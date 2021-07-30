{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.Federator.Options where

import Control.Exception (try)
import Control.Lens
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Domain (Domain (..), mkDomain)
import qualified Data.Yaml as Yaml
import Federator.Env
import Federator.Options
import Federator.Run
import Imports
import Test.Tasty
import Test.Tasty.HUnit

defRunSettings :: RunSettings
defRunSettings =
  RunSettings
    { federationStrategy = AllowAll,
      useSystemCAStore = True,
      remoteCAStore = Nothing,
      clientCertificate = Nothing,
      clientPrivateKey = Nothing
    }

tests :: TestTree
tests =
  testGroup
    "Options"
    [ parseFederationStrategy,
      testSettings
    ]

parseFederationStrategy :: TestTree
parseFederationStrategy =
  testCase "parse FederationStrategy examples" $ do
    assertParsesAs AllowAll $
      "allowAll: null"
    assertParsesAs (withAllowList []) $
      "allowedDomains: []"
    assertParsesAs (withAllowList ["test.org"]) $
      "allowedDomains:\n\
      \  - test.org"
    assertParsesAs (withAllowList ["example.com", "wire.com"]) $
      "allowedDomains:\n\
      \  - example.com\n\
      \  - wire.com"
    -- manual roundtrip example AllowAll
    let allowA = toStrict $ Aeson.encode AllowAll
    assertParsesAs AllowAll $ allowA
    -- manual roundtrip example AllowList
    let allowWire = (withAllowList ["wire.com"])
    let allowedDom = toStrict $ Aeson.encode allowWire
    assertParsesAs allowWire $ allowedDom
  where
    withAllowList =
      AllowList . AllowedDomains . map (either error id . mkDomain)

testSettings :: TestTree
testSettings =
  testGroup
    "settings"
    [ testCase "parse configuration example (open federation)" $ do
        assertParsesAs
          defRunSettings
          "federationStrategy:\n\
          \  allowAll:\n\
          \useSystemCAStore: true",
      testCase "parse configuration example (closed federation)" $ do
        let settings =
              defRunSettings
                { federationStrategy =
                    AllowList
                      ( AllowedDomains [Domain "server2.example.com"]
                      ),
                  useSystemCAStore = False,
                  clientCertificate = Just "client.pem",
                  clientPrivateKey = Just "client-key.pem"
                }
        assertParsesAs settings $
          "federationStrategy:\n\
          \  allowedDomains:\n\
          \    - server2.example.com\n\
          \useSystemCAStore: false\n\
          \clientCertificate: client.pem\n\
          \clientPrivateKey: client-key.pem",
      testCase "succefully read client credentials" $ do
        let settings =
              defRunSettings
                { clientCertificate = Just "test/resources/unit/localhost.pem",
                  clientPrivateKey = Just "test/resources/unit/localhost-key.pem"
                }
        assertParsesAs settings $
          "useSystemCAStore: true\n\
          \federationStrategy:\n\
          \  allowAll: null\n\
          \clientCertificate: test/resources/unit/localhost.pem\n\
          \clientPrivateKey: test/resources/unit/localhost-key.pem\n"
        tlsSettings <- mkTLSSettings settings
        assertBool "expected TLS client credentials" $
          notNullOf (creds . _Just) tlsSettings,
      testCase "parse missing client credentials" $ do
        let settings = defRunSettings
        assertParsesAs settings $
          "useSystemCAStore: true\n\
          \federationStrategy:\n\
          \  allowAll: null\n"
        tlsSettings <- mkTLSSettings settings
        assertBool "unexpected TLS client credentials" $
          nullOf (creds . _Just) tlsSettings,
      testCase "fail on missing client private key" $ do
        let settings =
              defRunSettings
                { clientCertificate = Just "test/resources/unit/localhost.pem"
                }
        assertParsesAs settings $
          "useSystemCAStore: true\n\
          \federationStrategy:\n\
          \  allowAll: null\n\
          \clientCertificate: test/resources/unit/localhost.pem\n"
        try @FederationSetupError (mkTLSSettings settings) >>= \case
          Left (InvalidClientCertificate _) -> pure ()
          Left e ->
            assertFailure $
              "expected invalid client certificate exception, got: "
                <> show e
          Right tlsSettings ->
            assertFailure $
              "expected failure for partial client credentials, got: "
                <> show (tlsSettings ^. creds),
      testCase "fail on missing certificate" $ do
        let settings =
              defRunSettings
                { clientPrivateKey = Just "test/resources/unit/localhost-key.pem"
                }
        assertParsesAs settings $
          "useSystemCAStore: true\n\
          \federationStrategy:\n\
          \  allowAll: null\n\
          \clientPrivateKey: test/resources/unit/localhost-key.pem\n"
        try @FederationSetupError (mkTLSSettings settings) >>= \case
          Left (InvalidClientCertificate _) -> pure ()
          Left e ->
            assertFailure $
              "expected invalid client certificate exception, got: "
                <> show e
          Right tlsSettings ->
            assertFailure $
              "expected failure for partial client credentials, got: "
                <> show (tlsSettings ^. creds),
      testCase "fail on non-existent certificate" $ do
        let settings =
              defRunSettings
                { clientCertificate = Just "non-existent",
                  clientPrivateKey = Just "non-existent"
                }
        assertParsesAs settings $
          "useSystemCAStore: true\n\
          \federationStrategy:\n\
          \  allowAll: null\n\
          \clientCertificate: non-existent\n\
          \clientPrivateKey: non-existent"
        try @FederationSetupError (mkTLSSettings settings) >>= \case
          Left (InvalidClientCertificate _) -> pure ()
          Left e ->
            assertFailure $
              "expected invalid client certificate exception, got: "
                <> show e
          Right tlsSettings ->
            assertFailure $
              "expected failure for non-existing client certificate, got: "
                <> show (tlsSettings ^. creds),
      testCase "fail on invalid certificate" $ do
        let settings =
              defRunSettings
                { clientCertificate = Just "test/resources/unit/invalid.pem",
                  clientPrivateKey = Just "test/resources/unit/localhost-key.pem"
                }
        assertParsesAs settings $
          "useSystemCAStore: true\n\
          \federationStrategy:\n\
          \  allowAll: null\n\
          \clientCertificate: test/resources/unit/invalid.pem\n\
          \clientPrivateKey: test/resources/unit/localhost-key.pem"
        try @FederationSetupError (mkTLSSettings settings) >>= \case
          Left (InvalidClientCertificate _) -> pure ()
          Left e ->
            assertFailure $
              "expected invalid client certificate exception, got: "
                <> show e
          Right tlsSettings ->
            assertFailure $
              "expected failure for invalid client certificate, got: "
                <> show (tlsSettings ^. creds),
      testCase "fail on invalid private key" $ do
        let settings =
              defRunSettings
                { clientCertificate = Just "test/resources/unit/localhost.pem",
                  clientPrivateKey = Just "test/resources/unit/invalid.pem"
                }
        assertParsesAs settings $
          "useSystemCAStore: true\n\
          \federationStrategy:\n\
          \  allowAll: null\n\
          \clientCertificate: test/resources/unit/localhost.pem\n\
          \clientPrivateKey: test/resources/unit/invalid.pem"
        try @FederationSetupError (mkTLSSettings settings) >>= \case
          Left (InvalidClientCertificate _) -> pure ()
          Left e ->
            assertFailure $
              "expected invalid client certificate exception, got: "
                <> show e
          Right tlsSettings ->
            assertFailure $
              "expected failure for invalid private key, got: "
                <> show (tlsSettings ^. creds)
    ]

assertParsesAs :: (HasCallStack, Eq a, FromJSON a, Show a) => a -> ByteString -> Assertion
assertParsesAs v bs =
  assertEqual "YAML parsing" (Right v) $
    either (Left . show) Right (Yaml.decodeEither' bs)
