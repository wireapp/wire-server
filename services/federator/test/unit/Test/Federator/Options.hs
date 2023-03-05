{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Lazy (toStrict)
import Data.Domain (Domain (..), mkDomain)
import Data.String.Interpolate as QQ
import qualified Data.Yaml as Yaml
import Federator.Options
import Federator.Run
import Imports
import Test.Tasty
import Test.Tasty.HUnit

defRunSettings :: FilePath -> FilePath -> RunSettings
defRunSettings client key =
  RunSettings
    { federationStrategy = AllowAll,
      useSystemCAStore = True,
      remoteCAStore = Nothing,
      clientCertificate = client,
      clientPrivateKey = key,
      dnsHost = Nothing,
      dnsPort = Nothing
    }

noClientCertSettings :: RunSettings
noClientCertSettings = defRunSettings "invalid-cert" "invalid-private-key"

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
    assertParsesAs (withAllowList ["test.org"]) . B8.pack $
      [QQ.i|
      allowedDomains:
        - test.org|]
    assertParsesAs (withAllowList ["example.com", "wire.com"]) . B8.pack $
      [QQ.i|
      allowedDomains:
        - example.com
        - wire.com|]
    -- manual roundtrip example AllowAll
    let allowA = toStrict $ Aeson.encode AllowAll
    assertParsesAs AllowAll $ allowA
    -- manual roundtrip example AllowList
    let allowWire = withAllowList ["wire.com"]
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
          (defRunSettings "client.pem" "client-key.pem")
          ( B8.pack
              [QQ.i|
                federationStrategy:
                  allowAll:
                clientCertificate: client.pem
                clientPrivateKey: client-key.pem
                useSystemCAStore: true|]
          ),
      testCase "parse configuration example (closed federation)" $ do
        let settings =
              (defRunSettings "client.pem" "client-key.pem")
                { federationStrategy =
                    AllowList
                      ( AllowedDomains [Domain "server2.example.com"]
                      ),
                  useSystemCAStore = False
                }
        assertParsesAs settings . B8.pack $
          [QQ.i|
          federationStrategy:
            allowedDomains:
              - server2.example.com
          useSystemCAStore: false
          clientCertificate: client.pem
          clientPrivateKey: client-key.pem|],
      testCase "succefully read client credentials" $ do
        let settings =
              defRunSettings
                "test/resources/unit/localhost.pem"
                "test/resources/unit/localhost-key.pem"
        assertParsesAs settings . B8.pack $
          [QQ.i|
          useSystemCAStore: true
          federationStrategy:
            allowAll: null
          clientCertificate: test/resources/unit/localhost.pem
          clientPrivateKey: test/resources/unit/localhost-key.pem|]
        void (mkTLSSettingsOrThrow settings),
      testCase "fail on missing client credentials" $
        assertParseFailure @RunSettings . B8.pack $
          [QQ.i|
          useSystemCAStore: true
          federationStrategy:
            allowAll: null|],
      testCase "fail on missing client private key" $ do
        assertParseFailure @RunSettings . B8.pack $
          [QQ.i|
          useSystemCAStore: true
          federationStrategy:
            allowAll: null
          clientCertificate: test/resources/unit/localhost.pem|],
      testCase "fail on missing certificate" $ do
        assertParseFailure @RunSettings . B8.pack $
          [QQ.i|
          useSystemCAStore: true
          federationStrategy:
            allowAll: null
          clientPrivateKey: test/resources/unit/localhost-key.pem|],
      testCase "fail on non-existent certificate" $ do
        let settings = defRunSettings "non-existent" "non-existent"
        assertParsesAs settings . B8.pack $
          [QQ.i|
          useSystemCAStore: true
          federationStrategy:
            allowAll: null
          clientCertificate: non-existent
          clientPrivateKey: non-existent|]
        try @FederationSetupError (mkTLSSettingsOrThrow settings) >>= \case
          Left (InvalidClientCertificate _) -> pure ()
          Left e ->
            assertFailure $
              "expected invalid client certificate exception, got: "
                <> show e
          Right _ ->
            assertFailure "expected failure for non-existing client certificate, got success",
      -- @SF.Federation @TSFI.Federate @S3 @S7
      testCase "failToStartWithInvalidServerCredentials" $ do
        let settings =
              defRunSettings
                "test/resources/unit/invalid.pem"
                "test/resources/unit/localhost-key.pem"
        assertParsesAs settings . B8.pack $
          [QQ.i|
          useSystemCAStore: true
          federationStrategy:
            allowAll: null
          clientCertificate: test/resources/unit/invalid.pem
          clientPrivateKey: test/resources/unit/localhost-key.pem|]
        try @FederationSetupError (mkTLSSettingsOrThrow settings) >>= \case
          Left (InvalidClientCertificate _) -> pure ()
          Left e ->
            assertFailure $
              "expected invalid client certificate exception, got: "
                <> show e
          Right _ ->
            assertFailure "expected failure for invalid client certificate, got success",
      -- @END
      testCase "fail on invalid private key" $ do
        let settings =
              defRunSettings
                "test/resources/unit/localhost.pem"
                "test/resources/unit/invalid.pem"
        assertParsesAs settings . B8.pack $
          [QQ.i|
          useSystemCAStore: true
          federationStrategy:
            allowAll: null
          clientCertificate: test/resources/unit/localhost.pem
          clientPrivateKey: test/resources/unit/invalid.pem|]
        try @FederationSetupError (mkTLSSettingsOrThrow settings) >>= \case
          Left (InvalidClientPrivateKey _) -> pure ()
          Left e ->
            assertFailure $
              "expected invalid client certificate exception, got: "
                <> show e
          Right _ ->
            assertFailure "expected failure for invalid private key, got success"
    ]

assertParsesAs :: (HasCallStack, Eq a, FromJSON a, Show a) => a -> ByteString -> Assertion
assertParsesAs v bs =
  assertEqual "YAML parsing" (Right v) $
    either (Left . show) Right (Yaml.decodeEither' bs)

assertParseFailure :: forall a. (FromJSON a, Show a) => ByteString -> Assertion
assertParseFailure bs = case Yaml.decodeEither' bs of
  Left _ -> pure ()
  Right (x :: a) -> assertFailure $ "expected YAML parsing failure, got: " <> show x
