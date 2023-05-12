{-# OPTIONS_GHC -Wno-orphans #-}

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

module Test.Federator.Validation where

import qualified Data.ByteString as BS
import Data.ByteString.Conversion
import Data.Domain (Domain (..), domainText)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Encoding as Text
import qualified Data.X509.Validation as X509
import Federator.Discovery
import Federator.Validation
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Test.Federator.InternalServer ()
import Test.Federator.Options (noClientCertSettings)
import Test.Federator.Util
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Routes.FederationDomainConfig
import Wire.API.User.Search
import Wire.Network.DNS.SRV (SrvTarget (..))

mockDiscoveryTrivial :: Sem (DiscoverFederator ': r) x -> Sem r x
mockDiscoveryTrivial = Polysemy.interpret $ \case
  DiscoverFederator dom -> pure . Right $ SrvTarget (Text.encodeUtf8 (domainText dom)) 443
  DiscoverAllFederators dom -> pure . Right $ SrvTarget (Text.encodeUtf8 (domainText dom)) 443 :| []

mockDiscoveryMapping :: HasCallStack => Domain -> NonEmpty ByteString -> Sem (DiscoverFederator ': r) x -> Sem r x
mockDiscoveryMapping origin targets = Polysemy.interpret $ \case
  DiscoverFederator _ -> error "Not mocked"
  DiscoverAllFederators dom ->
    pure $
      if dom == origin
        then Right $ fmap (`SrvTarget` 443) targets
        else Left $ DiscoveryFailureSrvNotAvailable "invalid origin domain"

mockDiscoveryFailure :: HasCallStack => Sem (DiscoverFederator ': r) x -> Sem r x
mockDiscoveryFailure = Polysemy.interpret $ \case
  DiscoverFederator _ -> error "Not mocked"
  DiscoverAllFederators _ -> pure . Left $ DiscoveryFailureDNSError "mock DNS error"

scaffoldingFederationDomainConfigs :: FederationDomainConfigs
scaffoldingFederationDomainConfigs =
  FederationDomainConfigs
    AllowList
    [ FederationDomainConfig (Domain "foo.example.com") FullSearch,
      FederationDomainConfig (Domain "example.com") FullSearch,
      FederationDomainConfig (Domain "federator.example.com") FullSearch
    ]
    10

tests :: TestTree
tests =
  testGroup
    "Validation"
    [ testGroup
        "federateWith"
        [ federateWithAllowListSuccess,
          federateWithAllowListFail
        ],
      testGroup
        "validateDomain"
        [ validateDomainAllowListFailSemantic,
          validateDomainAllowListFail,
          validateDomainAllowListSuccess,
          validateDomainCertMissing,
          validateDomainCertInvalid,
          validateDomainCertWrongDomain,
          validateDomainCertCN,
          validateDomainCertSAN,
          validateDomainMultipleFederators,
          validateDomainDiscoveryFailed,
          validateDomainNonIdentitySRV
        ]
    ]

federateWithAllowListSuccess :: TestTree
federateWithAllowListSuccess =
  testCase "should give True when target domain is in the list" $ do
    let settings = noClientCertSettings
    runM
      . assertNoError @ValidationError
      . runInputConst settings
      . runInputConst (FederationDomainConfigs AllowList [FederationDomainConfig (Domain "hello.world") FullSearch] 0)
      $ ensureCanFederateWith (Domain "hello.world")

federateWithAllowListFail :: TestTree
federateWithAllowListFail =
  testCase "should give False when target domain is not in the list" $ do
    let settings = noClientCertSettings
    eith :: Either ValidationError () <-
      runM
        . runError @ValidationError
        . runInputConst settings
        . runInputConst (FederationDomainConfigs AllowList [FederationDomainConfig (Domain "only.other.domain") FullSearch] 0)
        $ ensureCanFederateWith (Domain "hello.world")
    assertBool "federating should not be allowed" (isLeft eith)

validateDomainAllowListFailSemantic :: TestTree
validateDomainAllowListFailSemantic =
  testCase "semantic validation" $ do
    exampleCert <- BS.readFile "test/resources/unit/localhost.pem"
    let settings = noClientCertSettings
    res <-
      runM
        . runError
        . assertNoError @DiscoveryFailure
        . mockDiscoveryTrivial
        . runInputConst settings
        . runInputConst (FederationDomainConfigs AllowList [FederationDomainConfig (Domain "only.other.domain") FullSearch] 0)
        $ validateDomain (Just exampleCert) "invalid//.><-semantic-&@-domain"
    res @?= Left (DomainParseError "invalid//.><-semantic-&@-domain")

-- @SF.Federation @TSFI.Federate @TSFI.DNS @S2 @S3 @S7
--
-- Refuse to send outgoing request to non-included domain when allowlist is configured.
validateDomainAllowListFail :: TestTree
validateDomainAllowListFail =
  testCase "allow list validation" $ do
    exampleCert <- BS.readFile "test/resources/unit/localhost.example.com.pem"
    let settings = noClientCertSettings
    res <-
      runM
        . runError
        . assertNoError @DiscoveryFailure
        . mockDiscoveryTrivial
        . runInputConst settings
        . runInputConst (FederationDomainConfigs AllowList [FederationDomainConfig (Domain "only.other.domain") FullSearch] 0)
        $ validateDomain (Just exampleCert) "localhost.example.com"
    res @?= Left (FederationDenied (Domain "localhost.example.com"))

-- @END

validateDomainAllowListSuccess :: TestTree
validateDomainAllowListSuccess =
  testCase "should give parsed domain if in the allow list" $ do
    exampleCert <- BS.readFile "test/resources/unit/localhost.example.com.pem"
    let domain = Domain "localhost.example.com"
        settings = noClientCertSettings
    res <-
      runM
        . assertNoError @ValidationError
        . assertNoError @DiscoveryFailure
        . mockDiscoveryTrivial
        . runInputConst settings
        . runInputConst (FederationDomainConfigs AllowList [FederationDomainConfig domain FullSearch] 0)
        $ validateDomain (Just exampleCert) (toByteString' domain)
    assertEqual "validateDomain should give 'localhost.example.com' as domain" domain res

validateDomainCertMissing :: TestTree
validateDomainCertMissing =
  testCase "should fail if no client certificate is provided" $ do
    res <-
      runM
        . runError
        . assertNoError @DiscoveryFailure
        . mockDiscoveryTrivial
        . runInputConst noClientCertSettings
        . runInputConst defFederationDomainConfigs
        $ validateDomain Nothing "foo.example.com"
    res @?= Left NoClientCertificate

-- @SF.Federation @TSFI.Federate @TSFI.DNS @S2 @S3 @S7
-- Reject request if the client certificate for federator is invalid
validateDomainCertInvalid :: TestTree
validateDomainCertInvalid =
  testCase "should fail if the client certificate is invalid" $ do
    res <-
      runM
        . runError
        . assertNoError @DiscoveryFailure
        . mockDiscoveryTrivial
        . runInputConst noClientCertSettings
        . runInputConst scaffoldingFederationDomainConfigs
        $ validateDomain (Just "not a certificate") "foo.example.com"
    res @?= Left (CertificateParseError "no certificate found")

-- @END

-- @SF.Federation @TSFI.Federate @TSFI.DNS @S3 @S7
--
-- Reject request if the infrastructure domain in the client cert does not match the backend
-- domain in the `Wire-origin-domain` header.
validateDomainCertWrongDomain :: TestTree
validateDomainCertWrongDomain =
  testCase "should fail if the client certificate has a wrong domain" $ do
    exampleCert <- BS.readFile "test/resources/unit/localhost.example.com.pem"
    res <-
      runM
        . runError
        . assertNoError @DiscoveryFailure
        . mockDiscoveryTrivial
        . runInputConst noClientCertSettings
        . runInputConst scaffoldingFederationDomainConfigs
        $ validateDomain (Just exampleCert) "foo.example.com"
    res @?= Left (AuthenticationFailure (pure [X509.NameMismatch "foo.example.com"]))

-- @END

validateDomainCertCN :: TestTree
validateDomainCertCN =
  testCase "should succeed if the certificate has subject CN but no SAN" $ do
    exampleCert <- BS.readFile "test/resources/unit/example.com.pem"
    let domain = Domain "foo.example.com"
    res <-
      runM
        . assertNoError @ValidationError
        . assertNoError @DiscoveryFailure
        . mockDiscoveryTrivial
        . runInputConst noClientCertSettings
        . runInputConst scaffoldingFederationDomainConfigs
        $ validateDomain (Just exampleCert) (toByteString' domain)
    res @?= domain

validateDomainCertSAN :: TestTree
validateDomainCertSAN =
  testCase "should succeed if the certificate has a longer list of domains inside SAN, one of which is the expected one" $ do
    exampleCert <- BS.readFile "test/resources/unit/multidomain-federator.example.com.pem"
    let domain = Domain "federator.example.com"
    res <-
      runM
        . assertNoError @ValidationError
        . assertNoError @DiscoveryFailure
        . mockDiscoveryTrivial
        . runInputConst noClientCertSettings
        . runInputConst scaffoldingFederationDomainConfigs
        $ validateDomain (Just exampleCert) (toByteString' domain)
    res @?= domain

validateDomainMultipleFederators :: TestTree
validateDomainMultipleFederators =
  testCase "should succedd if certificate matches any of the given federators" $ do
    localhostExampleCert <- BS.readFile "test/resources/unit/localhost.example.com.pem"
    secondExampleCert <- BS.readFile "test/resources/unit/second-federator.example.com.pem"
    let runValidation =
          runM
            . assertNoError @ValidationError
            . assertNoError @DiscoveryFailure
            . mockDiscoveryMapping domain ("localhost.example.com" :| ["second-federator.example.com"])
            . runInputConst noClientCertSettings
            . runInputConst scaffoldingFederationDomainConfigs
        domain = Domain "foo.example.com"
    resFirst <-
      runValidation $
        validateDomain (Just localhostExampleCert) (toByteString' domain)
    resFirst @?= domain
    resSecond <-
      runValidation $
        validateDomain (Just secondExampleCert) (toByteString' domain)
    resSecond @?= domain

-- FUTUREWORK: is this test really necessary?
validateDomainDiscoveryFailed :: TestTree
validateDomainDiscoveryFailed =
  testCase "should fail if discovery fails" $ do
    exampleCert <- BS.readFile "test/resources/unit/example.com.pem"
    res <-
      runM
        . runError
        . assertNoError @ValidationError
        . mockDiscoveryFailure
        . runInputConst noClientCertSettings
        . runInputConst scaffoldingFederationDomainConfigs
        $ validateDomain (Just exampleCert) "example.com"
    res @?= Left (DiscoveryFailureDNSError "mock DNS error")

validateDomainNonIdentitySRV :: TestTree
validateDomainNonIdentitySRV =
  testCase "should run discovery to look up the federator domain" $ do
    exampleCert <- BS.readFile "test/resources/unit/localhost.example.com.pem"
    let domain = Domain "foo.example.com"
    res <-
      runM
        . assertNoError @ValidationError
        . assertNoError @DiscoveryFailure
        . mockDiscoveryMapping domain ("localhost.example.com" :| [])
        . runInputConst noClientCertSettings
        . runInputConst scaffoldingFederationDomainConfigs
        $ validateDomain (Just exampleCert) (toByteString' domain)
    res @?= domain
