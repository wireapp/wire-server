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

import Data.ByteString qualified as BS
import Data.Domain
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text.Encoding qualified as Text
import Data.X509.Validation qualified as X509
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

mockDiscoveryMapping :: (HasCallStack) => Domain -> NonEmpty ByteString -> Sem (DiscoverFederator ': r) x -> Sem r x
mockDiscoveryMapping origin targets = Polysemy.interpret $ \case
  DiscoverFederator _ -> error "Not mocked"
  DiscoverAllFederators dom ->
    pure $
      if dom == origin
        then Right $ fmap (`SrvTarget` 443) targets
        else Left $ DiscoveryFailureSrvNotAvailable "invalid origin domain"

mockDiscoveryFailure :: (HasCallStack) => Sem (DiscoverFederator ': r) x -> Sem r x
mockDiscoveryFailure = Polysemy.interpret $ \case
  DiscoverFederator _ -> error "Not mocked"
  DiscoverAllFederators _ -> pure . Left $ DiscoveryFailureDNSError "mock DNS error"

scaffoldingFederationDomainConfigs :: FederationDomainConfigs
scaffoldingFederationDomainConfigs =
  FederationDomainConfigs
    AllowDynamic
    [ FederationDomainConfig (Domain "foo.example.com") FullSearch FederationRestrictionAllowAll,
      FederationDomainConfig (Domain "example.com") FullSearch FederationRestrictionAllowAll,
      FederationDomainConfig (Domain "federator.example.com") FullSearch FederationRestrictionAllowAll
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
        [ validateDomainAllowListFail,
          validateDomainAllowListSuccess,
          validateDomainCertWrongDomain,
          validateDomainCertCN,
          validateDomainCertSAN,
          validateDomainMultipleFederators,
          validateDomainDiscoveryFailed,
          validateDomainNonIdentitySRV
        ],
      testGroup
        "decodeCertificate"
        [validateDomainCertInvalid]
    ]

federateWithAllowListSuccess :: TestTree
federateWithAllowListSuccess =
  testCase "should give True when target domain is in the list" $ do
    let settings = noClientCertSettings
    runM
      . assertNoError @ValidationError
      . runInputConst settings
      . runInputConst (FederationDomainConfigs AllowDynamic [FederationDomainConfig (Domain "hello.world") FullSearch FederationRestrictionAllowAll] 0)
      $ ensureCanFederateWith (Domain "hello.world")

federateWithAllowListFail :: TestTree
federateWithAllowListFail =
  testCase "should give False when target domain is not in the list" $ do
    let settings = noClientCertSettings
    eith :: Either ValidationError () <-
      runM
        . runError @ValidationError
        . runInputConst settings
        . runInputConst (FederationDomainConfigs AllowDynamic [FederationDomainConfig (Domain "only.other.domain") FullSearch FederationRestrictionAllowAll] 0)
        $ ensureCanFederateWith (Domain "hello.world")
    assertBool "federating should not be allowed" (isLeft eith)

-- @SF.Federation @TSFI.Federate @TSFI.DNS @S2 @S3 @S7
--
-- Refuse to send outgoing request to non-included domain when AllowDynamic is configured.
validateDomainAllowListFail :: TestTree
validateDomainAllowListFail =
  testCase "validateDomainAllowListFail - allow list validation" $ do
    Right exampleCert <- decodeCertificate <$> BS.readFile "test/resources/unit/localhost.example.com.pem"
    let settings = noClientCertSettings
    res <-
      runM
        . runError
        . assertNoError @DiscoveryFailure
        . mockDiscoveryTrivial
        . runInputConst settings
        . runInputConst (FederationDomainConfigs AllowDynamic [FederationDomainConfig (Domain "only.other.domain") FullSearch FederationRestrictionAllowAll] 0)
        $ validateDomain exampleCert (Domain "localhost.example.com")
    res @?= Left (FederationDenied (Domain "localhost.example.com"))

-- @END

validateDomainAllowListSuccess :: TestTree
validateDomainAllowListSuccess =
  testCase "should give parsed domain if in the allow list" $ do
    Right exampleCert <- decodeCertificate <$> BS.readFile "test/resources/unit/localhost.example.com.pem"
    let domain = Domain "localhost.example.com"
        settings = noClientCertSettings
    res <-
      runM
        . assertNoError @ValidationError
        . assertNoError @DiscoveryFailure
        . mockDiscoveryTrivial
        . runInputConst settings
        . runInputConst (FederationDomainConfigs AllowDynamic [FederationDomainConfig domain FullSearch FederationRestrictionAllowAll] 0)
        $ validateDomain exampleCert domain
    assertEqual "validateDomain should give 'localhost.example.com' as domain" domain res

-- @SF.Federation @TSFI.Federate @TSFI.DNS @S3 @S7
--
-- Reject request if the infrastructure domain in the client cert does not match the backend
-- domain in the `Wire-origin-domain` header.
validateDomainCertWrongDomain :: TestTree
validateDomainCertWrongDomain =
  testCase "validateDomainCertWrongDomain - should fail if the client certificate has a wrong domain" $ do
    Right exampleCert <- decodeCertificate <$> BS.readFile "test/resources/unit/localhost.example.com.pem"
    res <-
      runM
        . runError
        . assertNoError @DiscoveryFailure
        . mockDiscoveryTrivial
        . runInputConst noClientCertSettings
        . runInputConst scaffoldingFederationDomainConfigs
        $ validateDomain exampleCert (Domain "foo.example.com")
    res @?= Left (AuthenticationFailure (pure [X509.NameMismatch "foo.example.com"]))

-- @END

validateDomainCertCN :: TestTree
validateDomainCertCN =
  testCase "should succeed if the certificate has subject CN but no SAN" $ do
    Right exampleCert <- decodeCertificate <$> BS.readFile "test/resources/unit/example.com.pem"
    let domain = Domain "foo.example.com"
    res <-
      runM
        . assertNoError @ValidationError
        . assertNoError @DiscoveryFailure
        . mockDiscoveryTrivial
        . runInputConst noClientCertSettings
        . runInputConst scaffoldingFederationDomainConfigs
        $ validateDomain exampleCert domain
    res @?= domain

validateDomainCertSAN :: TestTree
validateDomainCertSAN =
  testCase "should succeed if the certificate has a longer list of domains inside SAN, one of which is the expected one" $ do
    Right exampleCert <- decodeCertificate <$> BS.readFile "test/resources/unit/multidomain-federator.example.com.pem"
    let domain = Domain "federator.example.com"
    res <-
      runM
        . assertNoError @ValidationError
        . assertNoError @DiscoveryFailure
        . mockDiscoveryTrivial
        . runInputConst noClientCertSettings
        . runInputConst scaffoldingFederationDomainConfigs
        $ validateDomain exampleCert domain
    res @?= domain

validateDomainMultipleFederators :: TestTree
validateDomainMultipleFederators =
  testCase "should succedd if certificate matches any of the given federators" $ do
    Right localhostExampleCert <- decodeCertificate <$> BS.readFile "test/resources/unit/localhost.example.com.pem"
    Right secondExampleCert <- decodeCertificate <$> BS.readFile "test/resources/unit/second-federator.example.com.pem"
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
        validateDomain localhostExampleCert domain
    resFirst @?= domain
    resSecond <-
      runValidation $
        validateDomain secondExampleCert domain
    resSecond @?= domain

-- FUTUREWORK: is this test really necessary?
validateDomainDiscoveryFailed :: TestTree
validateDomainDiscoveryFailed =
  testCase "should fail if discovery fails" $ do
    Right exampleCert <- decodeCertificate <$> BS.readFile "test/resources/unit/example.com.pem"
    res <-
      runM
        . runError
        . assertNoError @ValidationError
        . mockDiscoveryFailure
        . runInputConst noClientCertSettings
        . runInputConst scaffoldingFederationDomainConfigs
        $ validateDomain exampleCert (Domain "example.com")
    res @?= Left (DiscoveryFailureDNSError "mock DNS error")

validateDomainNonIdentitySRV :: TestTree
validateDomainNonIdentitySRV =
  testCase "should run discovery to look up the federator domain" $ do
    Right exampleCert <- decodeCertificate <$> BS.readFile "test/resources/unit/localhost.example.com.pem"
    let domain = Domain "foo.example.com"
    res <-
      runM
        . assertNoError @ValidationError
        . assertNoError @DiscoveryFailure
        . mockDiscoveryMapping domain ("localhost.example.com" :| [])
        . runInputConst noClientCertSettings
        . runInputConst scaffoldingFederationDomainConfigs
        $ validateDomain exampleCert domain
    res @?= domain

-- @SF.Federation @TSFI.Federate @TSFI.DNS @S2 @S3 @S7
-- Reject request if the client certificate for federator is invalid
validateDomainCertInvalid :: TestTree
validateDomainCertInvalid =
  testCase "validateDomainCertInvalid - should fail if the client certificate is invalid" $ do
    let res = decodeCertificate "not a certificate"
    res @?= Left "no certificate found"

-- @END
