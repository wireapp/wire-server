{-# OPTIONS_GHC -Wno-orphans #-}

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

module Test.Federator.Validation where

import qualified Data.ByteString as BS
import Data.Domain (Domain (..), domainText)
import Data.List.NonEmpty (NonEmpty (..))
import Data.String.Conversions
import qualified Data.Text.Encoding as Text
import Federator.Discovery (DiscoverFederator (..), LookupError (..))
import Federator.Options
import Federator.Validation
import Imports
import Polysemy (Sem, run)
import qualified Polysemy
import qualified Polysemy.Error as Polysemy
import qualified Polysemy.Reader as Polysemy
import Test.Federator.InternalServer ()
import Test.Federator.Options (noClientCertSettings)
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Federation.GRPC.Types
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
        else Left $ LookupErrorSrvNotAvailable "invalid origin domain"

mockDiscoveryFailure :: HasCallStack => Sem (DiscoverFederator ': r) x -> Sem r x
mockDiscoveryFailure = Polysemy.interpret $ \case
  DiscoverFederator _ -> error "Not mocked"
  DiscoverAllFederators _ -> pure . Left $ LookupErrorDNSError "mock DNS error"

tests :: TestTree
tests =
  testGroup "Validation" $
    [ testGroup "federateWith" $
        [ federateWithAllowListSuccess,
          federateWithAllowListFail
        ],
      testGroup "validateDomain" $
        [ validateDomainAllowListFailSemantic,
          validateDomainAllowListFail,
          validateDomainAllowListSuccess,
          validateDomainCertMissing,
          validateDomainCertInvalid,
          validateDomainCertWrongDomain,
          validateDomainCertCN,
          validateDomainMultipleFederators,
          validateDomainDiscoveryFailed,
          validateDomainNonIdentitySRV
        ],
      testGroup "validatePath - Success" validatePathSuccess,
      testGroup "validatePath - Normalize" validatePathNormalize,
      testGroup "validatePath - Forbid" validatePathForbidden
    ]

federateWithAllowListSuccess :: TestTree
federateWithAllowListSuccess =
  testCase "should give True when target domain is in the list" $ do
    let settings = settingsWithAllowList [Domain "hello.world"]
        res = run . Polysemy.runReader settings $ federateWith (Domain "hello.world")
    assertBool "federating should be allowed" res

federateWithAllowListFail :: TestTree
federateWithAllowListFail =
  testCase "should give False when target domain is not in the list" $ do
    let settings = settingsWithAllowList [Domain "only.other.domain"]
        res = run . Polysemy.runReader settings $ federateWith (Domain "hello.world")
    assertBool "federating should not be allowed" (not res)

validateDomainAllowListFailSemantic :: TestTree
validateDomainAllowListFailSemantic =
  testCase "semantic validation" $ do
    exampleCert <- BS.readFile "test/resources/unit/localhost.pem"
    let settings = settingsWithAllowList [Domain "only.other.domain"]
        res :: Either InwardError Domain =
          run
            . Polysemy.runError
            . mockDiscoveryTrivial
            . Polysemy.runReader settings
            $ validateDomain (Just exampleCert) ("invalid//.><-semantic-&@-domain" :: Text)
    res @?= Left (InwardError IAuthenticationFailed "Domain parse failure for [invalid//.><-semantic-&@-domain]: Failed reading: Invalid domain name: cannot be dotless domain")

validateDomainAllowListFail :: TestTree
validateDomainAllowListFail =
  testCase "allow list validation" $ do
    exampleCert <- BS.readFile "test/resources/unit/localhost.example.com.pem"
    let settings = settingsWithAllowList [Domain "only.other.domain"]
        res :: Either InwardError Domain =
          run
            . Polysemy.runError
            . mockDiscoveryTrivial
            . Polysemy.runReader settings
            $ validateDomain (Just exampleCert) ("localhost.example.com" :: Text)
    res @?= Left (InwardError IFederationDeniedByRemote "Origin domain [localhost.example.com] not in the federation allow list")

validateDomainAllowListSuccess :: TestTree
validateDomainAllowListSuccess =
  testCase "should give parsed domain if in the allow list" $ do
    exampleCert <- BS.readFile "test/resources/unit/localhost.example.com.pem"
    let domain = Domain "localhost.example.com"
        settings = settingsWithAllowList [domain]
        res :: Either InwardError Domain =
          run
            . Polysemy.runError
            . mockDiscoveryTrivial
            . Polysemy.runReader settings
            $ validateDomain (Just exampleCert) (domainText domain)
    assertEqual "validateDomain should give 'localhost.example.com' as domain" (Right domain) res

validateDomainCertMissing :: TestTree
validateDomainCertMissing =
  testCase "should fail if no client certificate is provided" $ do
    let res :: Either InwardError Domain =
          run . Polysemy.runError
            . mockDiscoveryTrivial
            . Polysemy.runReader noClientCertSettings
            $ validateDomain Nothing "foo.example.com"
    res @?= Left (InwardError IAuthenticationFailed "no client certificate provided")

validateDomainCertInvalid :: TestTree
validateDomainCertInvalid =
  testCase "should fail if the client certificate is invalid" $ do
    let res :: Either InwardError Domain =
          run . Polysemy.runError
            . mockDiscoveryTrivial
            . Polysemy.runReader noClientCertSettings
            $ validateDomain (Just "not a certificate") "foo.example.com"
    res @?= Left (InwardError IAuthenticationFailed "no certificate found")

validateDomainCertWrongDomain :: TestTree
validateDomainCertWrongDomain =
  testCase "should fail if the client certificate has a wrong domain" $ do
    exampleCert <- BS.readFile "test/resources/unit/localhost.example.com.pem"
    let res :: Either InwardError Domain =
          run . Polysemy.runError
            . mockDiscoveryTrivial
            . Polysemy.runReader noClientCertSettings
            $ validateDomain (Just exampleCert) "foo.example.com"
    res @?= Left (InwardError IAuthenticationFailed "none of the domain names match the certificate, errrors: [[NameMismatch \"foo.example.com\"]]")

validateDomainCertCN :: TestTree
validateDomainCertCN =
  testCase "should succeed if the certificate has subject CN but no SAN" $ do
    exampleCert <- BS.readFile "test/resources/unit/example.com.pem"
    let domain = Domain "foo.example.com"
        res :: Either InwardError Domain =
          run . Polysemy.runError
            . mockDiscoveryTrivial
            . Polysemy.runReader noClientCertSettings
            $ validateDomain (Just exampleCert) (domainText domain)
    res @?= Right domain

validateDomainMultipleFederators :: TestTree
validateDomainMultipleFederators =
  testCase "should succedd if certificate matches any of the given federators" $ do
    localhostExampleCert <- BS.readFile "test/resources/unit/localhost.example.com.pem"
    secondExampleCert <- BS.readFile "test/resources/unit/second-federator.example.com.pem"
    let runValidation =
          run
            . Polysemy.runError
            . mockDiscoveryMapping domain ("localhost.example.com" :| ["second-federator.example.com"])
            . Polysemy.runReader noClientCertSettings
        domain = Domain "foo.example.com"
        resFirst :: Either InwardError Domain =
          runValidation $ validateDomain (Just localhostExampleCert) (domainText domain)
    resFirst @?= Right domain
    let resSecond :: Either InwardError Domain =
          runValidation $ validateDomain (Just secondExampleCert) (domainText domain)
    resSecond @?= Right domain

validateDomainDiscoveryFailed :: TestTree
validateDomainDiscoveryFailed =
  testCase "should fail if discovery fails" $ do
    exampleCert <- BS.readFile "test/resources/unit/example.com.pem"
    let res :: Either InwardError Domain =
          run . Polysemy.runError
            . mockDiscoveryFailure
            . Polysemy.runReader noClientCertSettings
            $ validateDomain (Just exampleCert) "example.com"
    res @?= Left (InwardError IDiscoveryFailed "DNS error: mock DNS error")

validateDomainNonIdentitySRV :: TestTree
validateDomainNonIdentitySRV =
  testCase "should run discovery to look up the federator domain" $ do
    exampleCert <- BS.readFile "test/resources/unit/localhost.example.com.pem"
    let domain = Domain "foo.example.com"
        res :: Either InwardError Domain =
          run . Polysemy.runError
            . mockDiscoveryMapping domain ("localhost.example.com" :| [])
            . Polysemy.runReader noClientCertSettings
            $ validateDomain (Just exampleCert) (domainText domain)
    res @?= Right domain

validatePathSuccess :: [TestTree]
validatePathSuccess = do
  let paths =
        [ "federation/get-user-by-handle",
          "federation/get-conversations"
        ]
  expectOk <$> paths
  where
    expectOk :: ByteString -> TestTree
    expectOk path = testCase ("should allow " <> cs path) $ do
      runSanitize path @?= Right path

validatePathNormalize :: [TestTree]
validatePathNormalize = do
  let paths =
        [ ("federation//stuff", "federation/stuff"),
          ("/federation/get-user-by-handle", "federation/get-user-by-handle"),
          ("federation/../federation/stuff", "federation/stuff")
        ]
  expectNormalized <$> paths
  where
    expectNormalized :: (ByteString, ByteString) -> TestTree
    expectNormalized (input, output) = do
      testCase ("Should allow " <> cs input <> " and normalize to " <> cs output) $ do
        runSanitize input @?= Right output

validatePathForbidden :: [TestTree]
validatePathForbidden = do
  let paths =
        [ "",
          "/",
          "///",
          -- disallowed paths
          "federation",
          "/federation",
          "/federation/",
          "i/users",
          "/i/users",
          -- path traversals to avoid
          "../i/users",
          "federation/../i/users",
          "federation/%2e%2e/i/users", -- percent-encoded '../'
          "federation/%2E%2E/i/users",
          "federation/%252e%252e/i/users", -- double percent-encoded '../'
          "federation/%c0%ae%c0%ae/i/users", -- weird-encoded '../'
          -- syntax we don't wish to support
          "federation/é", -- not ASCII
          "federation/stuff?bar[]=baz", -- not parseable as URI
          "http://federation.wire.link/federation/stuff", -- contains scheme and domain
          "http://federation/stuff", -- contains scheme
          "federation.wire.link/federation/stuff", -- contains domain
          "federation/stuff?key=value", -- contains query parameter
          "federation/stuff%3fkey%3dvalue", -- contains query parameter
          "federation/stuff#fragment", -- contains fragment
          "federation/stuff%23fragment", -- contains fragment
          "federation/this-url-is-waaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaay-too-long"
        ]
  expectForbidden <$> paths
  where
    expectForbidden :: ByteString -> TestTree
    expectForbidden input = do
      testCase ("Should forbid '" <> cs (BS.take 40 input) <> "'") $ do
        let res = runSanitize input
        expectErr IForbiddenEndpoint res

runSanitize :: ByteString -> Either InwardError ByteString
runSanitize = run . Polysemy.runError @InwardError . sanitizePath

expectErr :: InwardErrorType -> Either InwardError ByteString -> IO ()
expectErr expectedType (Right bdy) = do
  assertFailure $ "expected error '" <> show expectedType <> "' but got a valid body: " <> show bdy
expectErr expectedType (Left err) =
  unless (inwardErrorType err == expectedType) $ do
    assertFailure $ "expected type '" <> show expectedType <> "' but got " <> show err

settingsWithAllowList :: [Domain] -> RunSettings
settingsWithAllowList domains =
  noClientCertSettings {federationStrategy = AllowList (AllowedDomains domains)}
