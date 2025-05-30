{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-binds -Wno-orphans -Wno-incomplete-uni-patterns #-}

module Test.SAML2.WebSSO.ConfigSpec
  ( spec,
  )
where

import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Char8 qualified as BS
import Data.Domain
import Data.Map qualified as M
import Data.String.Conversions
import Data.Yaml qualified as Yaml
import Hedgehog
import SAML2.WebSSO
import SAML2.WebSSO.Test.Arbitrary
import SAML2.WebSSO.Test.Util
import System.Logger (Level (..))
import Test.Hspec
import URI.ByteString.QQ

spec :: Spec
spec = describe "Config" $ do
  hedgehog . checkParallel . Group "roundtrip" $
    [("...", property $ forAll genConfig >>= \v -> tripping v toJSON (parseEither parseJSON))]
  describe "sample config" $ do
    let have :: Config
        have =
          Config
            { _cfgLogLevel = Debug,
              _cfgSPHost = "me.wire.com",
              _cfgSPPort = 443,
              _cfgDomainConfigs =
                Left
                  MultiIngressDomainConfig
                    { _cfgSPAppURI = [uri|https://me.wire.com/sp|],
                      _cfgSPSsoURI = [uri|https://me.wire.com/sso|],
                      _cfgContacts = [fallbackContact]
                    }
            }
    it "standard" $ do
      want <- readSampleIO "server-config.yaml"
      over _Left show (Yaml.decodeEither' (cs want))
        `shouldBe` Right have
    it "minimal contacts" $ do
      want <- readSampleIO "server-config-minimal-contact-details.yaml"
      let pers = ContactPerson ContactAdministrative Nothing Nothing Nothing (Just [uri|email:president@evil.corp|]) Nothing
          have' = have & cfgDomainConfigs . _Left . cfgContacts .~ [pers]
      over _Left show (Yaml.decodeEither' (cs want))
        `shouldBe` Right have'
    it "multi-ingress (single)" $ do
      let single =
            BS.unlines
              [ "version: SAML2.0",
                "logLevel: Warn",
                "spHost: 0.0.0.0",
                "spPort: 8088",
                "spAppUri: http://localhost:8088/",
                "spSsoUri: http://localhost:8088/sso",
                "contacts:",
                "  - type: ContactSupport",
                "    email: email:president@evil.corp"
              ]

          Right singleHave = Yaml.decodeEither' @Config single
          Right singleRoundTrip = Yaml.decodeEither' @Config (Yaml.encode singleHave)
          singleWant =
            Config
              { _cfgLogLevel = Warn,
                _cfgSPHost = "0.0.0.0",
                _cfgSPPort = 8088,
                _cfgDomainConfigs = Left (MultiIngressDomainConfig {_cfgSPAppURI = [uri|http://localhost:8088/|], _cfgSPSsoURI = [uri|http://localhost:8088/sso|], _cfgContacts = [ContactPerson {_cntType = ContactSupport, _cntCompany = Nothing, _cntGivenName = Nothing, _cntSurname = Nothing, _cntEmail = Just [uri|email:president@evil.corp|], _cntPhone = Nothing}]})
              }

      singleHave `shouldBe` singleWant
      singleRoundTrip `shouldBe` singleWant

    it "multi-ingress (multi)" $ do
      let multi =
            BS.unlines
              [ "version: SAML2.0",
                "logLevel: Debug",
                "spHost: 0.0.0.1",
                "spPort: 1",
                "spDomainConfigs:",
                "  domainone.io:",
                "    spAppUri: http://arg/",
                "    spSsoUri: http://arg/sso",
                "    contacts:",
                "      - type: ContactSupport",
                "        email: email:yes@no.io",
                "  domaintwo.io:",
                "    spAppUri: http://localhost:8088/",
                "    spSsoUri: http://localhost:8088/sso",
                "    contacts:",
                "      - type: ContactBilling",
                "        email: email:vice-president@evil.corp",
                "      - type: ContactBilling",
                "        company: evil corp.",
                "        givenName: Dr.",
                "        surname: Girlfriend",
                "        email: email:president@evil.corp",
                "        phone: '+314159265'"
              ]

          Right multiHave = Yaml.decodeEither' @Config multi
          Right multiRoundTrip = Yaml.decodeEither' @Config (Yaml.encode multiHave)
          multiWant =
            Config
              { _cfgLogLevel = Debug,
                _cfgSPHost = "0.0.0.1",
                _cfgSPPort = 1,
                _cfgDomainConfigs = Right (M.fromList [(Domain {_domainText = "domainone.io"}, MultiIngressDomainConfig {_cfgSPAppURI = [uri|http://arg/|], _cfgSPSsoURI = [uri|http://arg/sso|], _cfgContacts = [ContactPerson {_cntType = ContactSupport, _cntCompany = Nothing, _cntGivenName = Nothing, _cntSurname = Nothing, _cntEmail = Just [uri|email:yes@no.io|], _cntPhone = Nothing}]}), (Domain {_domainText = "domaintwo.io"}, MultiIngressDomainConfig {_cfgSPAppURI = [uri|http://localhost:8088/|], _cfgSPSsoURI = [uri|http://localhost:8088/sso|], _cfgContacts = [ContactPerson {_cntType = ContactBilling, _cntCompany = Nothing, _cntGivenName = Nothing, _cntSurname = Nothing, _cntEmail = Just [uri|email:vice-president@evil.corp|], _cntPhone = Nothing}, ContactPerson {_cntType = ContactBilling, _cntCompany = Just "evil corp.", _cntGivenName = Just "Dr.", _cntSurname = Just "Girlfriend", _cntEmail = Just [uri|email:president@evil.corp|], _cntPhone = Just "+314159265"}]})])
              }

      multiHave `shouldBe` multiWant
      multiRoundTrip `shouldBe` multiWant
