{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-binds -Wno-orphans #-}

module Test.SAML2.WebSSO.ConfigSpec
  ( spec,
  )
where

import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.String.Conversions
import Data.Yaml qualified as Yaml
import Hedgehog
import SAML2.WebSSO
import SAML2.WebSSO.Test.Arbitrary
import SAML2.WebSSO.Test.Util
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
    it "multi-ingress" $ do
      let simple =
            "version: SAML2.0\n"
              ++ "logLevel: Warn\n"
              ++ "spHost: 0.0.0.0\n"
              ++ "spPort: 8088\n"
              ++ "spAppUri: http://localhost:8088\n/"
              ++ "spSsoUri: http://localhost:8088/sso\n"
              ++ "contacts\n:"
              ++ "  - email: email:president@evil.corp\n"

          multi =
            "version: SAML2.0\n"
              ++ "logLevel: Debug\n"
              ++ "spHost: 0.0.0.1\n"
              ++ "spPort: 1\n"
              ++ "domainConfigs:\n"
              ++ "  domainone.io\n:"
              ++ "    spAppUri: http://arg\n/"
              ++ "    spSsoUri: http://arg/sso\n"
              ++ "    contacts\n:"
              ++ "      - email: email:yes@no.io\n"
              ++ "  domaintwo.io\n:"
              ++ "    spAppUri: http://localhost:8088\n/"
              ++ "    spSsoUri: http://localhost:8088/sso\n"
              ++ "    contacts\n:"
              ++ "      - email: email:vice-president@evil.corp\n"
              ++ "      - type: ContactSupport\n"
              ++ "        company: evil corp.\n"
              ++ "        givenName: Dr.\n"
              ++ "        surname: Girlfriend\n"
              ++ "        email: email:president@evil.corp\n"
              ++ "        phone: '+314159265'\n"
      -- TODO: Implement this test
      pending
