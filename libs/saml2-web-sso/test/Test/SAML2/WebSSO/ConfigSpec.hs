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
              _cfgSPAppURI = [uri|https://me.wire.com/sp|],
              _cfgSPSsoURI = [uri|https://me.wire.com/sso|],
              _cfgContacts = [fallbackContact]
            }
    it "standard" $ do
      want <- readSampleIO "server-config.yaml"
      over _Left show (Yaml.decodeEither' (cs want))
        `shouldBe` Right have
    it "minimal contacts" $ do
      want <- readSampleIO "server-config-minimal-contact-details.yaml"
      let pers = ContactPerson ContactAdministrative Nothing Nothing Nothing (Just [uri|email:president@evil.corp|]) Nothing
          have' = have & cfgContacts .~ [pers]
      over _Left show (Yaml.decodeEither' (cs want))
        `shouldBe` Right have'
