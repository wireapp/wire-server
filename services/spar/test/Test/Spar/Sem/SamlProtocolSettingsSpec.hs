module Test.Spar.Sem.SamlProtocolSettingsSpec where

import Arbitrary ()
import Control.Lens
import Data.ByteString.Conversion
import Data.Domain
import Data.Id
import qualified Data.Map as Map
import qualified Data.Text.Encoding as T
import Imports
import Polysemy
import SAML2.WebSSO
import Spar.Sem.SamlProtocolSettings
import Spar.Sem.SamlProtocolSettings.Servant (sparRouteToServant)
import Test.Hspec
import Test.Hspec.QuickCheck
import URI.ByteString (aggressiveNormalization, normalizeURIRef)

spec :: Spec
spec = do
  describe "SamlProtocolSettings actions" $ do
    prop "should find multi-ingress config values" $
      \(tid :: TeamId)
       (domain :: Domain)
       (cfg :: Config)
       (miCfg :: MultiIngressDomainConfig) -> do
          let cfgWithDomainEntry =
                cfg
                  & cfgDomainConfigs
                    %~ either
                      (\_ -> Right (Map.singleton domain miCfg))
                      (Right . Map.insert domain miCfg)
              ssoUri = normalizeURIText ((miCfg ^. cfgSPSsoURI . to renderURI) <> "/finalize-login/" <> idToText tid)

          (iss, respUri, contacts) <- runFinal . sparRouteToServant cfgWithDomainEntry $ do
            iss <- spIssuer (Just tid) (Just domain)
            respUri <- responseURI (Just tid) (Just domain)
            contacts <- contactPersons (Just domain)
            pure (iss, respUri, contacts)

          iss ^? _Just . fromIssuer . to renderURI `shouldBe` (Just ssoUri)
          respUri ^? _Just . to renderURI `shouldBe` (Just ssoUri)
          contacts `shouldBe` (miCfg ^. cfgContacts)

    prop "should find single-ingress config values" $
      \(tid :: TeamId)
       (mbDomain :: Maybe Domain)
       (cfg :: Config)
       (siCfg :: MultiIngressDomainConfig) -> do
          let cfgWithDomainEntry =
                cfg
                  & cfgDomainConfigs
                    %~ either
                      ((const . Left) siCfg)
                      ((const . Left) siCfg)
              ssoUri = normalizeURIText ((siCfg ^. cfgSPSsoURI . to renderURI) <> "/finalize-login/" <> idToText tid)

          (iss, respUri, contacts) <- runFinal . sparRouteToServant cfgWithDomainEntry $ do
            iss <- spIssuer (Just tid) mbDomain
            respUri <- responseURI (Just tid) mbDomain
            contacts <- contactPersons mbDomain
            pure (iss, respUri, contacts)

          iss ^? _Just . fromIssuer . to renderURI `shouldBe` (Just ssoUri)
          respUri ^? _Just . to renderURI `shouldBe` (Just ssoUri)
          contacts `shouldBe` (siCfg ^. cfgContacts)

normalizeURIText :: Text -> Text
normalizeURIText =
  T.decodeUtf8
    . toByteString'
    . normalizeURIRef aggressiveNormalization
    . either error id
    . parseURI' @(Either String)
