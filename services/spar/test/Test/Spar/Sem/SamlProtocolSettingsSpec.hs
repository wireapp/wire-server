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
    prop "should find the spIssuer" $
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
          iss <- runFinal . sparRouteToServant cfgWithDomainEntry $ do
            spIssuer (Just tid) (Just domain)
          iss ^? _Just . fromIssuer . to renderURI
            `shouldBe` (Just . normalizeURIText) ((miCfg ^. cfgSPSsoURI . to renderURI) <> "/finalize-login/" <> idToText tid)

normalizeURIText :: Text -> Text
normalizeURIText =
  T.decodeUtf8
    . toByteString'
    . normalizeURIRef aggressiveNormalization
    . either error id
    . parseURI' @(Either String)
