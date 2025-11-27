-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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
  describe "SamlProtocolSettings actions" . modifyMaxSuccess (const 10) $ do
    prop "should find multi-ingress config values" $
      \(mbTid :: Maybe TeamId)
       (domain :: Domain)
       (cfg :: Config)
       (miCfg :: MultiIngressDomainConfig) -> do
          let cfgWithDomainEntry =
                cfg
                  & cfgDomainConfigs
                    %~ either
                      (\_ -> Right (Map.singleton domain miCfg))
                      (Right . Map.insert domain miCfg)
              ssoUri = normalizeURIText ((miCfg ^. cfgSPSsoURI . to renderURI) <> "/finalize-login/" <> teamIdPath mbTid)

          (iss, respUri, contacts) <- runFinal . sparRouteToServant cfgWithDomainEntry $ do
            iss <- spIssuer mbTid (Just domain)
            respUri <- responseURI mbTid (Just domain)
            contacts <- contactPersons (Just domain)
            pure (iss, respUri, contacts)

          iss ^? _Just . fromIssuer . to renderURI `shouldBe` (Just ssoUri)
          respUri ^? _Just . to renderURI `shouldBe` (Just ssoUri)
          contacts `shouldBe` (miCfg ^. cfgContacts)

    prop "should find single-ingress config values" $
      \(mbTid :: Maybe TeamId)
       (mbDomain :: Maybe Domain)
       (cfg :: Config)
       (siCfg :: MultiIngressDomainConfig) -> do
          let cfgWithDomainEntry =
                cfg
                  & cfgDomainConfigs
                    %~ either
                      ((const . Left) siCfg)
                      ((const . Left) siCfg)
              ssoUri = normalizeURIText ((siCfg ^. cfgSPSsoURI . to renderURI) <> "/finalize-login/" <> teamIdPath mbTid)

          (iss, respUri, contacts) <- runFinal . sparRouteToServant cfgWithDomainEntry $ do
            iss <- spIssuer mbTid mbDomain
            respUri <- responseURI mbTid mbDomain
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

teamIdPath :: Maybe TeamId -> Text
teamIdPath Nothing = mempty
teamIdPath (Just tid) = idToText tid
