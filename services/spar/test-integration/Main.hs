{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

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

-- | It would be nice to use hspec-discover, which even has support for
-- <https://hspec.github.io/hspec-discover.html#using-a-custom-main-function custom main functions>.
--
-- This is trickier than expected, though: hspec-discover expects the modules to export @spec
-- :: Spec@, but we would need that to be @spec :: SpecWith TestEnv@. See
-- <https://github.com/hspec/hspec/issues/404>.
--
-- On the other hand, we cannot easily 'mkEnvFromOptions' inside each module, since it
-- requires @-s@, @-i@ command line modules, which will make 'hspec' choke. Related, but not
-- the solution: https://github.com/hspec/hspec/pull/397.
module Main where

import Control.Concurrent.Async (concurrently_)
import Control.Lens ((.~), (^.))
import Data.Text (pack)
import Imports
import Servant.API (toHeader)
import Spar.Run (mkApp)
import System.Environment (withArgs)
import System.Random (randomRIO)
import Test.Hspec
import Test.Hspec.JUnit
import Test.Hspec.JUnit.Config.Env
import Test.Hspec.Runner (Config (configAvailableFormatters), defaultConfig, hspecWith)
import qualified Test.LoggingSpec
import qualified Test.MetricsSpec
import qualified Test.Spar.APISpec
import qualified Test.Spar.AppSpec
import qualified Test.Spar.DataSpec
import qualified Test.Spar.Intra.BrigSpec
import qualified Test.Spar.Scim.AuthSpec
import qualified Test.Spar.Scim.UserSpec
import Util
import Web.Scim.Test.Acceptance (AcceptanceConfig (..), AcceptanceQueryConfig (..), microsoftAzure)
import Wire.API.User.Scim

main :: IO ()
main = do
  (wireArgs, hspecArgs) <- partitionArgs <$> getArgs
  let env = withArgs wireArgs mkEnvFromOptions
  -- FUTUREWORK(mangoiv): we should remove the deprecated module and instaed move to this config, however, this
  -- needs check of whether it modifies the behaviour
  -- junitConfig <- envJUnitConfig
  -- withArgs hspecArgs . hspec . add junitConfig $ do

  conf <- hspecConfig
  withArgs hspecArgs . hspecWith conf $ do
    for_ [minBound ..] $ \idpApiVersion -> do
      describe (show idpApiVersion) . beforeAll (env <&> teWireIdPAPIVersion .~ idpApiVersion) . afterAll destroyEnv $ do
        mkspecMisc
        mkspecSaml
        mkspecScim
    mkspecHscimAcceptance env destroyEnv

hspecConfig :: IO Config
hspecConfig = do
  junitConfig <- envJUnitConfig
  pure $
    defaultConfig
      { configAvailableFormatters =
          ("junit", checksAndJUnitFormatter junitConfig)
            : configAvailableFormatters defaultConfig
      }
  where
    checksAndJUnitFormatter junitConfig config = do
      junit <- junitFormat junitConfig config
      let checksFormatter = fromJust (lookup "checks" $ configAvailableFormatters defaultConfig)
      checks <- checksFormatter config
      pure $ \event -> do
        concurrently_ (junit event) (checks event)

partitionArgs :: [String] -> ([String], [String])
partitionArgs = go [] []
  where
    go wireArgs hspecArgs ("-s" : x : xs) = go (wireArgs <> ["-s", x]) hspecArgs xs
    go wireArgs hspecArgs ("-i" : x : xs) = go (wireArgs <> ["-i", x]) hspecArgs xs
    go wireArgs hspecArgs (x : xs) = go wireArgs (hspecArgs <> [x]) xs
    go wireArgs hspecArgs [] = (wireArgs, hspecArgs)

mkspecMisc :: SpecWith TestEnv
mkspecMisc = do
  describe "Logging" Test.LoggingSpec.spec
  describe "Metrics" Test.MetricsSpec.spec

mkspecSaml :: SpecWith TestEnv
mkspecSaml = do
  describe "Spar.API" Test.Spar.APISpec.spec
  describe "Spar.App" Test.Spar.AppSpec.spec
  describe "Spar.Data" Test.Spar.DataSpec.spec
  describe "Spar.Intra.Brig" Test.Spar.Intra.BrigSpec.spec

mkspecScim :: SpecWith TestEnv
mkspecScim = do
  describe "Spar.Scim.Auth" Test.Spar.Scim.AuthSpec.spec
  describe "Spar.Scim.User" Test.Spar.Scim.UserSpec.spec

mkspecHscimAcceptance :: IO TestEnv -> (TestEnv -> IO ()) -> Spec
mkspecHscimAcceptance mkenv _destroyenv = do
  describe "hscim acceptance tests" $
    microsoftAzure @SparTag AcceptanceConfig {..}
  where
    scimAppAndConfig = do
      env <- mkenv
      (app, _) <- mkApp (env ^. teOpts)
      scimAuthToken <- toHeader . fst <$> registerIdPAndScimToken `runReaderT` env
      let queryConfig = AcceptanceQueryConfig {..}
          scimPathPrefix = "/scim/v2"
      pure (app, queryConfig)
    genUserName = pack <$> replicateM 9 (randomRIO ('a', 'z'))
    responsesFullyKnown = False
