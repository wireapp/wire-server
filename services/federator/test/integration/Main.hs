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
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Control.Concurrent.Async (concurrently_)
import Imports
import OpenSSL (withOpenSSL)
import System.Environment (withArgs)
import Test.Federator.IngressSpec qualified
import Test.Federator.InwardSpec qualified
import Test.Federator.Util (TestEnv, mkEnvFromOptions)
import Test.Hspec
import Test.Hspec.JUnit
import Test.Hspec.JUnit.Config.Env
import Test.Hspec.Runner

main :: IO ()
main = withOpenSSL $ do
  (wireArgs, hspecArgs) <- partitionArgs <$> getArgs
  env <- withArgs wireArgs mkEnvFromOptions
  -- withArgs hspecArgs . hspec $ do
  --   beforeAll (pure env) . afterAll destroyEnv $ Hspec.mkspec

  -- FUTUREWORK(mangoiv): we should remove the deprecated module and instaed move to this config, however, this
  -- needs check of whether it modifies the behaviour
  -- junitConfig <- envJUnitConfig
  -- withArgs hspecArgs . hspec . add junitConfig $ do

  conf <- hspecConfig
  withArgs hspecArgs . hspecWith conf $ mkspec env

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

mkspec :: TestEnv -> Spec -- With TestEnv
mkspec env = do
  -- describe "Logging" Test.LoggingSpec.spec
  -- describe "Metrics" Test.MetricsSpec.spec
  describe "Federator.API" $ do
    Test.Federator.InwardSpec.spec env
    Test.Federator.IngressSpec.spec env
