{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Monoid
import Options.Applicative
import Spar.Options
import System.Environment
import Test.Hspec
import Util

import qualified Data.Yaml as Yaml
import qualified Test.Spar.APISpec
import qualified Test.Spar.DataSpec


main :: IO ()
main = withArgs [] . hspec =<< mkspec

mkspec :: IO Spec
mkspec = do
  let desc = "Spar - SSO Service Integration Test Suite"
  (integrationConfigFilePath, configFilePath) <- execParser (info (helper <*> cliOptsParser) (header desc <> fullDesc))
  integrationOpts :: IntegrationConfig <- Yaml.decodeFileEither integrationConfigFilePath >>= either (error . show) pure
  serviceOpts :: Opts <- Yaml.decodeFileEither configFilePath >>= either (error . show) pure

  pure . beforeAll (mkEnv integrationOpts serviceOpts) . afterAll destroyEnv $ do
    describe "Test.Spar.Data" Test.Spar.DataSpec.spec
    describe "Test.Spar.API" $ Test.Spar.APISpec.spec


-- | Accept config file locations as cli options.
cliOptsParser :: Parser (String, String)
cliOptsParser = (,) <$>
  (strOption $
    long "integration-config"
    <> short 'i'
    <> help "Integration config to load"
    <> showDefault
    <> value defaultIntPath)
  <*>
  (strOption $
    long "service-config"
    <> short 's'
    <> help "Spar application config to load"
    <> showDefault
    <> value defaultSparPath)
  where
    defaultIntPath = "/etc/wire/integration/integration.yaml"
    defaultSparPath = "/etc/wire/spar/conf/spar.yaml"
