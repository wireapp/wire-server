{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Monoid
import Options.Applicative
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
  (integrationConfigFilePath, _configFilePath) <- execParser (info (helper <*> cliOptsParser) (header desc <> fullDesc))
  opts :: IntegrationConfig <- Yaml.decodeFileEither integrationConfigFilePath >>= either (error . show) pure

  let specData = describe "Test.Spar.Data" Test.Spar.DataSpec.spec
      specAPI  = describe "Test.Spar.API" $ Test.Spar.APISpec.spec opts
  pure $ specData >> specAPI


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
    defaultIntPath = "/etc/wire/spar/conf/integration.yaml"
    defaultSparPath = "/etc/wire/spar/conf/spar.yaml"
