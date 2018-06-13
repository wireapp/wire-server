{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Monoid
import Options.Applicative
import Spar.Options
import Test.Hspec

import qualified Test.Spar.APISpec
import qualified Test.Spar.DataSpec


main :: IO ()
main = hspec =<< spec

spec :: IO Spec
spec = do
  let desc = "Spar - SSO Service Integration Test Suite"
  (_integrationConfigFilePath, configFilePath) <- execParser (info (helper <*> cliOptsParser) (header desc <> fullDesc))
  _opts :: Opts <- readOptsFile configFilePath
  -- intopts :: IntegrationConfig <- readOptsFile integrationConfigFilePath

  let specData = describe "Test.Spar.Data" Test.Spar.DataSpec.spec
  specAPI <- describe "Test.Spar.API" <$> Test.Spar.APISpec.mkspec
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
    <> help "Brig application config to load"
    <> showDefault
    <> value defaultBrigPath)
  where
    defaultIntPath = "/etc/wire/spar/conf/integration.yaml"
    defaultBrigPath = "/etc/wire/spar/conf/spar.yaml"
