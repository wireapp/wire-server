module Testlib.Options where

import Data.Either.Extra (eitherToMaybe)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Options.Applicative
import System.Environment (lookupEnv)
import Prelude

data TestSuite
  = IntegrationSuite
  | PerformanceSuite

data TestOptions = TestOptions
  { includeTests :: [String],
    excludeTests :: [String],
    testSuite :: TestSuite,
    listTests :: Bool,
    xmlReport :: Maybe FilePath,
    configFile :: String
  }

parseSuite :: String -> Either String TestSuite
parseSuite = \case
  "integration" -> Right IntegrationSuite
  "performance" -> Right PerformanceSuite
  "int" -> Right IntegrationSuite
  "perf" -> Right PerformanceSuite
  x -> Left $ "Invalid test suite: " <> x

showSuite :: TestSuite -> String
showSuite IntegrationSuite = "integration"
showSuite PerformanceSuite = "performance"

parser :: Parser TestOptions
parser =
  TestOptions
    <$> many
      ( strOption
          ( long "include"
              <> short 'i'
              <> metavar "PATTERN"
              <> help "Include tests matching PATTERN (simple substring match). This flag can be provided multiple times. This flag can also be provided via the TEST_INCLUDE environment variable."
          )
      )
    <*> many
      ( strOption
          ( long "exclude"
              <> short 'x'
              <> metavar "PATTERN"
              <> help "Exclude tests matching PATTERN (simple substring match). This flag can be provided multiple times. This flag can also be provided via the TEST_EXCLUDE environment variable."
          )
      )
    <*> option
      (eitherReader parseSuite)
      ( long "suite"
          <> metavar "SUITE"
          <> value IntegrationSuite
          <> showDefaultWith showSuite
          <> help "Test suite to run. This flag can also be provided via the TEST_SUITE environment variable."
      )
    <*> switch (long "list" <> short 'l' <> help "Only list tests.")
    <*> optional
      ( strOption
          ( long "xml"
              <> metavar "FILE"
              <> help "Generate XML report for the tests"
          )
      )
    <*> strOption
      ( long "config"
          <> short 'c'
          <> metavar "FILE"
          <> help "Use configuration FILE"
          <> value "services/integration.yaml"
      )

optInfo :: ParserInfo TestOptions
optInfo =
  info
    (parser <**> helper)
    ( fullDesc
        <> progDesc "Run integration tests"
        <> header "integration - wire-server integration test suite"
    )

getOptions :: IO TestOptions
getOptions = do
  defaultsInclude <- maybe [] (splitOn ",") <$> lookupEnv "TEST_INCLUDE"
  defaultsExclude <- maybe [] (splitOn ",") <$> lookupEnv "TEST_EXCLUDE"
  defaultsSuite <- (>>= eitherToMaybe . parseSuite) <$> lookupEnv "TEST_SUITE"
  defaultsXMLReport <- lookupEnv "TEST_XML"
  opts <- execParser optInfo
  pure
    opts
      { includeTests = includeTests opts `orFromEnv` defaultsInclude,
        excludeTests = excludeTests opts `orFromEnv` defaultsExclude,
        xmlReport = xmlReport opts `orFromEnv` defaultsXMLReport,
        testSuite = fromMaybe opts.testSuite defaultsSuite
      }
  where
    orFromEnv fromArgs fromEnv =
      if null fromArgs
        then fromEnv
        else fromArgs
