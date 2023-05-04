module Testlib.Options (getOptions, TestOptions (..)) where

import Options.Applicative

data TestOptions = TestOptions
  { includeTests :: [String],
    excludeTests :: [String],
    configFile :: String
  }

parser :: Parser TestOptions
parser =
  TestOptions
    <$> many
      ( strOption
          ( long "include"
              <> short 'i'
              <> metavar "PATTERN"
              <> help "Include tests matching PATTERN"
          )
      )
    <*> many
      ( strOption
          ( long "exclude"
              <> short 'x'
              <> metavar "PATTERN"
              <> help "Exclude tests matching PATTERN"
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
getOptions = execParser optInfo
