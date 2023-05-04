module Testlib.Options (getOptions, TestOptions (..)) where

import Options.Applicative

data TestOptions = TestOptions
  { includeTests :: [String],
    excludeTests :: [String]
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
