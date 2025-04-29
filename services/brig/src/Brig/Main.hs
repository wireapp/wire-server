module Brig.Main where

import Brig.Options (Opts (postgresql))
import Brig.Run
import Data.Map qualified as Map
import Data.Yaml qualified as Yaml
import Imports
import OpenSSL
import Options.Applicative

main :: IO ()
main = withOpenSSL $ do
  let desc = "Brig - User Service"
      parser = (,) <$> configParser <*> commandParser
  (configFile, cmd) <- execParser (info (parser <**> helper) (fullDesc <> progDesc desc))
  opts <-
    Yaml.decodeFileEither configFile >>= \case
      Left e ->
        fail $
          show e
            <> " while attempting to decode "
            <> configFile
      Right o -> pure o
  case cmd of
    Run -> run opts
    MigratePostgres mOverrideDbName reset -> do
      let optsWithOverride =
            maybe
              opts
              (\dbname -> opts {postgresql = Map.insert "dbname" dbname opts.postgresql})
              mOverrideDbName
      migratePostres optsWithOverride reset

data Command = Run | MigratePostgres (Maybe Text) Bool

commandParser :: Parser Command
commandParser =
  hsubparser (runParser <> migratePostgresParser)
    <|> pure Run
  where
    runParser =
      command "run" (info (pure Run) (progDesc "Run the brig service"))

    migratePostgresParser = do
      let parser =
            MigratePostgres
              <$> optional
                ( strOption
                    ( long "dbname"
                        <> help "overrides the dbname from brig config"
                    )
                )
              <*> switch
                ( long "reset"
                    <> help "*WARNING* This will delete all data, so should never be run in a production environment."
                )

      command
        "migrate-postgres"
        (info parser (progDesc "Migrate the postgresql database"))

configParser :: Parser FilePath
configParser =
  strOption
    ( long "config-file"
        <> short 'c'
        <> help "Config file to load"
        <> showDefault
        <> value "/etc/wire/brig/conf/brig.yaml"
    )
