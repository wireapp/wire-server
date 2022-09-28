{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Cassandra as C
import Cassandra.Settings as C
import ClientsWithoutPrekeys.Cache
import Control.Exception
import Control.Lens hiding (argument)
import Data.Text.Strict.Lens
import Options.Applicative
import System.IO

main :: IO ()
main = do
  Opts {..} <- execParser (info (helper <*> optParser) desc)
  client <-
    C.init
      . C.setContacts cHost []
      . C.setPortNumber (fromIntegral cPort)
      . C.setKeyspace cKeyspace
      $ C.defSettings
  creationTimes <- cacheByTimestamp client inputFNs `finally` shutdown client
  maybe
    (print creationTimes)
    (($ flip hPrint creationTimes) . flip withFile WriteMode)
    output
  where
    desc =
      header "clients-without-prekeys-from-logs-meta"
        <> progDesc "find metadata for clients without prekeys in Cassandra brig"
        <> fullDesc

data Opts = Opts
  { cHost :: String,
    cPort :: Int,
    cKeyspace :: C.Keyspace,
    output :: Maybe FilePath,
    inputFNs :: [FilePath]
  }

optParser :: Parser Opts
optParser =
  Opts
    <$> strOption
      ( long "cassandra-host"
          <> short 's'
          <> metavar "HOST"
          <> help "Cassandra Host"
          <> value "localhost"
          <> showDefault
      )
    <*> option
      auto
      ( long "cassandra-port"
          <> short 'p'
          <> metavar "PORT"
          <> help "Cassandra Port"
          <> value 9042
          <> showDefault
      )
    <*> ( C.Keyspace . view packed
            <$> strOption
              ( long "cassandra-keyspace"
                  <> short 'k'
                  <> metavar "KEYSPACE"
                  <> help "Cassandra Keyspace"
                  <> value "brig_test"
                  <> showDefault
              )
        )
    <*> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> metavar "OUTPUT"
              <> help "output file"
          )
      )
    <*> many (argument str (metavar "CSV..."))
