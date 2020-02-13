{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Options
  ( setCasBrig,
    setCasGalley,
    cHosts,
    cPort,
    cKeyspace,
    settingsParser,
  )
where

import qualified Cassandra as C
import Control.Lens
import Data.Text.Strict.Lens
import Imports
import Options.Applicative

data MigratorSettings
  = MigratorSettings
      { _setCasBrig :: !CassandraSettings,
        _setCasGalley :: !CassandraSettings
      }
  deriving (Show)

data CassandraSettings
  = CassandraSettings
      { _cHosts :: !String,
        _cPort :: !Word16,
        _cKeyspace :: !C.Keyspace
      }
  deriving (Show)

makeLenses ''MigratorSettings

makeLenses ''CassandraSettings

settingsParser :: Parser MigratorSettings
settingsParser =
  MigratorSettings
    <$> cassandraSettingsParser "brig"
    <*> cassandraSettingsParser "galley"

cassandraSettingsParser :: String -> Parser CassandraSettings
cassandraSettingsParser ks =
  CassandraSettings
    <$> strOption
      ( long ("cassandra-host-" ++ ks)
          <> metavar "HOST"
          <> help ("Cassandra Host for: " ++ ks)
          <> value "localhost"
          <> showDefault
      )
    <*> option
      auto
      ( long ("cassandra-port-" ++ ks)
          <> metavar "PORT"
          <> help ("Cassandra Port for: " ++ ks)
          <> value 9042
          <> showDefault
      )
    <*> ( C.Keyspace . view packed
            <$> strOption
              ( long ("cassandra-keyspace-" ++ ks)
                  <> metavar "STRING"
                  <> help ("Cassandra Keyspace for: " ++ ks)
                  <> value (ks ++ "_test")
                  <> showDefault
              )
        )
