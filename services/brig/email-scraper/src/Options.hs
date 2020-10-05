{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Options
    ( MigratorSettings (..)
    , CassandraSettings

    , setCasBrig
    , setDomains

    , cHosts
    , cPort
    , cKeyspace

    , settingsParser

    )
where

import Control.Lens
import Data.Monoid
import Data.Text.Strict.Lens
import Data.Text (splitOn, pack)
import Data.Word
import Imports
import Options.Applicative
import Options.Applicative.Types

import qualified Cassandra as C

data MigratorSettings = MigratorSettings
    { _setCasBrig   :: !CassandraSettings
    , _setDomains   :: ![Text]
    } deriving Show

data CassandraSettings = CassandraSettings
    { _cHosts    :: !String
    , _cPort     :: !Word16
    , _cKeyspace :: !C.Keyspace
    } deriving Show

makeLenses ''MigratorSettings
makeLenses ''CassandraSettings

settingsParser :: Parser MigratorSettings
settingsParser = MigratorSettings
    <$> cassandraSettingsParser "brig"
    <*> domainsParser

parseDomains :: ReadM [Text]
parseDomains = readerAsk >>= parse
  where
    parse :: String -> ReadM [Text]
    parse = return . splitOn "," . pack

domainsParser :: Parser [Text]
domainsParser =
    option parseDomains
        ( long    "domains"
       <> metavar "DOMAINS"
       <> help    "Domains to check, comma separated: "
       <> value   ["example.com"]
       <> showDefault
        )

cassandraSettingsParser :: String -> Parser CassandraSettings
cassandraSettingsParser ks = CassandraSettings
    <$> strOption
        ( long    ("cassandra-host-" ++ ks)
       <> metavar "HOST"
       <> help    ("Cassandra Host for: " ++ ks)
       <> value   "localhost"
       <> showDefault
        )

    <*> option auto
        ( long    ("cassandra-port-" ++ ks)
       <> metavar "PORT"
       <> help    ("Cassandra Port for: " ++ ks)
       <> value   9042
       <> showDefault
        )
    <*> ( C.Keyspace . view packed <$>
          strOption
          ( long    ("cassandra-keyspace-" ++ ks)
         <> metavar "STRING"
         <> help    ("Cassandra Keyspace for: " ++ ks)
         <> value   (ks ++ "_test")
         <> showDefault
          )
        )
