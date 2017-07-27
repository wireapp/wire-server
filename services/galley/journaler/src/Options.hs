{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Options
    ( Settings (..)

    , rStart
    , rCasSettings
    , rJournalSettings

    , cHosts
    , cPort
    , cKeyspace

    , settingsParser

    )
where

import Control.Lens
import Data.Id
import Data.Monoid
import Data.Text.Strict.Lens
import Data.Word
import Data.Maybe
import Galley.Options
import Options.Applicative

import qualified Data.UUID as UUID
import qualified Cassandra as C

data Settings = Settings
    { _rStart           :: !(Maybe TeamId)
    , _rCasSettings     :: !CassandraSettings
    , _rJournalSettings :: !JournalOpts
    }

data CassandraSettings = CassandraSettings
    { _cHosts    :: !String
    , _cPort     :: !Word16
    , _cKeyspace :: !C.Keyspace
    } deriving Show

makeLenses ''Settings
makeLenses ''CassandraSettings

settingsParser :: Parser Settings
settingsParser = Settings
    <$> optional ( teamIdOption
        ( long "start-team-id"
        <> help "starting TeamId"))
    <*> cassandraSettingsParser
    <*> journalOptsParser

cassandraSettingsParser :: Parser CassandraSettings
cassandraSettingsParser = CassandraSettings
    <$> strOption
        ( long    "cassandra-host"
       <> metavar "HOST"
       <> help    "Cassandra Host."
       <> value   "localhost"
       <> showDefault
        )

    <*> option auto
        ( long    "cassandra-port"
       <> metavar "PORT"
       <> help    "Cassandra Port."
       <> value   9042
       <> showDefault
        )
    <*> ( C.Keyspace . view packed <$>
          strOption
          ( long    "cassandra-keyspace"
         <> metavar "STRING"
         <> help    "Cassandra Keyspace."
         <> value   "galley_test"
         <> showDefault
          )
        )

teamIdOption :: Mod OptionFields String -> Parser TeamId
teamIdOption = fmap (Id . fromMaybe (error "invalid teamId") . UUID.fromString) . strOption
