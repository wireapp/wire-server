module Options
  ( Settings (..),
    rStart,
    rCasSettings,
    rJournalSettings,
    cHosts,
    cPort,
    cKeyspace,
    settingsParser,
  )
where

import qualified Cassandra as C
import Control.Lens
import Data.Id
import Data.Text.Strict.Lens
import qualified Data.UUID as UUID
import Galley.Options
import Imports
import Options.Applicative
import Util.Options

data JournalSettings
  = JournalSettings
      { _rStart :: !(Maybe TeamId),
        _rCasSettings :: !CassandraSettings,
        _rJournalSettings :: !JournalOpts
      }

data CassandraSettings
  = CassandraSettings
      { _cHosts :: !String,
        _cPort :: !Word16,
        _cKeyspace :: !C.Keyspace
      }
  deriving (Show)

makeLenses ''JournalSettings

makeLenses ''CassandraSettings

settingsParser :: Parser JournalSettings
settingsParser =
  JournalSettings
    <$> optional
      ( teamIdOption
          ( long "start-team-id"
              <> help "starting TeamId"
          )
      )
    <*> cassandraSettingsParser
    <*> journalOptsParser

cassandraSettingsParser :: Parser CassandraSettings
cassandraSettingsParser =
  CassandraSettings
    <$> strOption
      ( long "cassandra-host"
          <> metavar "HOST"
          <> help "Cassandra Host."
          <> value "localhost"
          <> showDefault
      )
    <*> option
      auto
      ( long "cassandra-port"
          <> metavar "PORT"
          <> help "Cassandra Port."
          <> value 9042
          <> showDefault
      )
    <*> ( C.Keyspace . view packed
            <$> strOption
              ( long "cassandra-keyspace"
                  <> metavar "STRING"
                  <> help "Cassandra Keyspace."
                  <> value "galley_test"
                  <> showDefault
              )
        )

journalOptsParser :: Parser JournalOpts
journalOptsParser =
  JournalOpts
    <$> ( fmap (view packed) $ strOption $
            long "team-events-queue-name"
              <> metavar "STRING"
              <> help "sqs queue name to send team events"
        )
    <*> ( option parseAWSEndpoint $
            long "aws-sqs-endpoint"
              <> value (AWSEndpoint "sqs.eu-west-1.amazonaws.com" True 443)
              <> metavar "STRING"
              <> showDefault
              <> help "aws endpoint"
        )

teamIdOption :: Mod OptionFields String -> Parser TeamId
teamIdOption = fmap (Id . fromMaybe (error "invalid teamId") . UUID.fromString) . strOption
