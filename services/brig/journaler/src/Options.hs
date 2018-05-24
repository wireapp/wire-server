{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Options
    ( rStart
    , rCasSettings
    , rJournalQueue

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
import Data.Text (Text)
import Options.Applicative
import Util.Options.Common

import qualified Data.UUID as UUID
import qualified Cassandra as C

data JournalSettings = JournalSettings
    { _rStart           :: !(Maybe UserId)
    , _rCasSettings     :: !CassandraSettings
    , _rJournalQueue    :: !Text
    }

data CassandraSettings = CassandraSettings
    { _cHosts    :: !String
    , _cPort     :: !Word16
    , _cKeyspace :: !C.Keyspace
    } deriving Show

makeLenses ''JournalSettings
makeLenses ''CassandraSettings

settingsParser :: Parser JournalSettings
settingsParser = JournalSettings
    <$> optional ( userIdOption
        ( long "start-user-id"
        <> help "starting UserId"))
    <*> cassandraSettingsParser
    <*> (textOption $
      long "aws-user-journal-queue" <> metavar "STRING" <>
      help "Event journal queue for user events (e.g. user deletion)")

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

userIdOption :: Mod OptionFields String -> Parser UserId
userIdOption = fmap (Id . fromMaybe (error "invalid userId") . UUID.fromString) . strOption
