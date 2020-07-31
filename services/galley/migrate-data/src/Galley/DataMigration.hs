-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Galley.DataMigration (cassandraSettingsParser, migrate) where

import qualified Cassandra as C
import qualified Cassandra.Settings as C
import Control.Monad.Catch (finally)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import Galley.DataMigration.Types
import Imports
import Options.Applicative (Parser)
import qualified Options.Applicative as Opts
import System.Logger.Class (Logger)
import qualified System.Logger.Class as Log

data CassandraSettings = CassandraSettings
  { cHost :: String,
    cPort :: Word16,
    cKeyspace :: C.Keyspace
  }

cassandraSettingsParser :: Parser CassandraSettings
cassandraSettingsParser =
  CassandraSettings
    <$> Opts.strOption
      ( Opts.long "cassandra-host"
          <> Opts.value "localhost"
      )
    <*> Opts.option
      Opts.auto
      ( Opts.long "cassandra-port"
          <> Opts.value 9042
      )
    <*> ( C.Keyspace
            <$> Opts.strOption
              ( Opts.long "cassandra-keyspace"
                  <> Opts.value "galley_test"
              )
        )

migrate :: Logger -> CassandraSettings -> [Migration] -> IO ()
migrate l cas ms = do
  env <- mkEnv l cas
  finally (go env) (cleanup env)
  where
    go env =
      runMigrationAction env $
        runMigrations ms

mkEnv :: Logger -> CassandraSettings -> IO Env
mkEnv l cas =
  Env
    <$> initCassandra
    <*> initLogger
  where
    initCassandra =
      C.init $
        C.setLogger (C.mkLogger l)
          . C.setContacts (cHost cas) []
          . C.setPortNumber (fromIntegral (cPort cas))
          . C.setKeyspace (cKeyspace cas)
          . C.setProtocolVersion C.V4
          $ C.defSettings
    initLogger = pure l

-- | Runs only the migrations which need to run
runMigrations :: [Migration] -> MigrationActionT IO ()
runMigrations migrations = do
  vmax <- latestMigrationVersion
  let pendingMigrations = filter (\m -> version m > vmax) migrations
  if null pendingMigrations
    then info "No new migrations."
    else info "New migrations found."
  mapM_ runMigration pendingMigrations

runMigration :: Migration -> MigrationActionT IO ()
runMigration (Migration ver txt mig) = do
  info $ "Running: [" <> show (migrationVersion ver) <> "] " <> Text.unpack txt
  mig
  persistVersion ver txt =<< liftIO getCurrentTime

latestMigrationVersion :: MigrationActionT IO MigrationVersion
latestMigrationVersion = MigrationVersion . maybe 0 fromIntegral <$> C.query1 cql (C.params C.Quorum ())
  where
    cql :: C.QueryString C.R () (Identity Int32)
    cql = "select version from data_migration where id=1 order by version desc limit 1"

persistVersion :: MigrationVersion -> Text -> UTCTime -> MigrationActionT IO ()
persistVersion (MigrationVersion v) desc time = C.write cql (C.params C.Quorum (fromIntegral v, desc, time))
  where
    cql :: C.QueryString C.W (Int32, Text, UTCTime) ()
    cql = "insert into data_migration (id, version, descr, date) values (1,?,?,?)"

info :: Log.MonadLogger m => String -> m ()
info = Log.info . Log.msg
