{-# LANGUAGE RecordWildCards #-}

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

module Spar.DataMigration.Run where

import qualified Cassandra as C
import qualified Cassandra.Settings as C
import Control.Lens
import Control.Monad.Catch (finally)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import Imports
import qualified Options.Applicative as Opts
import Spar.DataMigration.Options (settingsParser)
import Spar.DataMigration.Types
import qualified Spar.DataMigration.V1_ExternalIds as V1
import qualified System.Logger as Log

main :: IO ()
main = do
  settings <- Opts.execParser (Opts.info (Opts.helper <*> settingsParser) desc)
  migrate
    settings
    [V1.migration]
  where
    desc = Opts.header "Spar Cassandra Data Migrations" <> Opts.fullDesc

migrate :: MigratorSettings -> [Migration] -> IO ()
migrate settings ms = do
  env <- mkEnv settings
  runMigrations env ms `finally` cleanup env

mkEnv :: MigratorSettings -> IO Env
mkEnv settings = do
  lgr <- initLogger settings
  spar <- initCassandra (settings ^. setCasSpar) lgr
  brig <- initCassandra (settings ^. setCasBrig) lgr
  pure $ Env spar brig lgr (settings ^. setPageSize) (settings ^. setDebug) (settings ^. setDryRun)
  where
    initLogger s =
      Log.new
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        . Log.setLogLevel
          (if s ^. setDebug == Debug then Log.Debug else Log.Info)
        $ Log.defSettings
    initCassandra cas l =
      C.init
        . C.setLogger (C.mkLogger l)
        . C.setContacts (cas ^. cHosts) []
        . C.setPortNumber (fromIntegral $ cas ^. cPort)
        . C.setKeyspace (cas ^. cKeyspace)
        . C.setProtocolVersion C.V4
        $ C.defSettings

cleanup :: (MonadIO m) => Env -> m ()
cleanup env = do
  C.shutdown (sparCassandra env)
  C.shutdown (brigCassandra env)
  Log.close (logger env)

runMigrations :: Env -> [Migration] -> IO ()
runMigrations env migrations = do
  vmax <- latestMigrationVersion env
  let pendingMigrations = filter (\m -> version m > vmax) migrations
  if null pendingMigrations
    then info env "No new migrations."
    else info env "New migrations found."
  mapM_ (runMigration env) pendingMigrations

runMigration :: Env -> Migration -> IO ()
runMigration env@Env {..} (Migration ver txt mig) = do
  info env $ "Running: [" <> show (migrationVersion ver) <> "] " <> Text.unpack txt
  mig env
  unless (dryRun == DryRun) $
    persistVersion env ver txt =<< liftIO getCurrentTime

latestMigrationVersion :: Env -> IO MigrationVersion
latestMigrationVersion Env {..} =
  MigrationVersion . maybe 0 fromIntegral
    <$> C.runClient
      sparCassandra
      (C.query1 cql (C.params C.Quorum ()))
  where
    cql :: C.QueryString C.R () (Identity Int32)
    cql = "select version from data_migration where id=1 order by version desc limit 1"

persistVersion :: Env -> MigrationVersion -> Text -> UTCTime -> IO ()
persistVersion Env {..} (MigrationVersion v) desc time =
  C.runClient sparCassandra $
    C.write cql (C.params C.Quorum (fromIntegral v, desc, time))
  where
    cql :: C.QueryString C.W (Int32, Text, UTCTime) ()
    cql = "insert into data_migration (id, version, descr, date) values (1,?,?,?)"

info :: Env -> String -> IO ()
info Env {..} msg = Log.info logger $ Log.msg $ msg
