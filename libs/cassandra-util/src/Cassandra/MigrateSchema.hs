{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Cassandra.MigrateSchema (migrateSchema) where

import Cassandra (Client, Consistency (All, One), Keyspace (Keyspace), PrepQuery, QueryString (QueryString), R, S, Version (V4), W, params, query, query1, retry, runClient, write, x1)
import Cassandra.Schema
import Cassandra.Settings (Policy, defSettings, initialContactsPlain, setConnectTimeout, setContacts, setLogger, setMaxConnections, setPolicy, setPoolStripes, setPortNumber, setProtocolVersion, setResponseTimeout, setSendTimeout)
import Cassandra.Util (initCassandra)
import Control.Retry
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (pack)
import Data.Text.Lazy (fromStrict)
import Data.Time.Clock
import Data.UUID (UUID)
import Database.CQL.IO (Policy (Policy, acceptable, current, display, hostCount, onEvent, select, setup), schema)
import Database.CQL.IO.Tinylog qualified as CT
import Imports hiding (All, fromString, init, intercalate, log)
import System.Logger qualified as Log

-- FUTUREWORK: We could use the System.Logger.Class here in the future, but we don't have a ReaderT IO here (yet)
migrateSchema :: Log.Logger -> MigrationOpts -> [Migration] -> IO ()
migrateSchema l o ms = do
  hosts <- initialContactsPlain $ pack (migHost o)
  let cqlSettings =
        setLogger (CT.mkLogger l)
          . setContacts (NonEmpty.head hosts) (NonEmpty.tail hosts)
          . setPortNumber (fromIntegral $ migPort o)
          . setMaxConnections 1
          . setPoolStripes 1
          -- 'migrationPolicy' ensures we only talk to one host for all queries
          -- required for correct functioning of 'waitForSchemaConsistency'
          . setPolicy migrationPolicy
          -- use higher timeouts on schema migrations to reduce the probability
          -- of a timeout happening during 'migAction' or 'metaInsert',
          -- as that can lead to a state where schema migrations cannot be re-run
          -- without manual action.
          -- (due to e.g. "cannot create table X, already exists" errors)
          . setConnectTimeout 20
          . setSendTimeout 20
          . setResponseTimeout 50
          . setProtocolVersion V4
          $ defSettings
  cas <- initCassandra cqlSettings o.migTlsCa l
  runClient cas $ do
    let keyspace = Keyspace . migKeyspace $ o
    when (migReset o) $ do
      info "Dropping keyspace."
      void $ schema (dropKeyspace keyspace) (params All ())
    createKeyspace keyspace (migRepl o)
    useKeyspace keyspace
    void $ schema metaCreate (params All ())
    migrations <- newer <$> schemaVersion
    if null migrations
      then info "No new migrations."
      else info "New migrations found."
    forM_ migrations $ \Migration {..} -> do
      info $ "[" <> pack (show migVersion) <> "] " <> migText
      migAction
      now <- liftIO getCurrentTime
      write metaInsert (params All (migVersion, migText, now))
      info "Waiting for schema version consistency across peers..."
      waitForSchemaConsistency
      info "... done waiting."
  where
    newer v =
      dropWhile (maybe (const False) (>=) v . migVersion)
        . sortBy (\x y -> migVersion x `compare` migVersion y)
        $ ms
    info = liftIO . Log.log l Log.Info . Log.msg
    dropKeyspace :: Keyspace -> QueryString S () ()
    dropKeyspace (Keyspace k) = QueryString $ "drop keyspace if exists \"" <> fromStrict k <> "\""
    metaCreate :: QueryString S () ()
    metaCreate = "create columnfamily if not exists meta (id int, version int, descr text, date timestamp, primary key (id, version))"
    metaInsert :: QueryString W (Int32, Text, UTCTime) ()
    metaInsert = "insert into meta (id, version, descr, date) values (1,?,?,?)"

-- | Retrieve and compare local and peer system schema versions.
-- if they don't match, retry once per second for 30 seconds
waitForSchemaConsistency :: Client ()
waitForSchemaConsistency = do
  void $ retryWhileN 30 inDisagreement getSystemVersions
  where
    getSystemVersions :: Client (UUID, [UUID])
    getSystemVersions = do
      -- These two sub-queries must be made to the same node.
      -- (comparing local from node A and peers from node B wouldn't be correct)
      -- using the custom 'migrationPolicy' when connecting to cassandra ensures this.
      mbLocalVersion <- systemLocalVersion
      peers <- systemPeerVersions
      case mbLocalVersion of
        Just localVersion -> pure $ (localVersion, peers)
        Nothing -> error "No system_version in system.local (should never happen)"
    inDisagreement :: (UUID, [UUID]) -> Bool
    inDisagreement (localVersion, peers) = not $ all (== localVersion) peers
    systemLocalVersion :: Client (Maybe UUID)
    systemLocalVersion = fmap runIdentity <$> qry
      where
        qry = retry x1 (query1 cql (params One ()))
        cql :: PrepQuery R () (Identity UUID)
        cql = "select schema_version from system.local"
    systemPeerVersions :: Client [UUID]
    systemPeerVersions = fmap runIdentity <$> qry
      where
        qry = retry x1 (query cql (params One ()))
        cql :: PrepQuery R () (Identity UUID)
        cql = "select schema_version from system.peers"

retryWhileN :: (MonadIO m) => Int -> (a -> Bool) -> m a -> m a
retryWhileN n f m =
  retrying
    (constantDelay 1000000 <> limitRetries n)
    (const (pure . f))
    (const m)

-- | The migrationPolicy selects only one and always the same host
migrationPolicy :: IO Policy
migrationPolicy = do
  h <- newIORef Nothing
  pure $
    Policy
      { setup = setHost h,
        onEvent = const $ pure (),
        select = readIORef h,
        acceptable = const $ pure True,
        hostCount = fromIntegral . length . maybeToList <$> readIORef h,
        display = ("migrationPolicy: " ++) . show <$> readIORef h,
        current = maybeToList <$> readIORef h
      }
  where
    setHost h (a : _) _ = writeIORef h (Just a)
    setHost _ _ _ = pure ()
