{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- for ReplicationStrategy
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

-- Additional functionality on top of our cassandra library. Used by brig, brig's schema definitions, Spar, Spar's schema definitions, Galley, Galley's schema definitions, Gundeck, and Gundeck's schema definitions.

module Cassandra.Schema
  ( Migration (..),
    MigrationOpts (..),
    ReplicationStrategy (..),
    ReplicationFactor (..),
    ReplicationMap (..),
    schemaVersion,
    versionCheck,
    createKeyspace,
    useKeyspace,
    migrationOptsParser,
    schema',
  )
where

import Cassandra (Client, Consistency (All, One), Keyspace (Keyspace), QueryParams (QueryParams), QueryString (QueryString), params, query1, retry, x5)
import Control.Monad.Catch
import Data.Aeson
import Data.List.Split (splitOn)
import Data.Text (intercalate, pack)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder (fromString, fromText, toLazyText)
import Database.CQL.IO (HostResponse, getResult, request, schema)
import Database.CQL.Protocol (Query (Query), Request (RqQuery))
import Imports hiding (All, fromString, init, intercalate, log)
import Options.Applicative hiding (info)

data Migration = Migration
  { migVersion :: Int32,
    migText :: Text,
    migAction :: Client ()
  }

data MigrationOpts = MigrationOpts
  { migHost :: String,
    migPort :: Word16,
    migKeyspace :: Text,
    migRepl :: ReplicationStrategy,
    migReset :: Bool,
    migTlsCa :: Maybe FilePath
  }
  deriving (Eq, Show, Generic)

data ReplicationStrategy
  = SimpleStrategy {replicationFactor :: ReplicationFactor}
  | NetworkTopologyStrategy {dataCenters :: ReplicationMap}
  deriving (Eq, Show, Generic)

newtype ReplicationFactor = ReplicationFactor Word16
  deriving (Eq, Show, Generic)

newtype ReplicationMap = ReplicationMap [(Text, ReplicationFactor)]
  deriving (Eq, Show, Generic)

instance FromJSON ReplicationMap

instance FromJSON ReplicationFactor

instance FromJSON ReplicationStrategy

instance FromJSON MigrationOpts

instance Read ReplicationMap where
  -- ReplicationMap ::= DataCenter [("," DataCenter)*]
  -- DataCenter     ::= Name ":" ReplFactor
  -- Name           ::= Text
  -- ReplFactor     ::= Word16
  readsPrec _ s = [(ReplicationMap (dcMap s), "")]
    where
      dcMap = map dcEntry . splitOn ","
      dcEntry e = case splitOn ":" e of
        [k, v] -> (pack k, ReplicationFactor (read v))
        _ -> error $ "Failed reading: Invalid data center entry: " ++ e

schema' :: LT.Text -> Client ()
schema' q = void $ schema (QueryString q) (params All ())

schemaVersion :: Client (Maybe Int32)
schemaVersion = catch (fmap runIdentity <$> qry) h
  where
    qry = retry x5 $ query1 q (params One ())
    q = QueryString "select version from meta where id=1 order by version desc limit 1"
    h :: SomeException -> a
    h e =
      error $
        "Failed to read schema version from meta table. Error was: "
          <> show e

versionCheck :: Int32 -> Client ()
versionCheck v = do
  v' <- schemaVersion
  unless (Just v <= v') $
    error $
      "Schema Version too old! Expecting at least: "
        <> show v
        <> ", but got: "
        <> maybe "" show v'

createKeyspace :: Keyspace -> ReplicationStrategy -> Client ()
createKeyspace (Keyspace k) rs = void $ schema (cql rs) (params All ())
  where
    cql (SimpleStrategy (ReplicationFactor n)) =
      QueryString . toLazyText $
        fromText "create keyspace if not exists "
          <> fromText k
          <> fromText " with replication = { "
          <> fromText "    'class': 'SimpleStrategy' "
          <> fromText "  , 'replication_factor': '"
          <> fromString (show n)
          <> "'"
          <> fromText "};"
    cql (NetworkTopologyStrategy (ReplicationMap dcs)) =
      QueryString . toLazyText $
        fromText "create keyspace if not exists "
          <> fromText k
          <> fromText " with replication = { "
          <> fromText "    'class': 'NetworkTopologyStrategy' "
          <> fromText "  , "
          <> fromText (intercalate "," (map pair dcs))
          <> fromText "};"
    pair (dc, ReplicationFactor n) = "'" <> dc <> "': " <> pack (show n)

useKeyspace :: Keyspace -> Client ()
useKeyspace (Keyspace k) = void . getResult =<< qry
  where
    qry = request (RqQuery (Query cql prms)) :: Client (HostResponse () () ())
    prms = QueryParams One False () Nothing Nothing Nothing Nothing
    cql = QueryString $ "use \"" <> fromStrict k <> "\""

migrationOptsParser :: Parser MigrationOpts
migrationOptsParser =
  MigrationOpts
    <$> strOption
      ( long "host"
          <> metavar "HOST"
          <> value "localhost"
          <> help "Cassandra host"
      )
    <*> option
      auto
      ( long "port"
          <> metavar "PORT"
          <> value 9042
          <> help "Cassandra port"
      )
    <*> ( fmap pack . strOption $
            long "keyspace"
              <> metavar "STRING"
              <> help "Cassandra Keyspace"
        )
    <*> ( ( fmap (SimpleStrategy . ReplicationFactor) . option auto $
              long "replication-factor"
                <> metavar "INT"
                <> help "Replication Factor"
          )
            <|> ( fmap NetworkTopologyStrategy . option auto $
                    long "replication-map"
                      <> metavar "STRING"
                      <> help "Replication Map (i.e. \"eu-west:3,us-east:3\")"
                )
        )
    <*> switch
      ( long "reset"
          <> help "Reset the keyspace before running migrations"
      )
    <*> ( (optional . strOption)
            ( long "tls-ca-certificate-file"
                <> help "Location of a PEM encoded list of CA certificates to be used when verifying the Cassandra server's certificate"
            )
        )
