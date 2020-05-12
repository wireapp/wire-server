module V40_CreateTableDataMigration (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 40 "Create table `data_migration`" $ do
  schema'
    [r|
        CREATE TABLE data_migration (
            id      int,
            version int,
            descr   text,
            date    timestamp,
            PRIMARY KEY (id, version)
        );
        |]
