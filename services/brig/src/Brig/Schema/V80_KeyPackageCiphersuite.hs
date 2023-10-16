{-# LANGUAGE QuasiQuotes #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Schema.V80_KeyPackageCiphersuite
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

-- Index key packages by ciphersuite as well as user and client.

-- Note: this migration recreates the mls_key_packages table from scratch, and
-- therefore loses all the data it contains. That means clients will need to
-- re-upload key packages after this migration is run.

migration :: Migration
migration =
  Migration 80 "Recreate mls_key_packages table" $ do
    schema' [r| DROP TABLE IF EXISTS mls_key_packages; |]
    schema'
      [r|
        CREATE TABLE mls_key_packages
            ( user uuid
            , client text
            , cipher_suite int
            , ref blob
            , data blob
            , PRIMARY KEY ((user, client, cipher_suite), ref)
        ) WITH compaction = {'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy'}
          AND gc_grace_seconds = 864000;
     |]
