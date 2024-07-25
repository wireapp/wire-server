{-# LANGUAGE QuasiQuotes #-}

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

module Brig.Schema.V84_DropUserKeysHashed
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

-- user_keys_hash usage was removed in https://github.com/wireapp/wire-server/pull/2902
--
-- However, it's dangerous to remove a cassandra table together with the usage, as
-- during deployment, there is a time window where the schema migration has run, but the
-- old code still serves traffic, which then leads to 5xxs and user-observable errors.
-- Therefore the policy is to wait a reasonable amount of time (6 months) to allow all
-- installations to upgrade before removing the database tables. See also
-- backwards-incompatbile schema migration docs in
-- https://docs.wire.com/developer/developer/cassandra-interaction.html?highlight=backwards+incompatbile#backwards-incompatible-schema-changes
--

migration :: Migration
migration = Migration 84 "Drop deprecated user_keys_hashed table" $ do
  schema'
    [r|
      DROP TABLE IF EXISTS user_keys_hash
   |]
