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

module V52 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 52 "Add service whitelist table" $ do
  -- NB. It's expected that for every team there'll only be a few
  -- whitelisted services (tens? maybe less).
  void $
    schema'
      [r|
        create table if not exists service_whitelist
            ( team     uuid
            , provider uuid
            , service  uuid
            , primary key (team, provider, service)
            ) with clustering order by (provider asc, service asc)
    |]
  -- When a service is deleted, we have to remove it from the whitelist.
  -- Since 'service_whitelist' has 'team' as the partition key, Cassandra
  -- won't allow a naive "delete ... where service = X" query. Hence, we
  -- will create and maintain a reverse index for the whitelist.
  void $
    schema'
      [r|
        create table if not exists service_whitelist_rev
            ( team     uuid
            , provider uuid
            , service  uuid
            , primary key ((provider, service), team)
            )
    |]
