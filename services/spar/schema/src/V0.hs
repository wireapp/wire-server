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

module V0 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 0 "Initial schema" $ do
  -- FUTUREWORK: in authreq, field req, we may be able to use UUID, because we can create those?
  void $
    schema'
      [r|
        CREATE TABLE if not exists authreq
            ( req          text
            , end_of_life  timestamp
            , primary key  (req)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
        |]
  void $
    schema'
      [r|
        CREATE TABLE if not exists authresp
            ( resp         text
            , end_of_life  timestamp
            , primary key  (resp)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
        |]
  void $
    schema'
      [r|
        CREATE TABLE if not exists user
            ( issuer   text
            , sso_id   text
            , uid      uuid
            , primary key (issuer, sso_id)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
        |]
  void $
    schema'
      [r|
        CREATE TABLE if not exists idp
            ( idp           uuid
            , metadata      text
            , issuer        text
            , request_uri   text
            , public_key    blob
            , team          uuid
            , PRIMARY KEY (idp)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
        |]
  void $
    schema'
      [r|
        CREATE TABLE if not exists issuer_idp
            ( issuer        text
            , idp           uuid
            , PRIMARY KEY (issuer)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
        |]
  void $
    schema'
      [r|
        CREATE TABLE if not exists team_idp
            ( team          uuid
            , idp           uuid
            , PRIMARY KEY (team, idp)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
        |]
