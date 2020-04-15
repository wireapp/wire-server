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

module V34
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 34 "Add vcodes table" $
    -- Supposed to cover all existing use-cases for short-lived
    -- verification codes sent either by e-mail, sms or voice call,
    -- eventually superseding the 'activation_keys', 'login_codes',
    -- 'password_reset' and 'codes' tables.
    schema'
      [r|
        create table if not exists vcodes
            ( key       ascii -- opaque 'email' or 'phone'
            , scope     int
            , value     ascii -- secret value
            , retries   int   -- attempts left
            , email     text  -- email address (xor phone)
            , phone     text  -- phone number (xor email)
            , account   uuid  -- optional associated account ID
            , primary key (key, scope)
            ) with compaction = {'class': 'LeveledCompactionStrategy'}
              and gc_grace_seconds = 0;
    |]
