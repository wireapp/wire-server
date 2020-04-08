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

module V13
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 13 "Introduce reverse push CF (user_push) and remove index from push" $ do
  void $
    schema'
      [r|
            create columnfamily if not exists user_push
                ( usr       uuid -- user id
                , ptoken    text -- token
                , app       text -- application
                , transport int  -- transport type (0 = GCM, 1 = APNS)
                , primary key (usr, ptoken, app, transport)
                );
            |]
  void $
    schema'
      [r|
        drop index if exists push_usr_key;
        |]
