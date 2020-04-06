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

module V29 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 29 "Create new invitations tables" $ do
  void $
    schema'
      [r|
        drop columnfamily if exists invitation |]
  void $
    schema'
      [r|
        create columnfamily if not exists invitation
            ( inviter      uuid      -- user id that created the invitation
            , id           uuid      -- invitation id reference (relevant for inviter)
            , code         ascii     -- code of the invitation (known only by invitee)
            , email        text      -- email of the user invited
            , phone        text      -- phone of the user invited
            , created_at   timestamp -- time this invitation was created
            , name         text      -- name of the invitee
            , primary key (inviter, id)
            );
        |]
  void $
    schema'
      [r|
        create columnfamily if not exists invitation_info
            ( code        ascii -- code of the invitation (known only by invitee)
            , inviter     uuid  -- user id that created the invitation
            , id          uuid  -- invitation id reference (relevant for inviter)
            , primary key (code)
            );
        |]
