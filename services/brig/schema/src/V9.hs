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

module V9 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 9 "Initial schema" $ do
  void $
    schema'
      [r|
        create columnfamily if not exists user
            ( id        uuid
            , accent    list<float> -- accent colour RGBA
            , accent_id int         -- accent colour ID
            , name      text        -- user name
            , picture   list<blob>  -- user picture(s) (asset metadata)
            , email     text
            , phone     text
            , password  blob        -- pw hash
            , activated boolean
            , primary key (id)
            );
        |]
  void $
    schema'
      [r|
        -- verified, 'unique' user attributes
        create columnfamily if not exists user_keys
            ( key  text           -- email or phone number
            , user uuid           -- user ID
            , primary key (key)
            );
        |]
  void $
    schema'
      [r|
        -- (temporary) activation keys
        create columnfamily if not exists activation_keys
            ( key      ascii -- opaque version of key_text
            , key_type ascii -- ("email" or "phone")
            , key_text text  -- the plain 'key' (phone or email)
            , code     ascii -- random code
            , user     uuid
            , retries  int   -- # of remaining attempts
            , primary key (key)
            );
        |]
  void $
    schema'
      [r|
        -- (temporary) password reset codes
        create columnfamily if not exists password_reset
            ( key      ascii -- opaque version of the user ID
            , code     ascii -- random code
            , user     uuid
            , email    text
            , primary key (key)
            );
        |]
  void $
    schema'
      [r|
        create columnfamily if not exists push
            ( ptoken    text -- token
            , app       text -- application
            , transport int  -- transport type (0 = GCM, 1 = APNS)
            , usr       uuid -- user id
            , primary key (ptoken, app, transport)
            );
        |]
  void $
    schema'
      [r|
        -- TODO: usr is a really bad secondary index!
        create index if not exists push_usr_key on push (usr);
        |]
  void $
    schema'
      [r|
        create columnfamily if not exists connection
            ( left        uuid      -- user id "from" in the relation
            , right       uuid      -- user id "to"   in the relation
            , status      int       -- relation type (0 = ACCEPTED, 1 = BLOCKED, 2 = PENDING, 3 = IGNORED)
            , last_update timestamp -- last time this relation was updated
            , message     text      -- message sent together with the request
            , conv        uuid      -- conv id between the 2 (if needed)
            , primary key (left, right)
            );
        |]
  void $
    schema'
      [r|
        create index if not exists conn_status on connection (status);
        |]
  void $
    schema'
      [r|
        create columnfamily if not exists invitation
            ( inviter     uuid      -- user id that created the invitation
            , invitee     uuid      -- user id generated with the invitation
            , email       text      -- email of the user invited
            , last_update timestamp -- last time this invitation was updated
            , primary key (inviter, email)
            );
        |]
  void $
    schema'
      [r|
        create columnfamily if not exists invitee_info
            ( invitee uuid  -- user id generated with the invitation
            , inviter uuid  -- user id that created the invitation
            , conv    uuid  -- conv id between the 2
            , primary key (invitee)
            );
        |]
