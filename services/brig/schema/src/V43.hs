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

module V43
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 43 "Initial brig schema at time of open-sourcing wire-server in 2017" $ do
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

  -- Switch to leveled compaction
  void $
    schema'
      [r|
        alter columnfamily user with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily user_keys with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily activation_keys with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily password_reset with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily push with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily connection with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily invitation with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily invitee_info with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]

  -- Add user.status column
  void $
    schema'
      [r|
        alter columnfamily user add status int;
        |]

  -- Lower gc_grace_seconds on all CFs to 4 days
  void $
    schema'
      [r|
        alter columnfamily user with gc_grace_seconds = 345600;
        |]
  void $
    schema'
      [r|
        alter columnfamily user_keys with gc_grace_seconds = 345600;
        |]
  void $
    schema'
      [r|
        alter columnfamily activation_keys with gc_grace_seconds = 345600;
        |]
  void $
    schema'
      [r|
        alter columnfamily password_reset with gc_grace_seconds = 345600;
        |]
  void $
    schema'
      [r|
        alter columnfamily push with gc_grace_seconds = 345600;
        |]
  void $
    schema'
      [r|
        alter columnfamily connection with gc_grace_seconds = 345600;
        |]
  void $
    schema'
      [r|
        alter columnfamily invitation with gc_grace_seconds = 345600;
        |]
  void $
    schema'
      [r|
        alter columnfamily invitee_info with gc_grace_seconds = 345600;
        |]

  -- Introduce reverse push CF (user_push) and remove index from push
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

  -- Increase gc_grace_seconds back to 10 days
  void $
    schema'
      [r|
        alter columnfamily user with gc_grace_seconds = 864000;
        |]
  void $
    schema'
      [r|
        alter columnfamily user_keys with gc_grace_seconds = 864000;
        |]
  void $
    schema'
      [r|
        alter columnfamily activation_keys with gc_grace_seconds = 864000;
        |]
  void $
    schema'
      [r|
        alter columnfamily password_reset with gc_grace_seconds = 864000;
        |]
  void $
    schema'
      [r|
        alter columnfamily push with gc_grace_seconds = 864000;
        |]
  void $
    schema'
      [r|
        alter columnfamily connection with gc_grace_seconds = 864000;
        |]
  void $
    schema'
      [r|
        alter columnfamily invitation with gc_grace_seconds = 864000;
        |]
  void $
    schema'
      [r|
        alter columnfamily invitee_info with gc_grace_seconds = 864000;
        |]

  -- Add user.tracking_id
  void $
    schema'
      [r|
       alter columnfamily user add tracking_id uuid;
       |]

  -- Drop push column family
  void $
    schema'
      [r|
       drop columnfamily if exists push;
       |]

  -- Drop user_push column family
  -- 'user_push' has been moved to gundeck
  void $
    schema'
      [r|
       drop columnfamily if exists user_push;
       |]

  -- Add prekeys
  void $
    schema'
      [r|
        create columnfamily if not exists clients
            ( user   uuid
            , client text
            , tstamp timestamp
            , type   int
            , label  text
            , primary key (user, client)
            );
       |]
  void $
    schema'
      [r|
        create columnfamily if not exists prekeys
            ( user   uuid
            , client text
            , key    int
            , data   text
            , primary key (user, client, key)
            );
       |]

  -- Add properties
  void $
    schema'
      [r|
        create columnfamily if not exists properties
            ( user   uuid
            , key    ascii
            , value  blob
            , primary key (user, key)
            );
       |]

  -- Add activation_keys.challenge
  void $
    schema'
      [r|
       alter columnfamily activation_keys add challenge ascii;
       |]

  -- Remove password_reset.email
  void $
    schema'
      [r|
       alter columnfamily password_reset drop email;
       |]

  -- Add login_codes
  void $
    schema'
      [r|
        create columnfamily if not exists login_codes
            ( user    uuid
            , code    text
            , retries int
            , timeout timestamp
            , primary key (user)
            );
       |]

  -- Add password_reset.retries and timeout
  void $
    schema'
      [r|
       alter columnfamily password_reset add retries int;
       |]
  void $
    schema'
      [r|
       alter columnfamily password_reset add timeout timestamp;
       |]

  -- Add user.language and user.country
  void $
    schema'
      [r|
       alter columnfamily user add language ascii;
       |]
  void $
    schema'
      [r|
       alter columnfamily user add country ascii;
       |]

  -- Change client IDs from ascii to text
  schema' [r| alter columnfamily clients alter client type text; |]
  schema' [r| alter columnfamily prekeys alter client type text; |]

  -- Add additional client properties
  schema' [r| alter columnfamily clients add class int; |]
  schema' [r| alter columnfamily clients add cookie text; |]

  -- Create new invitations tables
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

  -- Add even more client properties
  schema' [r| alter columnfamily clients add ip inet; |]
  schema' [r| alter columnfamily clients add lat double; |]
  schema'
    [r| alter columnfamily clients add lon double; |]
    -- Add model to clients
    schema'
    [r| alter columnfamily clients add model text; |]
    -- Add generic verification codes
    schema'
    [r|
        create columnfamily if not exists codes
            ( user    uuid
            , scope   int
            , code    text
            , retries int
            , primary key (user, scope)
            );
    |]

  -- Add user.assets column
  schema'
    [r|
        create type if not exists asset
            ( typ int
            , key text
            );
    |]
  schema'
    [r|
        alter columnfamily user add assets list<frozen<asset>>;
    |]
    -- Add vcodes table
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

  -- Add service provider tables
  schema'
    [r|
        create table if not exists provider
            ( id       uuid
            , name     text
            , email    text
            , password blob
            , url      blob
            , descr    text
            , primary key (id)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
  schema'
    [r|
        create table if not exists provider_keys
            ( key      text
            , provider uuid
            , primary key (key)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
  schema'
    [r|
        create type if not exists pubkey
            ( typ  int
            , size int
            , pem  blob
            );
    |]
  schema'
    [r|
        create table if not exists service
            ( provider     uuid
            , id           uuid
            , name         text
            , descr        text
            , base_url     blob
            , auth_tokens  list<ascii>
            , pubkeys      list<frozen<pubkey>>
            , fingerprints list<blob>
            , assets       list<frozen<asset>>
            , tags         set<bigint>
            , enabled      boolean
            , primary key (provider, id)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
  schema'
    [r|
        create table if not exists service_tag
            ( bucket   int
            , tag      bigint
            , name     text
            , service  uuid
            , provider uuid
            , primary key ((bucket, tag), name, service)
            ) with clustering order by (name asc, service asc)
              and compaction = {'class': 'LeveledCompactionStrategy'};
    |]
  schema'
    [r|
        alter table user add provider uuid;
    |]
  schema'
    [r|
        alter table user add service uuid;
    |]
    -- Add asset.size attribute
    schema'
    [r|
        alter type asset add size int;
    |]
    -- Add budget table
    schema'
    [r|
        create table if not exists budget
            ( key    text
            , budget int
            , primary key (key)
            ) with compaction = {'class': 'LeveledCompactionStrategy'}
              and gc_grace_seconds = 0;
    |]

  -- Add user handles
  schema'
    [r|
        create table if not exists unique_claims
            ( value  text
            , claims set<uuid>
            , primary key (value)
            ) with compaction = {'class': 'LeveledCompactionStrategy'}
              and gc_grace_seconds = 0;
    |]
  schema'
    [r|
        create table if not exists user_handle
            ( handle text
            , user   uuid
            , primary key (handle)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
  schema'
    [r|
        alter table user add handle text;
    |]
    -- Add user_cookies table
    schema'
    [r|
        create table if not exists user_cookies
            ( user    uuid
            , expires timestamp
            , id      bigint
            , label   text
            , type    int
            , created timestamp
            , succ_id bigint
            , primary key (user, expires, id)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
    -- Add hashed userkeys table
    schema'
    [r|
        create table if not exists user_keys_hash
            ( key      blob
            , key_type int  -- hash type (0 = PHONE, 1 = EMAIL)
            , user     uuid
            , primary key (key)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
    |]
    -- Add searchable field to user table
    schema'
    [r|
        alter table user add searchable boolean
    |]
    -- Remove user.tracking_id
    void
    $ schema'
      [r|
       alter columnfamily user drop tracking_id;
       |]

  -- Add team invitations
  schema'
    [r|
        create columnfamily if not exists team_invitation
            ( team         uuid      -- team id that owns the invitation
            , id           uuid      -- invitation id reference (relevant for the team)
            , code         ascii     -- code of the invitation (known only by invitee)
            , email        text      -- email of the user invited
            , created_at   timestamp -- time this invitation was created
            , primary key (team, id)
            );
        |]
  schema'
    [r|
        create columnfamily if not exists team_invitation_info
            ( code        ascii -- code of the invitation (known only by invitee)
            , team        uuid  -- team id that created the invitation
            , id          uuid  -- invitation id reference (relevant for the team)
            , primary key (code)
            );
        |]
