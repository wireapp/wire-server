module V34 (migration) where

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
