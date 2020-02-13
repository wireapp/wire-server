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
