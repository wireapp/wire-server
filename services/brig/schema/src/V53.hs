module V53 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 53 "Add tables for tracking users spawned by services" $ do
  -- A table that keeps track of all users that belong to the given
  -- service, and which conversation they are in (a bot can only be in one
  -- conversation).
  --
  -- Operations we need to support:
  --   * Delete a user knowing (provider, service, user)
  --   * Select all users for (provider, service)
  void $
    schema'
      [r|
        create table if not exists service_user
            ( provider uuid
            , service  uuid
            , user     uuid
            , conv     uuid
            , team     uuid    -- present only if the conv belongs to a team
            , primary key ((provider, service), user)
            )
    |]
  -- A table similar to 'service_user', but only tracking bots that belong
  -- to team conversations, and ordered by team.
  --
  -- Operations we need to support:
  --   * Delete a user knowing (provider, service, user):
  --     can be done by consulting the previous table
  --   * Select all users for (provider, service, team)
  void $
    schema'
      [r|
        create table if not exists service_team
            ( provider uuid
            , service  uuid
            , team     uuid
            , user     uuid
            , conv     uuid
            , primary key ((provider, service), team, user)
            )
    |]
