module V43 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 43 "Add team invitations" $ do
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
