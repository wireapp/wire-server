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
