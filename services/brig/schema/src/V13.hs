module V13 (migration) where

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
