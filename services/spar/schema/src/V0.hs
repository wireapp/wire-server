module V0 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 0 "Initial schema" $ do
  -- FUTUREWORK: in authreq, field req, we may be able to use UUID, because we can create those?
  void $
    schema'
      [r|
        CREATE TABLE if not exists authreq
            ( req          text
            , end_of_life  timestamp
            , primary key  (req)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
        |]
  void $
    schema'
      [r|
        CREATE TABLE if not exists authresp
            ( resp         text
            , end_of_life  timestamp
            , primary key  (resp)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
        |]
  void $
    schema'
      [r|
        CREATE TABLE if not exists user
            ( issuer   text
            , sso_id   text
            , uid      uuid
            , primary key (issuer, sso_id)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
        |]
  void $
    schema'
      [r|
        CREATE TABLE if not exists idp
            ( idp           uuid
            , metadata      text
            , issuer        text
            , request_uri   text
            , public_key    blob
            , team          uuid
            , PRIMARY KEY (idp)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
        |]
  void $
    schema'
      [r|
        CREATE TABLE if not exists issuer_idp
            ( issuer        text
            , idp           uuid
            , PRIMARY KEY (issuer)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
        |]
  void $
    schema'
      [r|
        CREATE TABLE if not exists team_idp
            ( team          uuid
            , idp           uuid
            , PRIMARY KEY (team, idp)
            ) with compaction = {'class': 'LeveledCompactionStrategy'};
        |]
