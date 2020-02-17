module V33 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 33 "Add user.assets column" $ do
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
