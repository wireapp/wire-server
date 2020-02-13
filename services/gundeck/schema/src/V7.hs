module V7 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 7 "Add notifications column family" $
    schema'
      [r|
        create columnfamily if not exists notifications
            ( user    uuid      -- user id
            , id      timeuuid  -- notification id
            , payload blob      -- notification payload
            , clients set<text> -- intended recipients (empty=all)
            , primary key (user, id)
            ) with clustering order by (id asc)
               and compaction  = { 'class'               : 'LeveledCompactionStrategy'
                                 , 'tombstone_threshold' : 0.1 }
               and compression = { 'sstable_compression' : 'LZ4Compressor' }
               and gc_grace_seconds = 0;
        |]
