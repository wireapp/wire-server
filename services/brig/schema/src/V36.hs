module V36 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 36 "Add asset.size attribute" $
    schema' [r|
        alter type asset add size int;
    |]
