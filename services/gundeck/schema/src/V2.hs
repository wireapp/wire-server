
module V2 (migration) where

import Imports
import Cassandra.Schema

migration :: Migration
migration = Migration 2 "Add push_token.connection column" $ do
    schema' "alter columnfamily push add connection blob"
    schema' "alter columnfamily user_push add connection blob"
