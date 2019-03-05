module V4 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 4 "Add user_push.arn column" $
    schema' [r| alter columnfamily user_push add arn text; |]
