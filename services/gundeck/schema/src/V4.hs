module V4 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 4 "Add user_push.arn column" $
    schema' [r| alter columnfamily user_push add arn text; |]
