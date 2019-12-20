module V36 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 36 "Add extra `conversation_role` field in the member table" $ do
    schema' [r| ALTER TABLE member ADD conversation_role text; |]
