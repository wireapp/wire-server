module V38AddRemoteIdentifiers (addRemoteIdentifiers) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

addRemoteIdentifiers :: Migration
addRemoteIdentifiers = Migration 38 "Add remote identifiers to conversation related tables" $ do
  -- The user table answers the question:  What conversations am I a part of?
  -- In the context of federation one now needs to know: Where is this
  -- conversation located?
  schema'
    [r|
      ALTER TABLE user ADD (
        remote_conv        uuid,
        remote_conv_domain text
      );
    |]
  -- member table is used to answer the question: What users are part of a
  -- conversation?  In the context of federation it is now interesting to know:
  -- Are those users remote or local?
  schema'
    [r|
      ALTER TABLE member ADD (
        remote_user        uuid,
        remote_user_domain text
      );
    |]
