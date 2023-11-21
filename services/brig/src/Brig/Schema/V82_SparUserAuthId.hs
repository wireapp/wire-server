{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Brig.Schema.V82_SparUserAuthId
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 82 "Support less insane recording of spar UAuthId* (formerly UserSSOId)" $ do
  schema'
    [r| ALTER TABLE user ADD (
          saml_entity_id text, -- issuer from UserRef, stored in its xml encoding so we keep all data, can be null
          saml_name_id text, -- name id from UserRef, stored in its xml encoding so we keep all data, can be null
          scim_external_id text -- what you think it is, can be null
        )
      |]
