{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module V59
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 59 "Add invitee_name and phone to the team invitations table" $ do
  schema' [r| ALTER TABLE team_invitation ADD (invitee_name text, phone text); |]
