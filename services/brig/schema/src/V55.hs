{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V55 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 55 "Add perms to team invitations" $ do
    schema' [r| CREATE TYPE permissions (self bigint, copy bigint); |]

    schema' [r| alter table team_invitation add perms frozen<permissions>; |]
