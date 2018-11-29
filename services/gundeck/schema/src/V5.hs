{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V5 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 5 "Add user_push.fallback column" $
    schema' [r| alter columnfamily user_push add fallback int; |]
