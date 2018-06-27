{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V27 (migration) where

import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 27 "Add conversation message_timer" $
    schema' [r| ALTER TABLE conversation ADD message_timer bigint; |]
