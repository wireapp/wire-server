{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V3 (migration) where

import Cassandra.Schema
import Control.Monad (void)
import Text.RawString.QQ

migration :: Migration
migration = Migration 3 "Add cookie stash for binding existing users to sso identities" $ do

    void $ schema' [r|
        CREATE TABLE if not exists bind_cookie
            ( cookie          text
            , session_owner   uuid
            , primary key (cookie)
            )
    |]
