module V37 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 37 "Create table `custom_backend`" $ do
  schema'
    [r|
        CREATE TABLE custom_backend (
            domain             text,
            config_json_url    blob,
            webapp_welcome_url blob,
            PRIMARY KEY (domain)
        );
        |]
