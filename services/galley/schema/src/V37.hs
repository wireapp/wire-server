module V37 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 37 "Create table `custom_backend`" $ do
    schema' [r|
        CREATE TABLE custom_backend (
            domain             text,
            config_json_url    blob,
            webapp_welcome_url blob,
            block_cloud_users  boolean,
            PRIMARY KEY (domain)
        );
        |]
