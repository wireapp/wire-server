module V59 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 59 "Add table for storing whitelisted email addresses" $ do
    void $ schema' [r|
        create columnfamily if not exists whitelist_emails
            ( email   text -- email that is whitelisted
            , mode    int  -- whitelisting mode (0 = only this email directly, 1 = all emails with +-based 'subemails', eg foo+bar@example.com),
            , primary key (email)
            );
    |]
