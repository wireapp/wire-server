module Main where

import Imports
import Cassandra.Schema
import Control.Exception (finally)
import Util.Options

import qualified System.Logger.Extended as Log

import qualified V0
import qualified V1
import qualified V2
import qualified V3
import qualified V4
import qualified V5
import qualified V6
import qualified V7

main :: IO ()
main = do
    let desc = "Spar Cassandra Schema Migrations"
        defaultPath = "/etc/wire/spar/conf/spar-schema.yaml"
    o <- getOptions desc (Just migrationOptsParser) defaultPath
    l <- Log.mkLogger'
    migrateSchema l o
        [ V0.migration
        , V1.migration
        , V2.migration
        , V3.migration
        , V4.migration
        , V5.migration
        , V6.migration
        , V7.migration
        -- When adding migrations here, don't forget to update
        -- 'schemaVersion' in Spar.Data

        -- TODO: Add a migration that removes unused fields
        -- (we don't want to risk running a migration which would
        -- effectively break the currently deployed spar service)
        -- see https://github.com/wireapp/wire-server/pull/476.

        ] `finally` Log.close l
