module Main where

import Cassandra.Schema
import Control.Exception (finally)
import Imports
import qualified System.Logger.Extended as Log
import Util.Options
import qualified V1
import qualified V2
import qualified V3
import qualified V4
import qualified V5
import qualified V6
import qualified V7

main :: IO ()
main = do
  o <- getOptions desc (Just migrationOptsParser) defaultPath
  l <- Log.mkLogger'
  migrateSchema
    l
    o
    [ V1.migration,
      V2.migration,
      V3.migration,
      V4.migration,
      V5.migration,
      V6.migration,
      V7.migration
    ]
    `finally` Log.close l
  where
    desc = "Gundeck Cassandra Schema Migrations"
    defaultPath = "/etc/wire/gundeck/conf/gundeck-schema.yaml"
