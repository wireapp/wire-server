module Main where

import Imports
import Cassandra.Schema
import Control.Exception (finally)
import Options.Applicative

import qualified System.Logger.Extended as Log

import qualified V20
import qualified V21
import qualified V22
import qualified V23
import qualified V24
import qualified V25
import qualified V26
import qualified V27
import qualified V28
import qualified V29
import qualified V30
import qualified V31
import qualified V32
import qualified V33
import qualified V34

main :: IO ()
main = do
    o <- execParser (info (helper <*> migrationOptsParser) desc)
    l <- Log.mkLogger'
    migrateSchema l o
        [ V20.migration
        , V21.migration
        , V22.migration
        , V23.migration
        , V24.migration
        , V25.migration
        , V26.migration
        , V27.migration
        , V28.migration
        , V29.migration
        , V30.migration
        , V31.migration
        , V32.migration
        , V33.migration
        , V34.migration
        -- When adding migrations here, don't forget to update
        -- 'schemaVersion' in Galley.Data
        ]
      `finally`
        Log.close l
  where
    desc = header "Galley Cassandra Schema" <> fullDesc
