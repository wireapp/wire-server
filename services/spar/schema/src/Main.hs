{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cassandra.Schema
import Control.Exception (finally)
import System.Logger hiding (info)
import Util.Options

import qualified V0
import qualified V1
import qualified V2

main :: IO ()
main = do
    let desc = "Spar Cassandra Schema Migrations"
        defaultPath = "/etc/wire/spar/conf/spar-schema.yaml"
    o <- getOptions desc (Just migrationOptsParser) defaultPath
    l <- new $ setOutput StdOut . setFormat Nothing $ defSettings
    migrateSchema l o
        [ V0.migration
        , V1.migration
        , V2.migration
        -- When adding migrations here, don't forget to update
        -- 'schemaVersion' in Spar.Data
        ] `finally` close l
