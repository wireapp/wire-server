{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cassandra.Schema
import Control.Exception (finally)
import System.Logger hiding (info)
import Util.Options

import qualified V0

main :: IO ()
main = do
    let desc = "Spar Cassandra Schema Migrations"
        defaultPath = "/etc/wire/spar/conf/spar-schema.yaml"
    o <- getOptions desc migrationOptsParser defaultPath
    l <- new $ setOutput StdOut . setFormat Nothing $ defSettings
    migrateSchema l o
        [ V0.migration
        ] `finally` close l
