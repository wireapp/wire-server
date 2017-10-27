{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cassandra.Schema
import Control.Exception (finally)
import System.Logger hiding (info)
import Util.Options

import qualified V9
import qualified V10
import qualified V11
import qualified V12
import qualified V13
import qualified V14
import qualified V15
import qualified V16
import qualified V17
import qualified V18
import qualified V19
import qualified V20
import qualified V21
import qualified V22
import qualified V23
import qualified V24
import qualified V28
import qualified V29
import qualified V30
import qualified V31
import qualified V32
import qualified V33
import qualified V34
import qualified V35
import qualified V36
import qualified V37
import qualified V38
import qualified V39
import qualified V40
import qualified V41
import qualified V42
import qualified V43

main :: IO ()
main = do
    let desc = "Brig Cassandra Schema Migrations"
        defaultPath = "/etc/wire/brig/conf/brig-schema.yaml"
    o <- getOptions desc migrationOptsParser defaultPath
    l <- new $ setOutput StdOut . setFormat Nothing $ defSettings
    migrateSchema l o
        [ V9.migration
        , V10.migration
        , V11.migration
        , V12.migration
        , V13.migration
        , V14.migration
        , V15.migration
        , V16.migration
        , V17.migration
        , V18.migration
        , V19.migration
        , V20.migration
        , V21.migration
        , V22.migration
        , V23.migration
        , V24.migration
        , V28.migration
        , V29.migration
        , V30.migration
        , V31.migration
        , V32.migration
        , V33.migration
        , V34.migration
        , V35.migration
        , V36.migration
        , V37.migration
        , V38.migration
        , V39.migration
        , V40.migration
        , V41.migration
        , V42.migration
        , V43.migration
        ] `finally` close l
