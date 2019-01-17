{-# LANGUAGE OverloadedStrings #-}

module Main where

import Imports
import Cassandra.Schema
import Control.Exception (finally)
import Options.Applicative
import System.Logger hiding (info)

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

main :: IO ()
main = do
    o <- execParser (info (helper <*> migrationOptsParser) desc)
    l <- new $ setOutput StdOut . setFormat Nothing $ defSettings
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
        -- When adding migrations here, don't forget to update
        -- 'schemaVersion' in Galley.Data
        ]
      `finally`
        close l
  where
    desc = header "Galley Cassandra Schema" <> fullDesc
