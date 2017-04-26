{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cassandra.Schema
import Control.Exception (finally)
import Data.Monoid
import Options.Applicative
import System.Logger hiding (info)

import qualified V1
import qualified V2
import qualified V3
import qualified V4
import qualified V5
import qualified V6
import qualified V7

main :: IO ()
main = do
    o <- execParser (info (helper <*> migrationOptsParser) desc)
    l <- new $ setOutput StdOut . setFormat Nothing $ defSettings
    migrateSchema l o
        [ V1.migration
        , V2.migration
        , V3.migration
        , V4.migration
        , V5.migration
        , V6.migration
        , V7.migration
        ] `finally` close l
  where
    desc = header "Gundeck Cassandra Schema Migrations" <> fullDesc
