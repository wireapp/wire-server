{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cassandra.Schema
import Control.Exception (finally)
import Data.Monoid ((<>))
import Options.Applicative
import System.Logger hiding (info)

import qualified V20
import qualified V21
import qualified V22
import qualified V23
import qualified V24
import qualified V25

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
        ]
      `finally`
        close l
  where
    desc = header "Galley Cassandra Schema" <> fullDesc
