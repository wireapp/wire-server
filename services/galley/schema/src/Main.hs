{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cassandra.Schema
import Control.Exception (finally)
import Data.Monoid ((<>))
import Options.Applicative
import System.Logger hiding (info)

import qualified V20
import qualified V21

main :: IO ()
main = do
    o <- execParser (info (helper <*> migrationOptsParser) desc)
    l <- new $ setOutput StdOut . setFormat Nothing $ defSettings
    migrateSchema l o
        [ V20.migration
        , V21.migration
        ]
      `finally`
        close l
  where
    desc = header "Galley Cassandra Schema" <> fullDesc
