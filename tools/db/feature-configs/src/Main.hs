{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Cassandra as C
import Cassandra.Settings as C
import Imports
import Options as O
import Options.Applicative
import System.Logger qualified as Log
import Work

main :: IO ()
main = do
  s <- execParser (info (helper <*> settingsParser) desc)
  lgr <- initLogger
  cs <- initCas s.casGalley lgr
  runCommand
    Opts
      { granularity = s.granularity,
        logger = lgr,
        clientState = cs,
        feature = s.feature,
        selector = s.selector,
        update = s.update,
        dryRun = s.dryRun
      }
  where
    desc =
      header "feature-configs"
        <> progDesc "DB Operations on feature-configs"
        <> fullDesc
    initLogger =
      Log.new
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        $ Log.defSettings
    initCas cas l =
      C.init
        . C.setLogger (C.mkLogger l)
        . C.setContacts (cas.hosts) []
        . C.setPortNumber (fromIntegral $ cas.port)
        . C.setKeyspace (cas.keyspace)
        . C.setProtocolVersion C.V4
        $ C.defSettings
