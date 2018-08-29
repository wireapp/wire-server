{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cassandra as C
import Cassandra.Settings as C
import Control.Lens hiding ((.=))
import Data.Monoid
import Work
import Options as O
import Options.Applicative

import qualified System.Logger as Log


main :: IO ()
main = do
    s <- execParser (info (helper <*> settingsParser) desc)
    lgr <- initLogger
    bc <- initCas (s^.setCasBrig) lgr      -- Brig's Cassandra
    gc <- initCas (s^.setCasGalley) lgr    -- Galley's Cassandra
    runCommand lgr bc gc
  where
    desc = header   "service-backfill"
        <> progDesc "Backfill service tables"
        <> fullDesc

    initLogger
        = Log.new
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        $ Log.defSettings

    initCas cas l
        = C.init l
        . C.setContacts        (cas^.cHosts) []
        . C.setPortNumber      (fromIntegral $ cas^.cPort)
        . C.setKeyspace        (cas^.cKeyspace)
        . C.setProtocolVersion C.V3
        $ C.defSettings
