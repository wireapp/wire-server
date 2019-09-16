{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Imports
import Cassandra as C
import Cassandra.Settings as C
import Control.Lens hiding ((.=))
import Work
import Options as O
import Options.Applicative

import qualified System.Logger as Log


main :: IO ()
main = do
    s <- execParser (info (helper <*> settingsParser) desc)
    lgr <- initLogger
    bc <- initCas (s^.setCasBrig) lgr      -- Brig's Cassandra
    runCommand lgr bc
  where
    desc = header   "auto-whitelist"
        <> progDesc "Whitelist all services used by teams"
        <> fullDesc

    initLogger
        = Log.new  -- TODO: use mkLogger'?
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        $ Log.defSettings

    initCas cas l
        = C.init
        . C.setLogger          (C.mkLogger l)
        . C.setContacts        (cas^.cHosts) []
        . C.setPortNumber      (fromIntegral $ cas^.cPort)
        . C.setKeyspace        (cas^.cKeyspace)
        . C.setProtocolVersion C.V4
        $ C.defSettings
