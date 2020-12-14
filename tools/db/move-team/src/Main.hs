{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Main
  ( main,
    debugMainDebugExportFull,
    debugMainImport,
    debugMainExport,
  )
where

import Cassandra as C
import Cassandra.Settings as C
import Control.Lens hiding ((.=))
import Data.Id (Id (Id))
import Data.UUID
import Imports
import Options as O
import Options.Applicative
import System.Environment (withArgs)
import qualified System.Logger as Log
import System.Process (system)
import Types
import Work

main :: IO ()
main = do
  s <- execParser (info (helper <*> settingsParser) desc)
  lgr <- initLogger
  case s of
    Export teamid targetPath connSettings -> do
      bc <- initCas (connSettings ^. setCasBrig) lgr
      gc <- initCas (connSettings ^. setCasGalley) lgr
      sc <- initCas (connSettings ^. setCasSpar) lgr
      gunC <- initCas (connSettings ^. setCasGundeck) lgr
      runExport $ Env lgr bc gc sc gunC targetPath (Id teamid) 100
    Import sourcePath connSettings -> do
      bc <- initCas (connSettings ^. setCasBrig) lgr
      gc <- initCas (connSettings ^. setCasGalley) lgr
      sc <- initCas (connSettings ^. setCasSpar) lgr
      gunC <- initCas (connSettings ^. setCasGundeck) lgr
      let dummyTeamId :: UUID = fromJust $ Data.UUID.fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c"
      runImport $ Env lgr bc gc sc gunC sourcePath (Id dummyTeamId) 100
    DebugExportFull targetPath connSettings -> do
      bc <- initCas (connSettings ^. setCasBrig) lgr
      gc <- initCas (connSettings ^. setCasGalley) lgr
      sc <- initCas (connSettings ^. setCasSpar) lgr
      gunC <- initCas (connSettings ^. setCasGundeck) lgr
      let dummyTeamId :: UUID = fromJust $ Data.UUID.fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c"
      runDebugExportFull $ Env lgr bc gc sc gunC targetPath (Id dummyTeamId) 100
  where
    desc =
      header "service-backfill"
        <> progDesc "Backfill service tables"
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
        . C.setContacts (cas ^. cHosts) []
        . C.setPortNumber (fromIntegral $ cas ^. cPort)
        . C.setKeyspace (cas ^. cKeyspace)
        . C.setProtocolVersion C.V4
        $ C.defSettings

debugMainDebugExportFull :: IO ()
debugMainDebugExportFull = do
  let dir = "/tmp/full-backup"
  void $ system $ "rm -rf " <> dir
  void $ system $ "mkdir -p " <> dir
  withArgs
    [ "debug-export-full",
      "--target-path",
      dir
    ]
    main

debugMainImport :: IO ()
debugMainImport = do
  let dir = "/tmp/full-backup"
  withArgs
    [ "import",
      "--source-path",
      dir
    ]
    main

debugMainExport :: IO ()
debugMainExport = do
  let dir = "/tmp/move-team-export"
  void $ system $ "rm -rf " <> dir
  void $ system $ "mkdir -p " <> dir
  withArgs
    [ "export",
      "--teamid",
      "e09f7a63-b5d4-4db4-a3c1-18bddf3df7fc",
      "--target-path",
      dir
    ]
    main
