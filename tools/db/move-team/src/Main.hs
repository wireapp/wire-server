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
    debugMainExport,
    debugMainImport,
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

-- ./dist/move-team --sink=/tmp/x.json --teamid=1595e6e0-11eb-48bb-afa2-c10207a78889

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

debugMainExport :: IO ()
debugMainExport = do
  let dir = "/tmp/full-backup"
  void $ system $ "rm -rf " <> dir
  withArgs
    [ "export",
      "--teamid",
      "e8cd3353-3c4c-4ced-807b-3a7a571cb6cf",
      "--target-path",
      dir
    ]
    main

debugMainImport :: IO ()
debugMainImport = do
  let dir = "/home/stefan/full-backup"
  withArgs
    [ "import",
      "--source-path",
      dir
    ]
    main
