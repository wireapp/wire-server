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
  )
where

import Cassandra as C
import Cassandra.Settings as C
import Control.Lens hiding ((.=))
import Data.Id (Id (Id))
import Imports
import Options as O
import Options.Applicative
import qualified System.Logger as Log
import Types
import Work

-- ./dist/move-team --sink=/tmp/x.json --teamid=1595e6e0-11eb-48bb-afa2-c10207a78889

main :: IO ()
main = do
  s <- execParser (info (helper <*> settingsParser) desc)
  lgr <- initLogger
  bc <- initCas (s ^. setCasBrig) lgr
  gc <- initCas (s ^. setCasGalley) lgr
  sc <- initCas (s ^. setCasSpar) lgr
  runCommand $ Env lgr bc gc sc (s ^. setTargetPath) (Id $ s ^. setTeamId) 100
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
