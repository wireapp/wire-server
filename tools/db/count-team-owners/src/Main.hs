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
import Imports
import Options as O
import Options.Applicative
import qualified System.Logger as Log
import Work

main :: IO ()
main = do
  s <- execParser (info (helper <*> settingsParser) desc)
  lgr <- initLogger
  gc <- initCas (setCasGalley s) lgr
  runCommand lgr gc
  where
    desc =
      header "count-team-owners"
        <> progDesc "Backfill billing_team_member table"
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
        . C.setContacts (cHosts cas) []
        . C.setPortNumber (fromIntegral $ cPort cas)
        . C.setKeyspace (cKeyspace cas)
        . C.setProtocolVersion C.V4
        $ C.defSettings
