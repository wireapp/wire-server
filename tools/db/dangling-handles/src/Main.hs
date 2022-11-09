{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
import qualified DanglingHandles
import qualified HandleLessUsers
import Imports
import Options as O
import Options.Applicative
import qualified System.Logger as Log
import System.Logger.Extended (structuredJSONRenderer)

main :: IO ()
main = do
  (cmd, s) <- execParser (info (helper <*> optionsParser) desc)
  lgr <- initLogger
  brig <- initCas (setCasBrig s) (Log.clone (Just "cassandra-brig") lgr)
  let workLogger = Log.clone (Just "work") lgr
      outputFile = setIncosistenciesFile s
  case cmd of
    DanglingHandles Nothing ->
      DanglingHandles.runCommand workLogger brig outputFile
    DanglingHandles (Just handlesFile) ->
      DanglingHandles.examineHandles workLogger brig handlesFile outputFile
    HandleLessUsers ->
      HandleLessUsers.runCommand workLogger brig outputFile
  Log.info lgr $ Log.msg (Log.val "Done scanning, sleeping for 4 hours so logs can be extracted") . Log.field "file" (setIncosistenciesFile s)
  threadDelay (4 * 60 * 60 * 1_000_000)
  Log.info lgr $ Log.msg (Log.val "Sleep compelete, logs will not be accessible anymore if this was running in a container!")
  where
    desc =
      header "db-inconsistencies"
        <> progDesc "finds inconsistencies in the DB"
        <> fullDesc
    initLogger =
      Log.new
        . Log.setOutput Log.StdOut
        . Log.setBufSize 0
        . Log.setRenderer structuredJSONRenderer
        $ Log.defSettings
    initCas cas l =
      C.init
        . C.setLogger (C.mkLogger l)
        . C.setContacts (cHosts cas) []
        . C.setPortNumber (fromIntegral $ cPort cas)
        . C.setKeyspace (cKeyspace cas)
        . C.setProtocolVersion C.V4
        $ C.defSettings
