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
import Data.Text as Text
import Database.Bloodhound qualified as ES
import Imports
import Network.HTTP.Client qualified as HTTP
import Options as O
import Options.Applicative
import System.Logger qualified as Log
import Work

main :: IO ()
main = do
  s <- execParser (info (helper <*> settingsParser) desc)
  lgr <- initLogger
  cas <- initCas (setCasBrig s) lgr
  mgr <- HTTP.newManager HTTP.defaultManagerSettings
  let es = initES (setESBrig s) mgr
  runCommand lgr cas es (esIndex $ setESBrig s)
  where
    desc =
      header "find-undead"
        <> progDesc "finds users which are in ES but not in cassandra"
        <> fullDesc
    initLogger =
      Log.new
        . Log.setOutput Log.StdOut
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
    initES es = ES.mkBHEnv (ES.Server . Text.pack $ "http://" <> esHost es <> ":" <> show (esPort es))
