{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module Bots.Lib where

import Bots.Types
import Cassandra as C
import Cassandra.Settings as C
import Data.Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.List as CL
import qualified Database.CQL.Protocol as CQL
import Imports
import Options.Applicative
import qualified System.Logger as Log
import System.Logger.Message ((.=))

scanServiceTeam :: ClientState -> ConduitM () [ServiceRow] IO ()
scanServiceTeam client =
  transPipe (runClient client) (paginateC selectAll (paramsP One () 1000) x5)
    .| Conduit.map (fmap CQL.asRecord)
  where
    selectAll :: C.PrepQuery C.R () (CQL.TupleType ServiceRow)
    selectAll =
      "SELECT provider, service, team FROM service_team"

process :: Log.Logger -> Maybe Int -> ClientState -> IO TeamsWithService
process logger limit brigClient =
  runConduit $
    scanServiceTeam brigClient
      .| Conduit.concat
      .| (maybe (Conduit.filter (const True)) Conduit.take limit)
      .| forever (CL.isolate 5000 .| (Conduit.foldMap toTeamsWithService >>= yield))
      .| Conduit.takeWhile ((> 0) . entriesSearched)
      .| CL.scan (<>) mempty
        `fuseUpstream` Conduit.mapM_ (\r -> Log.info logger $ "entries_searched" .= show r.entriesSearched)

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optsParser) desc)
  logger <- initLogger
  brigClient <- initCas opts.brigDb logger
  putStrLn "scanning users table..."
  res <- process logger opts.limit brigClient
  Log.info logger $ "total_entries" .= show res.entriesSearched
  Log.info logger $ "teams_with_service" .= show (length res.teams)
  where
    initLogger =
      Log.new
        . Log.setLogLevel Log.Info
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        $ Log.defSettings
    initCas settings l =
      C.init
        . C.setLogger (C.mkLogger l)
        . C.setContacts settings.host []
        . C.setPortNumber (fromIntegral settings.port)
        . C.setKeyspace settings.keyspace
        . C.setProtocolVersion C.V4
        $ C.defSettings
    desc = header "bots" <> progDesc "This program retrieves information about bots" <> fullDesc
