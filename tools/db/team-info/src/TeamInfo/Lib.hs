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

module TeamInfo.Lib where

import Cassandra as C
import Cassandra.Settings as C
import Data.Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.List as CL
import Data.Id (TeamId, UserId)
import Data.Time
import qualified Database.CQL.Protocol as CQL
import Imports
import Options.Applicative
import qualified System.Logger as Log
import TeamInfo.Types

lookupClientsLastActiveTimestamps :: ClientState -> UserId -> IO [Maybe UTCTime]
lookupClientsLastActiveTimestamps client u = do
  runClient client $ runIdentity <$$> retry x1 (query selectClients (params One (Identity u)))
  where
    selectClients :: PrepQuery R (Identity UserId) (Identity (Maybe UTCTime))
    selectClients = "SELECT last_active from clients where user = ?"

selectTeamMembers :: ClientState -> TeamId -> ConduitM () [TeamMemberRow] IO ()
selectTeamMembers client teamId =
  transPipe (runClient client) (paginateC cql (paramsP One (Identity teamId) 1000) x5)
    .| Conduit.map (fmap CQL.asRecord)
  where
    cql :: C.PrepQuery C.R (Identity TeamId) (CQL.TupleType TeamMemberRow)
    cql =
      "SELECT user, legalhold_status FROM team_member WHERE team = ?"

lookUpActivity :: ClientState -> TeamMemberRow -> IO TeamMember
lookUpActivity brigClient tmr = do
  lastActiveTimestamps <- catMaybes <$> lookupClientsLastActiveTimestamps brigClient tmr.id
  if null lastActiveTimestamps
    then do
      pure $ TeamMember tmr.id tmr.legalhold Nothing
    else do
      let lastActive = maximum lastActiveTimestamps
      pure $ TeamMember tmr.id tmr.legalhold (Just lastActive)

process :: TeamId -> ClientState -> ClientState -> IO [TeamMember]
process teamId brigClient galleyClient =
  runConduit
    $ selectTeamMembers galleyClient teamId
    .| Conduit.concat
    .| Conduit.mapM (lookUpActivity brigClient)
    .| CL.consume

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optsParser) desc)
  logger <- initLogger
  brigClient <- initCas opts.brigDb logger
  galleyClient <- initCas opts.galleyDb logger
  teamMembers <- process opts.teamId brigClient galleyClient
  for_ teamMembers $ \tm -> Log.info logger $ Log.msg (show tm)
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
    desc = header "team-info" <> progDesc "get team info" <> fullDesc
