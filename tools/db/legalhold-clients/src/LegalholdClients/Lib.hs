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

module LegalholdClients.Lib where

import Cassandra as C
import Cassandra.Settings as C
import Data.Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.List as CL
import Data.Id
import qualified Database.CQL.Protocol as CQL
import Imports
import LegalholdClients.Types
import Options.Applicative
import qualified System.Logger as Log
import Wire.API.User.Client (ClientType (..))

selectClients :: ClientState -> ConduitM () [ClientRow] IO ()
selectClients client =
  transPipe (runClient client) (paginateC cql (paramsP One () 1000) x5)
    .| Conduit.map (fmap CQL.asRecord)
  where
    cql :: C.PrepQuery C.R () (CQL.TupleType ClientRow)
    cql =
      "SELECT client, user, type, tstamp, last_active FROM clients"

addTeam :: ClientState -> ClientRow -> IO ClientInfo
addTeam client cr = do
  ClientInfo cr <$> runClient client ((runIdentity =<<) <$> retry x1 (query1 cql (params One (Identity cr.user))))
  where
    cql :: C.PrepQuery C.R (Identity UserId) (Identity (Maybe TeamId))
    cql =
      "SELECT team FROM user WHERE id = ?"

process :: ClientState -> IO [ClientInfo]
process brigClient =
  runConduit
    $ selectClients brigClient
    .| Conduit.concat
    .| CL.filter (\cr -> cr.clientType == LegalHoldClientType)
    .| CL.mapM (addTeam brigClient)
    .| CL.consume

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optsParser) desc)
  logger <- initLogger
  brigClient <- initCas opts.brigDb logger
  clients <- process brigClient
  let csv = fmap toCsvRow clients
  putStrLn "team,client_id,timestamp,last_active"
  putStrLn $ unlines csv
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
    desc = header "legalhold-clients" <> progDesc "get team info" <> fullDesc
