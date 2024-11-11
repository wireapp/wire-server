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

module BotInfo.Lib where

import BotInfo.Types
import Cassandra as C
import Cassandra.Settings as C
import Data.Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.List as CL
import Data.Id
import qualified Database.CQL.Protocol as CQL
import Imports
import Options.Applicative
import qualified System.Logger as Log

selectServices :: ClientState -> ConduitM () [ServiceProviderRow] IO ()
selectServices client =
  transPipe (runClient client) (paginateC cql (paramsP One () 1000) x5)
    .| Conduit.map (fmap CQL.asRecord)
  where
    cql :: C.PrepQuery C.R () (CQL.TupleType ServiceProviderRow)
    cql =
      "SELECT team, service, provider FROM service_whitelist"

lookupService :: ClientState -> ProviderId -> ServiceId -> IO (Maybe ServiceRow)
lookupService client providerId serviceId = do
  fmap CQL.asRecord <$$> runClient client $ retry x1 (query1 cql (params One (providerId, serviceId)))
  where
    cql :: PrepQuery R (ProviderId, ServiceId) (CQL.TupleType ServiceRow)
    cql = "select base_url, enabled from service where provider = ? AND id = ?"

process :: ClientState -> ClientState -> IO [String]
process brigClient galleyClient =
  runConduit $
    selectServices brigClient
      .| Conduit.concat
      .| Conduit.mapM (\row -> toBotInfo row <$> lookupService galleyClient (row.providerId) (row.serviceId))
      .| Conduit.map toCsv
      .| CL.consume

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optsParser) desc)
  logger <- initLogger
  brigClient <- initCas opts.brigDb logger
  galleyClient <- initCas opts.galleyDb logger
  csvLines <- process brigClient galleyClient
  let csv = unlines csvLines
  putStrLn "team,service,provider,base_url,enabled"
  putStrLn csv
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
    desc = header "bot-info" <> progDesc "get bot info" <> fullDesc
