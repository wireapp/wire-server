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
import Control.Monad.Trans.Maybe
import Data.Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.List as CL
import Data.Id
import Data.Map as Map
import qualified Database.CQL.Protocol as CQL
import Imports
import Options.Applicative
import qualified System.Logger as Log
import Wire.API.User (EmailAddress)
import Wire.API.Routes.Internal.Galley.TeamsIntra

selectServices :: ClientState -> ConduitM () [ServiceProviderRow] IO ()
selectServices client =
  transPipe (runClient client) (paginateC cql (paramsP One () 1000) x5)
    .| Conduit.map (fmap CQL.asRecord)
  where
    cql :: C.PrepQuery C.R () (CQL.TupleType ServiceProviderRow)
    cql =
      "SELECT team, service, provider FROM service_whitelist"

isTeamActive :: ClientState -> ServiceProviderRow -> IO Bool
isTeamActive client spr = do
  mStatus <- (runIdentity =<<) <$> runClient client (retry x1 (query1 cql (params One (Identity spr.teamId))))
  pure $ mStatus == Just Active
  where
    cql :: PrepQuery R (Identity TeamId) (Identity (Maybe TeamStatus))
    cql = "SELECT status FROM team WHERE team = ?"

lookupService :: ClientState -> ProviderId -> ServiceId -> IO (Maybe ServiceRow)
lookupService client providerId serviceId = do
  fmap CQL.asRecord <$$> runClient client $ retry x1 (query1 cql (params One (providerId, serviceId)))
  where
    cql :: PrepQuery R (ProviderId, ServiceId) (CQL.TupleType ServiceRow)
    cql = "select base_url, enabled from service where provider = ? AND id = ?"

lookupTeamOwnerEmail :: Log.Logger -> MVar (Map TeamId (Maybe EmailAddress)) -> ClientState -> ClientState -> TeamId -> IO (Maybe EmailAddress)
lookupTeamOwnerEmail logger cache brig galley teamId = do
  emailCache <- readMVar cache
  maybe fromDb pure $ Map.lookup teamId emailCache
  where
    fromDb :: IO (Maybe EmailAddress)
    fromDb = do
      Log.info logger $ Log.msg ("New team: " <> show teamId)
      mFromDb <- lookupEmailInDb brig galley teamId
      modifyMVar_ cache (pure . Map.insert teamId mFromDb)
      pure mFromDb

lookupEmailInDb :: ClientState -> ClientState -> TeamId -> IO (Maybe EmailAddress)
lookupEmailInDb brig galley team = runMaybeT $ do
  uid <- MaybeT ((runIdentity =<<) <$> runClient galley (retry x1 (query1 selectCreatorFromTeam (params One (Identity team)))))
  MaybeT ((runIdentity =<<) <$> runClient brig (retry x1 (query1 selectEmailFromUser (params One (Identity uid)))))
  where
    selectCreatorFromTeam :: C.PrepQuery C.R (Identity TeamId) (Identity (Maybe UserId))
    selectCreatorFromTeam = "SELECT creator FROM team WHERE team = ?"

    selectEmailFromUser :: C.PrepQuery C.R (Identity UserId) (Identity (Maybe EmailAddress))
    selectEmailFromUser = "SELECT email FROM user WHERE id = ?"

process :: Log.Logger -> MVar (Map TeamId (Maybe EmailAddress)) -> ClientState -> ClientState -> IO [String]
process logger cache brigClient galleyClient =
  runConduit
    $ selectServices brigClient
    .| Conduit.concat
    .| Conduit.filterM (isTeamActive galleyClient)
    .| Conduit.mapM
      ( \row -> do
          toBotInfo row
            <$> lookupService galleyClient (row.providerId) (row.serviceId)
            <*> lookupTeamOwnerEmail logger cache brigClient galleyClient row.teamId
      )
    .| Conduit.map toCsv
    .| CL.consume

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optsParser) desc)
  logger <- initLogger
  brigClient <- initCas opts.brigDb logger
  galleyClient <- initCas opts.galleyDb logger
  cache <- newMVar Map.empty
  csvLines <- process logger cache brigClient galleyClient
  let csv = unlines csvLines
  putStrLn "team,email,service,provider,host,enabled"
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
