{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

module Work where

import Cassandra
import Data.Conduit
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List qualified as C
import Data.Id
import Imports
import System.Logger (Logger)
import System.Logger qualified as Log
import UnliftIO.Async (pooledMapConcurrentlyN)
import Wire.API.User

deriving instance Cql Name

runCommand :: Logger -> ClientState -> ClientState -> IO ()
runCommand l brig galley =
  runConduit $
    zipSources
      (C.sourceList [(1 :: Int32) ..])
      (transPipe (runClient galley) getUsers)
      .| C.mapM
        ( \(i, p) ->
            Log.info l (Log.field "convs" (show (i * pageSize)))
              >> pure p
        )
      .| C.mapM (runClient galley . pooledMapConcurrentlyN 10 resolveBot)
      .| C.concat
      .| C.catMaybes
      .| C.chunksOf 50
      .| C.mapM_ (runClient brig . writeBots)

pageSize :: Int32
pageSize = 1000

----------------------------------------------------------------------------
-- Queries

-- | Get users from Galley
getUsers :: ConduitM () [(Maybe ProviderId, Maybe ServiceId, BotId, ConvId)] Client ()
getUsers = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () (Maybe ProviderId, Maybe ServiceId, BotId, ConvId)
    cql = "SELECT provider, service, user, conv FROM member"

-- | Get bot info from Galley
resolveBot ::
  (Maybe ProviderId, Maybe ServiceId, BotId, ConvId) ->
  Client (Maybe (ProviderId, ServiceId, BotId, ConvId, Maybe TeamId))
resolveBot (Just pid, Just sid, bid, cid) = do
  tid <- retry x5 $ query1 teamSelect (params LocalQuorum (Identity cid))
  pure (Just (pid, sid, bid, cid, runIdentity =<< tid))
  where
    teamSelect :: PrepQuery R (Identity ConvId) (Identity (Maybe TeamId))
    teamSelect = "SELECT team FROM conversation WHERE conv = ?"
resolveBot _ = pure Nothing

-- | Write users to Brig
writeBots ::
  [(ProviderId, ServiceId, BotId, ConvId, Maybe TeamId)] ->
  Client ()
writeBots [] = pure ()
writeBots xs = retry x5 . batch $ do
  setConsistency LocalQuorum
  setType BatchLogged
  forM_ xs $ \(pid, sid, bid, cid, mbTid) -> do
    addPrepQuery writeUser (pid, sid, bid, cid, mbTid)
    forM_ mbTid $ \tid ->
      addPrepQuery writeTeam (pid, sid, bid, cid, tid)
  where
    writeUser :: PrepQuery W (ProviderId, ServiceId, BotId, ConvId, Maybe TeamId) ()
    writeUser = "INSERT INTO service_user (provider, service, user, conv, team) VALUES(?,?,?,?,?)"
    writeTeam :: PrepQuery W (ProviderId, ServiceId, BotId, ConvId, TeamId) ()
    writeTeam = "INSERT INTO service_team (provider, service, user, conv, team) VALUES(?,?,?,?,?)"
