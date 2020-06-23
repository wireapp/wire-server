{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

module Work
  ( runCommand,
  )
where

import Brig.Types hiding (Client)
import Cassandra
import Data.Id
import Data.List.Extra (nubOrd)
import Imports
import System.Logger (Logger)
import qualified System.Logger as Log
import UnliftIO.Async (pooledMapConcurrentlyN_)

deriving instance Cql Name

runCommand :: Logger -> ClientState -> IO ()
runCommand l brig = runClient brig $ do
  services <- getServices
  existing <- filterM doesServiceExist (nubOrd services)
  pooledMapConcurrentlyN_ 20 (whitelistService l) existing

----------------------------------------------------------------------------
-- Queries

-- | Get all services in team conversations
getServices :: Client [(ProviderId, ServiceId, TeamId)]
getServices = retry x5 $ query cql (params Quorum ())
  where
    cql :: PrepQuery R () (ProviderId, ServiceId, TeamId)
    cql = "SELECT provider, service, team FROM service_team"

-- | Check if a service exists
doesServiceExist :: (ProviderId, ServiceId, a) -> Client Bool
doesServiceExist (pid, sid, _) =
  retry x5 $ fmap isJust $ query1 cql (params Quorum (pid, sid))
  where
    cql :: PrepQuery R (ProviderId, ServiceId) (Identity ServiceId)
    cql =
      "SELECT id FROM service \
      \WHERE provider = ? AND id = ?"

-- | Add the service to the whitelist
whitelistService :: Logger -> (ProviderId, ServiceId, TeamId) -> Client ()
whitelistService l (pid, sid, tid) = do
  Log.info l $
    Log.msg (Log.val "Whitelisting")
      . Log.field "provider" (show pid)
      . Log.field "service" (show sid)
      . Log.field "team" (show tid)
  retry x5 . batch $ do
    setConsistency Quorum
    setType BatchLogged
    addPrepQuery insert1 (tid, pid, sid)
    addPrepQuery insert1Rev (tid, pid, sid)
  where
    insert1 :: PrepQuery W (TeamId, ProviderId, ServiceId) ()
    insert1 =
      "INSERT INTO service_whitelist \
      \(team, provider, service) \
      \VALUES (?, ?, ?)"
    insert1Rev :: PrepQuery W (TeamId, ProviderId, ServiceId) ()
    insert1Rev =
      "INSERT INTO service_whitelist_rev \
      \(team, provider, service) \
      \VALUES (?, ?, ?)"
