{-# OPTIONS_GHC -Wno-orphans #-}

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
import Data.Time.Clock
import Imports
import System.Logger (Logger)
import System.Logger qualified as Log

runCommand :: Bool -> Logger -> ClientState -> IO ()
runCommand dryRun l brig =
  runConduit $
    zipSources
      (C.sourceList [(1 :: Int32) ..])
      (transPipe (runClient brig) getClients)
      .| C.mapM
        ( \(i, rows) -> do
            Log.info l (Log.field "number of clients processed: " (show (i * pageSize)))
            pure rows
        )
      .| C.mapM_ (\rows -> runClient brig (mapM_ (filterReportRemove dryRun l) rows))

pageSize :: Int32
pageSize = 1000

type ClientRow =
  ( UserId, -- user
    Text, -- client
    Maybe (Cassandra.Set Int32), -- capabilities
    Maybe Int32, -- class
    Maybe Text, -- cookie
    Maybe Text, -- label
    Maybe UTCTime, -- last_active
    Maybe Text, -- model
    Maybe UTCTime, -- tstamp
    Maybe Int32 -- type
  )

getClients :: ConduitM () [ClientRow] Client ()
getClients = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () ClientRow
    cql = "select user, client, capabilities, class, cookie, label, last_active, model, tstamp, type from clients"

filterReportRemove :: Bool -> Logger -> ClientRow -> Client ()
filterReportRemove dryRun l row@(user, client, Nothing, Nothing, Nothing, Nothing, Just _lastActive, Nothing, Nothing, Nothing) = do
  Log.info l (Log.msg $ "*** bad row in brig.clients: " <> show row)
  if dryRun
    then do
      Log.info l (Log.msg $ "would run: " <> rmqs)
    else do
      Log.info l (Log.msg $ "running: " <> rmqs)
      rm user client
      Log.info l (Log.msg @Text "removed!")
  where
    rm :: (MonadClient m) => UserId -> Text -> m ()
    rm uid cid =
      retry x5 $ write rmq (params LocalQuorum (uid, cid))

    rmq :: PrepQuery W (UserId, Text) ()
    rmq = fromString rmqs

    rmqs :: String
    rmqs = "delete from clients where user = ? and client = ?"
filterReportRemove _ _ _ = pure ()
