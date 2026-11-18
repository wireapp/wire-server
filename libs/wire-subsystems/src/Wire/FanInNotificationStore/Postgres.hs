{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- | Postgres interpreter for fan-in notification persistence. Pass the dedicated
-- notification database pool explicitly; this never uses the ambient application
-- database pool. The pool is owned by the caller and is not closed here.
module Wire.FanInNotificationStore.Postgres
  ( interpretFanInNotificationStoreToPostgres,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Range (Range, fromRange)
import Data.Vector qualified as V
import Hasql.Pool (UsageError)
import Hasql.Pool.Extended qualified as PG
import Hasql.Session qualified as Session
import Hasql.Statement (Statement)
import Imports
import Polysemy
import Polysemy.Error (Error, throw)
import Wire.API.Notification (NotificationId)
import Wire.FanInNotificationStore
import Wire.FanInNotificationStore.Postgres.Queries
import Wire.Postgres (useWithResetAndRetry)

interpretFanInNotificationStoreToPostgres :: (Member (Embed IO) r, Member (Error UsageError) r, Member (Error FanInNotificationStoreError) r) => PG.Pool -> InterpreterFor FanInNotificationStore r
interpretFanInNotificationStoreToPostgres pool = interpret $ \case
  AppendFanInUserNotifications rows -> appendNotifications pool userStatements rows
  GetFanInUserNotificationPage uid cursor pageSize -> readPage pool userStatements uid cursor pageSize
  GetLastFanInUserNotificationId uid -> execute pool uid userStatements.selectLast
  HasFanInUserNotification uid nid -> execute pool (uid, nid) userStatements.notificationExists
  AppendFanInClientNotifications rows -> appendNotifications pool clientStatements rows
  GetFanInClientNotificationPage uid cid cursor pageSize -> readPage pool clientStatements (uid, cid) cursor pageSize
  GetLastFanInClientNotificationId uid cid -> execute pool (uid, cid) clientStatements.selectLast
  HasFanInClientNotification uid cid nid -> execute pool ((uid, cid), nid) clientStatements.notificationExists
  AppendFanInTeamNotifications rows -> appendNotifications pool teamStatements rows
  GetFanInTeamNotificationPage tid cursor pageSize -> readPage pool teamStatements tid cursor pageSize
  GetLastFanInTeamNotificationId tid -> execute pool tid teamStatements.selectLast
  HasFanInTeamNotification tid nid -> execute pool (tid, nid) teamStatements.notificationExists
  AppendFanInEpochNotifications rows -> appendNotifications pool epochStatements rows
  GetFanInEpochNotificationPage gid ep cursor pageSize -> readPage pool epochStatements (gid, ep) cursor pageSize
  GetLastFanInEpochNotificationId gid ep -> execute pool (gid, ep) epochStatements.selectLast
  HasFanInEpochNotification gid ep nid -> execute pool ((gid, ep), nid) epochStatements.notificationExists
  AppendFanInLocalConnectionNotifications rows -> appendNotifications pool localConnectionStatements rows
  GetFanInLocalConnectionNotificationPage uid cursor pageSize -> readPage pool localConnectionStatements uid cursor pageSize
  GetLastFanInLocalConnectionNotificationId uid -> execute pool uid localConnectionStatements.selectLast
  HasFanInLocalConnectionNotification uid nid -> execute pool (uid, nid) localConnectionStatements.notificationExists
  AppendFanInRemoteConnectionNotifications rows -> appendNotifications pool remoteConnectionStatements rows
  GetFanInRemoteConnectionNotificationPage domain uid cursor pageSize -> readPage pool remoteConnectionStatements (domain, uid) cursor pageSize
  GetLastFanInRemoteConnectionNotificationId domain uid -> execute pool (domain, uid) remoteConnectionStatements.selectLast
  HasFanInRemoteConnectionNotification domain uid nid -> execute pool ((domain, uid), nid) remoteConnectionStatements.notificationExists

execute :: (Member (Embed IO) r, Member (Error UsageError) r) => PG.Pool -> input -> Statement input output -> Sem r output
execute pool params stmt = liftIO (useWithResetAndRetry pool (Session.statement params stmt)) >>= either throw pure

-- | Validate the whole batch and derive its partition timestamps before any
-- database access. Identical ids always target the same primary key. A failed
-- append also rolls back its cursor update.
appendNotifications :: (Member (Embed IO) r, Member (Error UsageError) r, Member (Error FanInNotificationStoreError) r) => PG.Pool -> NotificationStatements key -> NonEmpty (key, FanInNotification) -> Sem r ()
appendNotifications pool statements rows = do
  prepared <- for (NE.toList rows) $ \(key, notification) -> do
    createdAt <- either throw pure (fanInNotificationCreatedAt notification)
    pure (key, notification, createdAt)
  execute pool (V.fromList prepared) statements.insertNotifications

readPage :: (Member (Embed IO) r, Member (Error UsageError) r) => PG.Pool -> NotificationStatements key -> key -> Maybe NotificationId -> Range 1 10000 Int32 -> Sem r FanInNotificationPage
readPage pool statements key cursor pageSize = do
  let size = fromRange pageSize
  rows <- execute pool (key, cursor, size + 1) statements.selectPage
  pure $ FanInNotificationPage (V.toList (V.take (fromIntegral size) rows)) (V.length rows > fromIntegral size)
