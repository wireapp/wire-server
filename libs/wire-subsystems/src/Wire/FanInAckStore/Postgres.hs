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

-- | Acknowledgement persistence using the dedicated notification database.
-- The caller owns the pool. Normal advancement and explicit resets each use a
-- single atomic upsert; authorization and stream access belong to the subsystem.
module Wire.FanInAckStore.Postgres (interpretFanInAckStoreToPostgres) where

import Hasql.Pool (UsageError)
import Hasql.Pool.Extended qualified as PG
import Hasql.Session qualified as Session
import Hasql.Statement (Statement)
import Imports
import Polysemy
import Polysemy.Error
import Wire.FanInAckStore
import Wire.FanInAckStore.Postgres.Queries
import Wire.FanInNotificationStore (fanInNotificationIdCreatedAt)
import Wire.Postgres (useWithResetAndRetry)

interpretFanInAckStoreToPostgres :: forall r. (Member (Embed IO) r, Member (Error UsageError) r, Member (Error FanInAckStoreError) r) => PG.Pool -> InterpreterFor FanInAckStore r
interpretFanInAckStoreToPostgres pool = interpret $ \case
  GetFanInAck key -> withAckKey key $ \statements params -> execute params statements.selectAck
  UpsertFanInAck key nid -> do
    validate nid
    withAckKey key $ \statements params -> execute (params, nid) statements.upsertAck
  AdvanceFanInAck key nid -> do
    validate nid
    withAckKey key $ \statements params -> execute (params, nid) statements.advanceAck
  DeleteFanInAck key -> withAckKey key $ \statements params -> execute params statements.deleteAck
  where
    execute :: input -> Statement input output -> Sem r output
    execute params stmt = liftIO (useWithResetAndRetry pool (Session.statement params stmt)) >>= either throw pure
    validate nid = either (const (throw (InvalidFanInAckNotificationId nid))) (const (pure ())) (fanInNotificationIdCreatedAt nid)

-- Keep table selection fixed in Haskell. No table or column names come from
-- input, and every component of the key is passed as a bound SQL parameter.
withAckKey :: FanInAckKey -> (forall key. AckStatements key -> key -> a) -> a
withAckKey key f = case key of
  FanInUserAck uid cid -> f userAckStatements (uid, cid)
  FanInClientAck uid cid -> f clientAckStatements (uid, cid)
  FanInTeamAck uid cid tid -> f teamAckStatements (uid, cid, tid)
  FanInEpochAck uid cid gid epoch -> f epochAckStatements (uid, cid, gid, epoch)
  FanInLocalConnectionAck uid connected -> f localConnectionAckStatements (uid, connected)
  FanInRemoteConnectionAck uid domain connected -> f remoteConnectionAckStatements (uid, domain, connected)
