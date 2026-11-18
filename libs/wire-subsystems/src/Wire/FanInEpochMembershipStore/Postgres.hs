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

-- | Uses the dedicated notification pool. A durable per-group row serializes
-- replacement, including first writes and empty memberships. Validation happens
-- under that lock; no membership/history rows change on stale/conflicting input.
module Wire.FanInEpochMembershipStore.Postgres (interpretFanInEpochMembershipStoreToPostgres) where

import Data.Set qualified as Set
import Data.Vector qualified as V
import Hasql.Pool (UsageError)
import Hasql.Pool.Extended qualified as PG
import Hasql.Session qualified as Session
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as Tx
import Imports
import Polysemy
import Polysemy.Error
import Wire.FanInEpochMembershipStore
import Wire.FanInEpochMembershipStore.Postgres.Queries
import Wire.Postgres (useWithResetAndRetry)

interpretFanInEpochMembershipStoreToPostgres :: forall r. (Member (Embed IO) r, Member (Error UsageError) r, Member (Error FanInEpochMembershipStoreError) r) => PG.Pool -> InterpreterFor FanInEpochMembershipStore r
interpretFanInEpochMembershipStoreToPostgres pool = interpret $ \case
  GetFanInEpochMembers gid epoch -> V.toList <$> execute (Session.statement (gid, epoch) selectMembers)
  GetFanInEpochsForClient uid cid -> V.toList <$> execute (Session.statement (uid, cid) selectEpochsForClient)
  ReplaceFanInCurrentEpochMembers gid epoch supersededAt members -> do
    result <- execute $ Tx.transaction Tx.ReadCommitted Tx.Write $ do
      first <- Tx.statement (gid, epoch) insertHead
      current <- Tx.statement gid lockHead
      if not first && epoch < current
        then pure (Left FanInStaleEpoch)
        else
          if not first && epoch == current
            then do
              existing <- V.toList <$> Tx.statement (gid, current) selectCurrentMembers
              pure $ if Set.fromList existing == Set.fromList members then Right () else Left FanInConflictingEpoch
            else do
              Tx.statement (gid, supersededAt) archiveMembers
              Tx.statement gid deleteMembers
              Tx.statement (gid, epoch, V.fromList (Set.toList (Set.fromList members))) insertMembers
              Tx.statement (gid, epoch) updateHead
              pure (Right ())
    either throw pure result
  where
    execute :: Session.Session a -> Sem r a
    execute session = liftIO (useWithResetAndRetry pool session) >>= either throw pure
