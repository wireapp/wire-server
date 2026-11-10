-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program. If not, see <https://www.gnu.org/licenses/>.

-- | Reconciliation of the stored @mls_group_member_client@ and
-- @mls_history_client@ rows against the authoritative ratchet tree stored in
-- the group's GroupInfo.
--
-- Ghost clients (rows lost to the external-commit re-join race fixed in
-- WPB-29648) manifest as tree leaves that have no corresponding row. This
-- module computes the minimal set of writes that brings the rows back in sync
-- with the tree.
module Wire.ConversationStore.MLS.Reconcile
  ( MlsReconcileOps (..),
    mlsReconcileOps,
  )
where

import Data.Domain (Domain)
import Data.Id (ClientId, HistoryClientId, UserId)
import Data.IntMap qualified as IntMap
import Data.Map qualified as Map
import Imports
import Wire.API.MLS.Credential (ClientIdentity (..), GroupMember (..))
import Wire.ConversationStore.MLS.Types (IndexMap, unIndexMap)

-- | The writes needed to bring the member-client rows in line with the
-- ratchet-tree leaves.
data MlsReconcileOps = MlsReconcileOps
  { -- | leaf, removal_pending = false
    roMemberInsert :: [(Domain, UserId, ClientId, Int32)],
    roMemberUpdateLeaf :: [(Domain, UserId, ClientId, Int32)],
    roMemberDelete :: [(Domain, UserId, ClientId)],
    roHistoryInsert :: [(HistoryClientId, Int32)],
    roHistoryUpdateLeaf :: [(HistoryClientId, Int32)],
    roHistoryDelete :: [HistoryClientId]
  }
  deriving stock (Eq, Show)

-- | Compute the diff between the current rows and the tree leaves.
--
-- Rules:
--
--   * Rows with @removal_pending == True@ are ignored entirely: their leaf is
--     still occupied in the clients' trees and pending remove proposals must
--     stay resolvable.
--   * A tree leaf with no row is inserted (pending = false).
--   * A row whose leaf differs from the tree leaf is updated.
--   * A non-pending row whose client is absent from the tree is deleted (the
--     row-write of a previous commit was lost).
mlsReconcileOps ::
  -- | Current @mls_group_member_client@ rows
  [(Domain, UserId, ClientId, Int32, Bool)] ->
  -- | Current @mls_history_client@ rows
  [(HistoryClientId, Int32, Bool)] ->
  -- | Tree leaves (as extracted from the stored GroupInfo)
  IndexMap ->
  MlsReconcileOps
mlsReconcileOps members history leaves =
  let (mIns, mUpd, mDel) =
        diffRows
          ( Map.fromList
              [ ((cid.ciDomain, cid.ciUser, cid.ciClient), fromIntegral i)
              | (i, RegularClient cid) <- IntMap.assocs treeMembers
              ]
          )
          (Map.fromList [((d, u, c), (leafIdx, pending)) | (d, u, c, leafIdx, pending) <- members])
      (hIns, hUpd, hDel) =
        diffRows
          (Map.fromList [(h, fromIntegral i) | (i, HistoryClient h) <- IntMap.assocs treeMembers])
          (Map.fromList [(k, (leafIdx, pending)) | (k, leafIdx, pending) <- history])
   in MlsReconcileOps
        { roMemberInsert = [withKey k l | (k, l) <- mIns],
          roMemberUpdateLeaf = [withKey k l | (k, l) <- mUpd],
          roMemberDelete = mDel,
          roHistoryInsert = hIns,
          roHistoryUpdateLeaf = hUpd,
          roHistoryDelete = hDel
        }
  where
    treeMembers = unIndexMap leaves

    withKey :: (Domain, UserId, ClientId) -> Int32 -> (Domain, UserId, ClientId, Int32)
    withKey (dom, usr, cid) l = (dom, usr, cid, l)

    -- The three reconcile rules for one row kind. Pending rows block inserts
    -- (any row, incl. pending) but are never updated or deleted.
    diffRows ::
      (Ord k) =>
      Map k Int32 ->
      Map k (Int32, Bool) ->
      ([(k, Int32)], [(k, Int32)], [k])
    diffRows tree current =
      let active = Map.filter (not . snd) current
          ins = [(k, l) | (k, l) <- Map.toList tree, not (Map.member k current)]
          upd = [(k, l) | (k, l) <- Map.toList tree, Just (rowLeaf, _) <- [Map.lookup k active], rowLeaf /= l]
          del = [k | (k, _) <- Map.toList active, not (Map.member k tree)]
       in (ins, upd, del)
