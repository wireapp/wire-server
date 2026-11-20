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

-- | Heal ghost clients: reconcile the stored @mls_group_member_client@ /
-- @mls_history_client@ rows with the ratchet tree persisted in the group's
-- GroupInfo. Runs before every commit bundle is validated, so a group whose
-- rows were lost (e.g. to the external-commit re-join race fixed in
-- WPB-29648) self-heals instead of rejecting every subsequent commit.
--
-- The GroupInfo is uploaded by the committing client, so its tree is only
-- trusted when its digest matches the one recorded at commit time, which
-- happens only if the tree matched the membership the server validated for
-- that commit ('groupInfoDigest'). The reconcile runs under the per-group
-- commit lock (the request fails with a retryable stale-message error if the
-- lock is held, as commits do) and only when the live epoch and the stored
-- GroupInfo epoch both match the locked epoch, so a concurrent or
-- partially-failed commit cannot be rolled back by a stale tree.
--
-- The commit lock is a lease ('repairTTL'). The reconcile starts no write
-- after 'repairDeadline' (half the lease), and the lock is released only
-- while the lease is still safely held ('leaseStillHeld'); otherwise it is
-- left to expire, so a late repair cannot delete a successor's lock. The
-- remaining exposure is a single write in flight at the deadline, which has
-- the other half of the lease to complete. An incomplete repair fails the
-- request with a retryable error; the writes are idempotent, so the next
-- attempt continues where it stopped.
module Wire.ConversationSubsystem.MLS.Repair
  ( GroupInfoRepairEnabled (..),
    repairMLSMemberClients,
    repairDeadline,
    leaseStillHeld,
    rowsMatchTree,
    validatedGroupInfoDigest,
    storedLeaves,
  )
where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray qualified as BA
import Data.Hex (hex)
import Data.Qualified (Local, tUnqualified)
import Data.Set qualified as Set
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Imports
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)
import Polysemy.Input (Input, input)
import Polysemy.Resource (Resource, finally)
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as TinyLog
import System.Logger qualified as Log
import Wire.API.Conversation.Protocol qualified as Protocol
import Wire.API.MLS.Credential (GroupMember (..))
import Wire.API.MLS.Epoch (Epoch (..), epochNumber)
import Wire.API.MLS.Group (GroupId (..))
import Wire.API.MLS.GroupInfo (GroupContext (..), GroupInfo (..), GroupInfoData (..), GroupInfoTBS (..))
import Wire.API.MLS.LeafNode (LeafIndex)
import Wire.API.MLS.Serialisation (RawMLS (..), decodeMLS')
import Wire.API.MLS.SubConversation (ConvOrSubConvId)
import Wire.ConversationStore
import Wire.ConversationStore.MLS.Reconcile (ReconcileResult (..))
import Wire.ConversationStore.MLS.Types (ClientMap, ConvOrSubConv, IndexMap, cmAssocs, cmIdentities, imAssocs, imLookup)
import Wire.ConversationSubsystem.Errors (ConversationSubsystemError (..))
import Wire.ConversationSubsystem.MLS.GroupInfoCheck (groupInfoLeaves)
import Wire.ConversationSubsystem.MLS.Util (getPendingBackendRemoveProposals)
import Wire.ProposalStore (ProposalStore)
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now

newtype GroupInfoRepairEnabled = GroupInfoRepairEnabled Bool
  deriving stock (Eq, Ord, Show)

-- | Lease of the commit lock taken by the repair.
repairTTL :: NominalDiffTime
repairTTL = 60

-- | Time before the lease expires after which the lock is no longer released.
releaseMargin :: NominalDiffTime
releaseMargin = 10

-- | No reconcile write is started after this time: half the lease, leaving
-- the other half for a write in flight (with its retries) to complete.
repairDeadline :: UTCTime -> UTCTime
repairDeadline acquiredAt = addUTCTime (repairTTL / 2) acquiredAt

-- | Whether a lease taken at the first time is still safely held at the
-- second one. Only then may the lock be released: once the lease expired, a
-- successor may hold the same (group, epoch) lock.
leaseStillHeld :: UTCTime -> UTCTime -> Bool
leaseStillHeld acquiredAt now = diffUTCTime now acquiredAt < repairTTL - releaseMargin

-- | SHA-256 digest of a raw GroupInfo.
groupInfoDigest :: ByteString -> ByteString
groupInfoDigest raw = BA.convert (hash raw :: Digest SHA256)

-- | The digest to record for a GroupInfo uploaded with a commit: only if it
-- belongs to the group, claims the post-commit epoch and its tree matches the
-- post-commit membership the server validated. Binding group id and epoch
-- matters because the repair relies on them to reject stale trees.
validatedGroupInfoDigest :: GroupId -> Epoch -> IndexMap -> RawMLS GroupInfo -> Maybe ByteString
validatedGroupInfoDigest gid postCommitEpoch newIndexMap groupInfo =
  guard
    ( ctx.groupId == gid
        && ctx.epoch == postCommitEpoch
        && groupInfoLeaves groupInfo.value == Right newIndexMap
    )
    $> groupInfoDigest groupInfo.raw
  where
    ctx = groupInfo.value.tbs.groupContext

-- | Returns True if the caller must refetch the conversation: its member rows
-- disagree with a trusted stored tree. This holds even if this request wrote
-- nothing, because a concurrent request may already have healed the rows.
-- Throws 'ConversationSubsystemErrorMLSStaleMessage' if a repair is needed but
-- the commit lock is held, or if the repair ran out of time: the rows may
-- still be (partly) damaged.
repairMLSMemberClients ::
  ( Member ConversationStore r,
    Member (Error ConversationSubsystemError) r,
    Member MLSCommitLockStore r,
    Member Now r,
    Member ProposalStore r,
    Member Resource r,
    Member TinyLog r,
    Member (Input (Maybe GroupInfoRepairEnabled)) r
  ) =>
  Local ConvOrSubConv ->
  Sem r Bool
repairMLSMemberClients lConvOrSub = do
  enabled <- isGroupInfoRepairEnabled
  case (enabled, convOrSub.mlsMeta.cnvmlsActiveData) of
    (True, Just activeData) -> do
      let gid = convOrSub.mlsMeta.cnvmlsGroupId
          epoch = Protocol.epoch activeData
      -- Cheap read-only precheck: the healthy case (no diff) never reads the
      -- validation digest nor takes the per-group commit lock.
      suspected <- isJust <$> trustedDivergingLeaves convOrSub gid epoch
      when suspected $ do
        -- Taken before acquiring, so the lease cannot have started earlier.
        acquiredAt <- Now.get
        acquireCommitLock gid epoch repairTTL >>= \case
          Acquired -> do
            complete <-
              repairUnderLock convOrSub gid epoch (repairDeadline acquiredAt)
                `finally` releaseIfLeaseHeld gid epoch acquiredAt
            unless complete $ do
              TinyLog.warn $ skipMsg gid "repair deadline reached before all rows were written" id
              throw ConversationSubsystemErrorMLSStaleMessage
          NotAcquired -> do
            TinyLog.info $ skipMsg gid "commit lock held by a concurrent commit or repair" id
            throw ConversationSubsystemErrorMLSStaleMessage
      pure suspected
    _ -> pure False
  where
    convOrSub = tUnqualified lConvOrSub

-- | The leaves of the stored tree if it is trusted, belongs to the group, is
-- at the expected epoch and disagrees with the rows of the given snapshot.
trustedDivergingLeaves ::
  ( Member ConversationStore r,
    Member TinyLog r
  ) =>
  ConvOrSubConv ->
  GroupId ->
  Epoch ->
  Sem r (Maybe IndexMap)
trustedDivergingLeaves convOrSub gid epoch =
  storedLeaves repairSkipped convOrSub.id gid epoch >>= \case
    Just (raw, leaves)
      | not (rowsMatchTree convOrSub.indexMap convOrSub.members leaves) -> do
          validatedHash <- getGroupInfoValidatedHash convOrSub.id
          if validatedHash == Just (groupInfoDigest raw)
            then pure (Just leaves)
            else do
              -- Info, not warn: any member can upload a non-matching tree.
              TinyLog.info $ skipMsg gid "stored groupinfo tree was not validated against the server membership" id
              pure Nothing
    _ -> pure Nothing

-- | Whether a snapshot's rows agree with the tree leaves, as far as the
-- reconcile ('mlsReconcileOps') would act on them:
--
--   * every active (non-pending) regular-client row sits on the leaf the tree
--     assigns to it (else it would be updated or deleted); checked per row, so
--     rows sharing a leaf index are caught although the index map keeps only
--     one of them;
--   * every regular-client tree leaf has a row, pending or not (else it would
--     be inserted);
--   * the history-client entries equal those of the tree (pending flags of
--     history rows are not in the snapshot).
--
-- Pending regular rows the tree disagrees with are deliberately ignored: the
-- reconcile never touches them, so flagging them would make every commit
-- retry a repair that cannot converge.
rowsMatchTree :: IndexMap -> ClientMap LeafIndex -> IndexMap -> Bool
rowsMatchTree indexMap activeMembers leaves =
  all activeOnTreeLeaf (cmAssocs activeMembers)
    && all (`Set.member` rowClients) [cid | (_, RegularClient cid) <- imAssocs leaves]
    && historyEntries indexMap == historyEntries leaves
  where
    activeOnTreeLeaf (cid, idx) = imLookup leaves idx == Just (RegularClient cid)
    rowClients =
      Set.fromList ([cid | (_, RegularClient cid) <- imAssocs indexMap] <> cmIdentities activeMembers)
    historyEntries im = [(idx, h) | (idx, HistoryClient h) <- imAssocs im]

-- | The raw stored GroupInfo and its leaves, if it is decodable, belongs to
-- the group and is at the expected epoch. Otherwise, a warning is logged with
-- the given message, saying what is skipped.
storedLeaves ::
  ( Member ConversationStore r,
    Member TinyLog r
  ) =>
  ByteString ->
  ConvOrSubConvId ->
  GroupId ->
  Epoch ->
  Sem r (Maybe (ByteString, IndexMap))
storedLeaves skipped convOrSubId gid epoch =
  getConvOrSubGroupInfo convOrSubId >>= \case
    Nothing -> pure Nothing
    Just (GroupInfoData raw) ->
      case decodeMLS' raw of
        -- Undecodable stored groupinfo: nothing authoritative to heal from.
        Left e -> warnSkip "undecodable stored groupinfo" (Log.field "detail" e)
        Right groupInfo
          | groupInfo.tbs.groupContext.groupId /= gid ->
              warnSkip "stored groupinfo belongs to another group" id
          -- Stale tree (rows advanced past the stored groupinfo): healing from it
          -- would roll back the last commit's membership changes.
          | groupInfo.tbs.groupContext.epoch /= epoch ->
              warnSkip
                "stored groupinfo epoch mismatch"
                ( Log.field "conversationEpoch" (epochNumber epoch)
                    . Log.field "groupInfoEpoch" (epochNumber groupInfo.tbs.groupContext.epoch)
                )
          | otherwise ->
              case groupInfoLeaves groupInfo of
                Left e -> warnSkip "cannot extract leaves from stored groupinfo" (Log.field "detail" e)
                Right leaves -> pure (Just (raw, leaves))
  where
    warnSkip reason extra = TinyLog.warn (skipMsgWith skipped gid reason extra) $> Nothing

-- | Release the commit lock unless the lease may have expired, in which case
-- it is left to expire: a successor may hold it by now.
releaseIfLeaseHeld ::
  ( Member MLSCommitLockStore r,
    Member Now r,
    Member TinyLog r
  ) =>
  GroupId ->
  Epoch ->
  UTCTime ->
  Sem r ()
releaseIfLeaseHeld gid epoch acquiredAt = do
  now <- Now.get
  if leaseStillHeld acquiredAt now
    then releaseCommitLock gid epoch
    else
      TinyLog.warn $
        Log.msg ("not releasing MLS commit lock of the member-client repair: lease may have expired" :: ByteString)
          . groupIdField gid
          . Log.field "epoch" (epochNumber epoch)

-- | Re-check everything under the commit lock against live data, then write
-- the diff. Returns False if the deadline stopped the writes early.
repairUnderLock ::
  ( Member ConversationStore r,
    Member ProposalStore r,
    Member TinyLog r
  ) =>
  ConvOrSubConv ->
  GroupId ->
  Epoch ->
  UTCTime ->
  Sem r Bool
repairUnderLock convOrSub gid epoch deadline = do
  -- The epoch in the snapshot was read before the lock was acquired; a
  -- commit may have advanced the group meanwhile.
  liveEpoch <- getConvOrSubEpoch convOrSub.id
  if liveEpoch /= Just epoch
    then do
      TinyLog.warn $
        skipMsg
          gid
          "live epoch moved past the locked epoch"
          ( Log.field "lockedEpoch" (epochNumber epoch)
              . Log.field "liveEpoch" (maybe "none" (show . epochNumber) liveEpoch)
          )
      pure True
    else
      -- Re-read the stored groupinfo and its digest: either may have changed
      -- before the lock was acquired. The diff itself is computed against the
      -- current rows, so a concurrent heal results in no writes.
      trustedDivergingLeaves convOrSub gid epoch >>= \case
        Nothing -> pure True
        Just leaves -> do
          -- A row lost after its removal was planned is restored as pending,
          -- so the outstanding remove proposal still resolves to it.
          pendingLeaves <- getPendingBackendRemoveProposals gid epoch
          result <- reconcileMLSClients gid leaves pendingLeaves deadline
          let logAt = if result.inserted + result.updated + result.deleted > 0 then TinyLog.warn else TinyLog.debug
          logAt $
            Log.msg ("repaired MLS member-client rows from ratchet tree" :: ByteString)
              . groupIdField gid
              . Log.field "epoch" (epochNumber epoch)
              . Log.field "inserted" result.inserted
              . Log.field "updated" result.updated
              . Log.field "deleted" result.deleted
              . Log.field "complete" (show result.complete)
          pure result.complete

skipMsg :: GroupId -> ByteString -> (Log.Msg -> Log.Msg) -> Log.Msg -> Log.Msg
skipMsg = skipMsgWith repairSkipped

repairSkipped :: ByteString
repairSkipped = "skipping MLS member-client repair"

skipMsgWith :: ByteString -> GroupId -> ByteString -> (Log.Msg -> Log.Msg) -> Log.Msg -> Log.Msg
skipMsgWith skipped gid reason extra =
  Log.msg skipped
    . groupIdField gid
    . Log.field "reason" reason
    . extra

groupIdField :: GroupId -> Log.Msg -> Log.Msg
groupIdField gid = Log.field "groupId" ("0x" <> hex (unGroupId gid))

isGroupInfoRepairEnabled :: (Member (Input (Maybe GroupInfoRepairEnabled)) r) => Sem r Bool
isGroupInfoRepairEnabled =
  input <&> \case
    Just (GroupInfoRepairEnabled b) -> b
    Nothing -> False
