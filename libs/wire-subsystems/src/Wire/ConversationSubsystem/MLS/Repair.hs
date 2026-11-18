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
-- commit lock (skipped, not failed, if the lock is held by a concurrent
-- commit) and only when the live epoch and the stored GroupInfo epoch both
-- match the locked epoch, so a concurrent or partially-failed commit cannot
-- be rolled back by a stale tree.
module Wire.ConversationSubsystem.MLS.Repair
  ( GroupInfoRepairEnabled (..),
    repairMLSMemberClients,
    validatedGroupInfoDigest,
  )
where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray qualified as BA
import Data.Hex (hex)
import Data.Qualified (Local, tUnqualified)
import Data.Time (NominalDiffTime)
import Imports
import Polysemy (Member, Sem)
import Polysemy.Input (Input, input)
import Polysemy.Resource (Resource, finally)
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as TinyLog
import System.Logger qualified as Log
import Wire.API.Conversation.Protocol qualified as Protocol
import Wire.API.MLS.Epoch (Epoch (..), epochNumber)
import Wire.API.MLS.Group (GroupId (..))
import Wire.API.MLS.GroupInfo (GroupContext (..), GroupInfo (..), GroupInfoData (..), GroupInfoTBS (..))
import Wire.API.MLS.Serialisation (RawMLS (..), decodeMLS')
import Wire.API.MLS.SubConversation (ConvOrSubConvId)
import Wire.ConversationStore
import Wire.ConversationStore.MLS.Types (ConvOrSubConv, IndexMap)
import Wire.ConversationSubsystem.MLS.GroupInfoCheck (groupInfoLeaves)

newtype GroupInfoRepairEnabled = GroupInfoRepairEnabled Bool
  deriving stock (Eq, Ord, Show)

repairTTL :: NominalDiffTime
repairTTL = fromIntegral (60 :: Int)

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
repairMLSMemberClients ::
  ( Member ConversationStore r,
    Member MLSCommitLockStore r,
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
        lockAcquired <- acquireCommitLock gid epoch repairTTL
        -- NotAcquired: a concurrent commit owns the group; leave it alone.
        when (lockAcquired == Acquired) $
          repairUnderLock convOrSub gid epoch `finally` releaseCommitLock gid epoch
      pure suspected
    _ -> pure False
  where
    convOrSub = tUnqualified lConvOrSub

-- | The leaves of the stored tree if it is trusted, belongs to the group, is
-- at the expected epoch and disagrees with the row-derived index map of the
-- given snapshot.
trustedDivergingLeaves ::
  ( Member ConversationStore r,
    Member TinyLog r
  ) =>
  ConvOrSubConv ->
  GroupId ->
  Epoch ->
  Sem r (Maybe IndexMap)
trustedDivergingLeaves convOrSub gid epoch =
  storedLeaves convOrSub.id gid epoch >>= \case
    Just (raw, leaves)
      | leaves /= convOrSub.indexMap -> do
          validatedHash <- getGroupInfoValidatedHash convOrSub.id
          if validatedHash == Just (groupInfoDigest raw)
            then pure (Just leaves)
            else do
              -- Info, not warn: any member can upload a non-matching tree.
              TinyLog.info $ skipMsg gid "stored groupinfo tree was not validated against the server membership" id
              pure Nothing
    _ -> pure Nothing

-- | The raw stored GroupInfo and its leaves, if it is decodable, belongs to
-- the group and is at the expected epoch.
storedLeaves ::
  ( Member ConversationStore r,
    Member TinyLog r
  ) =>
  ConvOrSubConvId ->
  GroupId ->
  Epoch ->
  Sem r (Maybe (ByteString, IndexMap))
storedLeaves convOrSubId gid epoch =
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
    warnSkip reason extra = TinyLog.warn (skipMsg gid reason extra) $> Nothing

-- | Re-check everything under the commit lock against live data, then write
-- the diff.
repairUnderLock ::
  ( Member ConversationStore r,
    Member TinyLog r
  ) =>
  ConvOrSubConv ->
  GroupId ->
  Epoch ->
  Sem r ()
repairUnderLock convOrSub gid epoch = do
  -- The epoch in the snapshot was read before the lock was acquired; a
  -- commit may have advanced the group meanwhile.
  liveEpoch <- getConvOrSubEpoch convOrSub.id
  if liveEpoch /= Just epoch
    then
      TinyLog.warn $
        skipMsg
          gid
          "live epoch moved past the locked epoch"
          ( Log.field "lockedEpoch" (epochNumber epoch)
              . Log.field "liveEpoch" (maybe "none" (show . epochNumber) liveEpoch)
          )
    else
      -- Re-read the stored groupinfo and its digest: either may have changed
      -- before the lock was acquired. The diff itself is computed against the
      -- current rows, so a concurrent heal results in no writes.
      trustedDivergingLeaves convOrSub gid epoch >>= traverse_ \leaves -> do
        (ins, upd, del) <- reconcileMLSClients gid leaves
        let logAt = if ins + upd + del > 0 then TinyLog.warn else TinyLog.debug
        logAt $
          Log.msg ("repaired MLS member-client rows from ratchet tree" :: ByteString)
            . groupIdField gid
            . Log.field "epoch" (epochNumber epoch)
            . Log.field "inserted" ins
            . Log.field "updated" upd
            . Log.field "deleted" del

skipMsg :: GroupId -> ByteString -> (Log.Msg -> Log.Msg) -> Log.Msg -> Log.Msg
skipMsg gid reason extra =
  Log.msg ("skipping MLS member-client repair" :: ByteString)
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
