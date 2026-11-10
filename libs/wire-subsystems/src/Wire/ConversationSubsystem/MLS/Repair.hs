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
-- @mls_history_client@ rows with the authoritative ratchet tree persisted in
-- the group's GroupInfo. Runs before every commit bundle is validated, so a
-- group whose rows were lost (e.g. to the external-commit re-join race fixed
-- in WPB-29648) self-heals instead of rejecting every subsequent commit.
--
-- The reconcile runs under the per-group commit lock (skipped, not failed, if
-- the lock is held by a concurrent commit) and only when the stored GroupInfo
-- epoch matches the conversation epoch, so a concurrent commit or a
-- partially-failed commit cannot be rolled back by a stale tree.
module Wire.ConversationSubsystem.MLS.Repair
  ( GroupInfoRepairEnabled (..),
    repairMLSMemberClients,
  )
where

import Data.Hex (hex)
import Data.Qualified (Local, tUnqualified)
import Data.Time (NominalDiffTime)
import Imports
import Polysemy (Member, Sem)
import Polysemy.Input (Input, input)
import Polysemy.Resource (Resource, bracket)
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as TinyLog
import System.Logger qualified as Log
import Wire.API.Conversation.Protocol qualified as Protocol
import Wire.API.MLS.Epoch (Epoch (..), epochNumber)
import Wire.API.MLS.Group (GroupId (..))
import Wire.API.MLS.GroupInfo (GroupInfoData (..))
import Wire.API.MLS.GroupInfo qualified as GroupInfo
import Wire.API.MLS.Serialisation (decodeMLS')
import Wire.ConversationStore
import Wire.ConversationStore.MLS.Types (ConvOrSubConv)
import Wire.ConversationSubsystem.MLS.GroupInfoCheck (groupInfoLeaves)

newtype GroupInfoRepairEnabled = GroupInfoRepairEnabled Bool
  deriving stock (Eq, Ord, Show)

repairTTL :: NominalDiffTime
repairTTL = fromIntegral (60 :: Int)

-- | Returns True if rows were changed (caller must refetch the conversation).
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
      -- Cheap read-only precheck: the healthy case (no diff) never takes the
      -- per-group commit lock, so the common path costs no extra LWT.
      suspected <- precheck convOrSub gid epoch
      if not suspected
        then pure False
        else do
          lockAcquired <- acquireCommitLock gid epoch repairTTL
          if lockAcquired == NotAcquired
            then pure False -- a concurrent commit owns the group; leave it alone
            else
              bracket
                (pure ())
                (\() -> releaseCommitLock gid epoch)
                (const (repairUnderLock convOrSub gid epoch))
    _ -> pure False
  where
    convOrSub = tUnqualified lConvOrSub

-- Read-only check: True if the stored tree disagrees with the row-derived
-- index map (or the stored groupinfo cannot be interpreted, which we log).
precheck ::
  ( Member ConversationStore r,
    Member TinyLog r
  ) =>
  ConvOrSubConv ->
  GroupId ->
  Epoch ->
  Sem r Bool
precheck convOrSub gid epoch = do
  mGroupInfoData <- getConvOrSubGroupInfo convOrSub.id
  case mGroupInfoData of
    Nothing -> pure False
    Just groupInfoData ->
      case decodeMLS' (unGroupInfoData groupInfoData) of
        -- Undecodable stored groupinfo: nothing authoritative to heal from.
        Left e -> warnSkip gid "undecodable stored groupinfo" (Just e) >> pure False
        Right groupInfo
          -- Stale tree (rows advanced past the stored groupinfo): healing from it
          -- would roll back the last commit's membership changes.
          | GroupInfo.epoch groupInfo.tbs.groupContext /= epoch -> do
              TinyLog.warn $
                Log.msg ("skipping MLS member-client repair" :: ByteString)
                  . Log.field "groupId" ("0x" <> hex (unGroupId gid))
                  . Log.field "reason" ("stored groupinfo epoch mismatch" :: ByteString)
                  . Log.field "conversationEpoch" (epochNumber epoch)
                  . Log.field "groupInfoEpoch" (epochNumber (GroupInfo.epoch groupInfo.tbs.groupContext))
              pure False
          | otherwise ->
              case groupInfoLeaves groupInfo of
                Left e -> warnSkip gid "cannot extract leaves from stored groupinfo" (Just e) >> pure False
                Right leaves -> pure (leaves /= convOrSub.indexMap)

-- Re-check under the commit lock, then write the diff.
repairUnderLock ::
  ( Member ConversationStore r,
    Member TinyLog r
  ) =>
  ConvOrSubConv ->
  GroupId ->
  Epoch ->
  Sem r Bool
repairUnderLock convOrSub gid epoch = do
  suspected <- precheck convOrSub gid epoch
  if not suspected
    then pure False -- a concurrent commit healed or changed things meanwhile
    else do
      -- precheck just told us the leaves differ; recompute the diff to write.
      mGroupInfoData <- getConvOrSubGroupInfo convOrSub.id
      case mGroupInfoData of
        Just groupInfoData
          | Right groupInfo <- decodeMLS' (unGroupInfoData groupInfoData),
            Right leaves <- groupInfoLeaves groupInfo -> do
              (ins, upd, del) <- reconcileMLSClients gid leaves
              TinyLog.warn $
                Log.msg ("repaired MLS member-client rows from ratchet tree" :: ByteString)
                  . Log.field "groupId" ("0x" <> hex (unGroupId gid))
                  . Log.field "epoch" (epochNumber epoch)
                  . Log.field "inserted" ins
                  . Log.field "updated" upd
                  . Log.field "deleted" del
              pure (ins + upd + del > 0)
        _ -> pure False -- vanished mid-flight; the next bundle retries

warnSkip ::
  (Member TinyLog r) =>
  GroupId ->
  ByteString ->
  Maybe Text ->
  Sem r ()
warnSkip gid reason mDetail =
  TinyLog.warn $
    Log.msg ("skipping MLS member-client repair" :: ByteString)
      . Log.field "groupId" ("0x" <> hex (unGroupId gid))
      . Log.field "reason" reason
      . Log.field "detail" (fromMaybe "" mDetail)

isGroupInfoRepairEnabled :: (Member (Input (Maybe GroupInfoRepairEnabled)) r) => Sem r Bool
isGroupInfoRepairEnabled =
  input <&> \case
    Just (GroupInfoRepairEnabled b) -> b
    Nothing -> False
