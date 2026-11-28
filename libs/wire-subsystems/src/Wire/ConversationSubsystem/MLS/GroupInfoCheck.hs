-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.ConversationSubsystem.MLS.GroupInfoCheck
  ( checkGroupState,
    resolveUnknownLeaves,
    GroupInfoMismatch (..),
    GroupInfoCheckEnabled (..),
  )
where

import Data.Bifunctor
import Data.Hex
import Data.Id
import Data.Map qualified as Map
import Data.Set qualified as Set
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.NonDet
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as TinyLog
import System.Logger qualified as Log
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Protocol
import Wire.API.Error.Galley
import Wire.API.MLS.Credential
import Wire.API.MLS.Extension
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.LeafNode
import Wire.API.MLS.RatchetTree
import Wire.API.MLS.Serialisation
import Wire.API.Team.Feature
import Wire.ConversationStore
import Wire.ConversationStore.MLS.Types
import Wire.ConversationSubsystem.Errors (ConversationSubsystemError (..))
import Wire.FeaturesConfigSubsystem (FeaturesConfigSubsystem, getFeatureForTeam)

data GroupInfoMismatch = GroupInfoMismatch
  {clients :: [(Int, GroupMember)]}
  deriving (Show)

newtype GroupInfoCheckEnabled
  = GroupInfoCheckEnabled Bool
  deriving stock (Eq, Ord, Show)

checkGroupState ::
  forall r.
  ( Member (Error GroupInfoMismatch) r,
    Member (Input (Maybe GroupInfoCheckEnabled)) r,
    Member (Error MLSProtocolError) r,
    Member ConversationStore r,
    Member FeaturesConfigSubsystem r,
    Member (Error ConversationSubsystemError) r
  ) =>
  ConvOrSubConv ->
  IndexMap ->
  GroupInfo ->
  Sem r ()
checkGroupState convOrSub newLeaves groupInfo = do
  check <- isGroupInfoCheckEnabled convOrSub.conv.mcMetadata.cnvmTeam
  case (check, groupStateMismatch newLeaves groupInfo) of
    (True, Left e) -> throw (mlsProtocolError e)
    (True, Right (Just mismatch)) -> do
      existingMismatch <- existingGroupStateMismatch convOrSub
      when (isNothing existingMismatch) $ throw mismatch
    _ -> pure ()

groupStateMismatch :: IndexMap -> GroupInfo -> Either Text (Maybe GroupInfoMismatch)
groupStateMismatch leaves groupInfo = do
  giLeaves <- groupInfoLeaves groupInfo
  pure $ guard (leaves /= giLeaves) $> GroupInfoMismatch (imAssocs leaves)

-- | The members of the ratchet tree carried by a GroupInfo, by leaf index.
groupInfoLeaves :: GroupInfo -> Either Text IndexMap
groupInfoLeaves groupInfo = do
  trees <-
    first
      (const "Could not parse ratchet tree extension in GroupInfo")
      $ findExtension groupInfo.tbs.extensions
  tree :: RatchetTree <- case trees of
    (tree : _) -> pure tree
    _ -> Left "No ratchet tree extension found in GroupInfo"
  imFromList <$> traverse (traverse getIdentity) (ratchetTreeLeaves tree)
  where
    getIdentity :: LeafNode -> Either Text GroupMember
    getIdentity leaf = fst <$> credentialIdentityAndKey leaf.credential

-- | Resolve leaves removed by a commit that have no backend member row (see
-- 'paRemoveUnknown') to their members, using the ratchet tree of the stored
-- GroupInfo of the current epoch. Fails with 'mls-invalid-leaf-node-index' if
-- a leaf is blank or out of range there, or if that GroupInfo is missing or
-- unusable, and with 'mls-stale-message' if a concurrent commit already stored
-- the GroupInfo of a later epoch.
--
-- The stored GroupInfo is supplied by clients and not authenticated by the
-- backend: the result may only be used to validate a commit, never to grant
-- membership or roles, nor to change member rows.
resolveUnknownLeaves ::
  ( Member ConversationStore r,
    Member (Error ConversationSubsystemError) r,
    Member TinyLog r
  ) =>
  ConvOrSubConv ->
  Set LeafIndex ->
  Sem r (Map LeafIndex GroupMember)
resolveUnknownLeaves convOrSub indices
  | Set.null indices = pure mempty
  | otherwise = do
      mGroupInfoData <- getConvOrSubGroupInfo convOrSub.id
      groupInfo :: GroupInfo <-
        note ConversationSubsystemErrorMLSInvalidLeafNodeIndex $
          rightToMaybe . decodeMLS' . unGroupInfoData =<< mGroupInfoData
      let storedEpoch = groupInfo.tbs.groupContext.epoch
      case (.epoch) <$> convOrSub.mlsMeta.cnvmlsActiveData of
        Just currentEpoch
          | storedEpoch > currentEpoch -> throw ConversationSubsystemErrorMLSStaleMessage
          | storedEpoch == currentEpoch -> pure ()
        _ -> throw ConversationSubsystemErrorMLSInvalidLeafNodeIndex
      members <-
        note ConversationSubsystemErrorMLSInvalidLeafNodeIndex $ do
          leaves <- rightToMaybe (groupInfoLeaves groupInfo)
          traverse (imLookup leaves) (Map.fromSet id indices)
      TinyLog.warn $
        Log.msg ("accepting MLS Remove proposals for leaves without a member row" :: ByteString)
          . Log.field "groupId" ("0x" <> hex (unGroupId convOrSub.mlsMeta.cnvmlsGroupId))
          . Log.field "leafIndexCount" (Set.size indices)
          . Log.field "leafIndices" (show (take 10 (Set.toList indices)))
      pure members

existingGroupStateMismatch ::
  (Member ConversationStore r, Member (Error ConversationSubsystemError) r) =>
  ConvOrSubConv ->
  Sem r (Maybe GroupInfoMismatch)
existingGroupStateMismatch convOrSub = do
  res <-
    runError @ConversationSubsystemError $
      do
        groupInfoData <- getConvOrSubGroupInfo convOrSub.id >>= note ConversationSubsystemErrorMLSMissingGroupInfo
        groupInfo <-
          either (\_ -> throw ConversationSubsystemErrorMLSMissingGroupInfo) pure $
            decodeMLS' (unGroupInfoData groupInfoData)
        case groupStateMismatch convOrSub.indexMap groupInfo of
          Left _ -> throw ConversationSubsystemErrorMLSMissingGroupInfo
          Right m -> pure m
  case res of
    Right m -> pure m
    Left ConversationSubsystemErrorMLSMissingGroupInfo -> pure Nothing
    Left e -> throw e

isGroupInfoCheckEnabled ::
  ( Member FeaturesConfigSubsystem r,
    Member (Input (Maybe GroupInfoCheckEnabled)) r
  ) =>
  Maybe TeamId ->
  Sem r Bool
isGroupInfoCheckEnabled Nothing = pure False
isGroupInfoCheckEnabled (Just tid) = fmap isJust . runNonDetMaybe $ do
  global <- input
  guard (global == Just (GroupInfoCheckEnabled True))
  mls <- getFeatureForTeam @_ @MLSConfig tid
  guard (getAny mls.config.mlsGroupInfoDiagnostics)
