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

module Wire.ConversationSubsystem.MLS.Util where

import Control.Comonad
import Control.Monad.Codensity
import Data.ByteString.Conversion (toByteString')
import Data.Hex
import Data.Id
import Data.Qualified
import Data.Set qualified as Set
import Data.Text qualified as T
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Resource (Resource, bracket)
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as TinyLog
import System.Logger qualified as Log
import Wire.API.Conversation hiding (Member)
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group.Serialisation
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation
import Wire.API.MLS.SubConversation
import Wire.ConversationStore
import Wire.ProposalStore
import Wire.StoredConversation

getLocalConvForUser ::
  ( Member (ErrorS 'ConvNotFound) r,
    Member ConversationStore r
  ) =>
  Qualified UserId ->
  Local ConvId ->
  Sem r StoredConversation
getLocalConvForUser qusr lcnv = do
  conv <- getConversation (tUnqualified lcnv) >>= noteS @'ConvNotFound

  -- check that sender is part of conversation
  isMember' <-
    foldQualified
      lcnv
      ( fmap isJust
          . getLocalMember conv.id_
          . tUnqualified
      )
      (fmap isJust . getRemoteMember conv.id_)
      qusr
  unless isMember' $ throwS @'ConvNotFound

  pure conv

getPendingBackendRemoveProposals ::
  ( Member ProposalStore r,
    Member TinyLog r
  ) =>
  GroupId ->
  Epoch ->
  Sem r (Set LeafIndex)
getPendingBackendRemoveProposals gid epoch = do
  proposals <- getAllPendingProposals gid epoch
  indexList <-
    catMaybes
      <$> for
        proposals
        ( \prop -> case (prop.origin, prop.proposal.value) of
            (Just ProposalOriginBackend, RemoveProposal i) -> pure (Just i)
            (Nothing, _) -> do
              TinyLog.warn $ Log.msg ("found pending proposal without origin, ignoring" :: ByteString)
              pure Nothing
            _ -> pure Nothing
        )

  let indexSet = Set.fromList indexList
  when (length indexList /= length indexSet) $ do
    TinyLog.warn $
      Log.msg ("found duplicate proposals" :: ByteString)
        . Log.field "groupId" ("0x" <> hex (unGroupId gid))
        . Log.field "epoch" (epochNumber epoch)
  pure indexSet

withCommitLock ::
  forall r.
  ( Member Resource r,
    Member ConversationStore r,
    Member (ErrorS 'MLSStaleMessage) r,
    Member MLSCommitLockStore r,
    Member TinyLog r
  ) =>
  Local ConvOrSubConvId ->
  GroupId ->
  Epoch ->
  Codensity (Sem r) ()
withCommitLock lConvOrSubId gid epoch =
  Codensity $ \k ->
    bracket
      ( acquireCommitLock gid epoch ttl >>= \lockAcquired ->
          when (lockAcquired == NotAcquired) $ do
            logStaleCommitLock
              "commit-lock-not-acquired"
              lConvOrSubId
              gid
              epoch
              Nothing
            throwS @'MLSStaleMessage
      )
      (const $ releaseCommitLock gid epoch)
      ( const $ do
          actualEpoch <-
            fromMaybe (Epoch 0) <$> case tUnqualified lConvOrSubId of
              Conv cnv -> getConversationEpoch cnv
              SubConv cnv sub -> getSubConversationEpoch cnv sub
          unless (actualEpoch == epoch) $ do
            logStaleCommitLock
              "commit-lock-epoch-mismatch"
              lConvOrSubId
              gid
              epoch
              (Just actualEpoch)
            throwS @'MLSStaleMessage
          k ()
      )
  where
    ttl = fromIntegral (600 :: Int) -- 10 minutes

logStaleCommitLock ::
  (Member TinyLog r) =>
  ByteString ->
  Local ConvOrSubConvId ->
  GroupId ->
  Epoch ->
  Maybe Epoch ->
  Sem r ()
logStaleCommitLock reason lConvOrSubId gid messageEpoch mStoredEpoch =
  let convOrSubId = tUnqualified lConvOrSubId
   in TinyLog.warn $
        Log.msg ("rejecting stale MLS commit due to commit lock" :: ByteString)
          . Log.field "reason" reason
          . Log.field "groupId" ("0x" <> hex (unGroupId gid))
          . Log.field "messageEpoch" (epochNumber messageEpoch)
          . Log.field "storedEpoch" (maybe ("unknown" :: ByteString) (toByteString' . epochNumber) mStoredEpoch)
          . Log.field "domain" (toByteString' (show (tDomain lConvOrSubId)))
          . Log.field "parentConvId" (toByteString' (show convOrSubId.conv))
          . Log.field "subConvId" (maybe ("none" :: ByteString) (toByteString' . show) convOrSubId.subconv)
          . Log.field "convOrSubConvId" (toByteString' (show convOrSubId))
          . Log.field "isSubConversation" (isSubConv convOrSubId)

getConvFromGroupId ::
  (Member (Error MLSProtocolError) r) =>
  GroupId ->
  Sem r (ConvType, Qualified ConvOrSubConvId)
getConvFromGroupId gid = case groupIdToConv gid of
  Left e -> throw (mlsProtocolError ("Could not parse group ID: " <> T.pack e))
  Right (_, parts) -> pure (parts.convType, parts.qConvId)
