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

module Galley.API.MLS.Util where

import Control.Comonad
import Control.Monad.Codensity
import Data.Hex
import Data.Id
import Data.Qualified
import Data.Set qualified as Set
import Data.Text qualified as T
import Galley.Data.Conversation.Types hiding (Conversation)
import Galley.Data.Conversation.Types qualified as Data
import Galley.Data.Types
import Galley.Effects
import Galley.Effects.ConversationStore
import Galley.Effects.MemberStore
import Galley.Effects.ProposalStore
import Galley.Effects.SubConversationStore
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

getLocalConvForUser ::
  ( Member (ErrorS 'ConvNotFound) r,
    Member ConversationStore r,
    Member MemberStore r
  ) =>
  Qualified UserId ->
  Local ConvId ->
  Sem r Data.Conversation
getLocalConvForUser qusr lcnv = do
  conv <- getConversation (tUnqualified lcnv) >>= noteS @'ConvNotFound

  -- check that sender is part of conversation
  isMember' <-
    foldQualified
      lcnv
      ( fmap isJust
          . getLocalMember (convId conv)
          . tUnqualified
      )
      (fmap isJust . getRemoteMember (convId conv))
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
        ( \case
            (Just ProposalOriginBackend, proposal) -> case proposal.value of
              RemoveProposal i -> pure (Just i)
              _ -> pure Nothing
            (Just ProposalOriginClient, _) -> pure Nothing
            (Nothing, _) -> do
              TinyLog.warn $ Log.msg ("found pending proposal without origin, ignoring" :: ByteString)
              pure Nothing
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
    Member SubConversationStore r
  ) =>
  Local ConvOrSubConvId ->
  GroupId ->
  Epoch ->
  Codensity (Sem r) ()
withCommitLock lConvOrSubId gid epoch =
  Codensity $ \k ->
    bracket
      ( acquireCommitLock gid epoch ttl >>= \lockAcquired ->
          when (lockAcquired == NotAcquired) $
            throwS @'MLSStaleMessage
      )
      (const $ releaseCommitLock gid epoch)
      ( const $ do
          actualEpoch <-
            fromMaybe (Epoch 0) <$> case tUnqualified lConvOrSubId of
              Conv cnv -> getConversationEpoch cnv
              SubConv cnv sub -> getSubConversationEpoch cnv sub
          unless (actualEpoch == epoch) $ throwS @'MLSStaleMessage
          k ()
      )
  where
    ttl = fromIntegral (600 :: Int) -- 10 minutes

getConvFromGroupId ::
  (Member (Error MLSProtocolError) r) =>
  GroupId ->
  Sem r (ConvType, Qualified ConvOrSubConvId)
getConvFromGroupId gid = case groupIdToConv gid of
  Left e -> throw (mlsProtocolError (T.pack e))
  Right (_, parts) -> pure (parts.convType, parts.qConvId)
