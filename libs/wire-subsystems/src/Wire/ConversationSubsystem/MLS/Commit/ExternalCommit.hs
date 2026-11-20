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

module Wire.ConversationSubsystem.MLS.Commit.ExternalCommit
  ( ExternalCommitAction (..),
    getExternalCommitData,
    processExternalCommit,
  )
where

import Control.Comonad
import Control.Lens (forOf_)
import Control.Monad.Codensity
import Data.Map qualified as Map
import Data.Qualified
import Data.Set qualified as Set
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Resource (Resource)
import Polysemy.State
import Polysemy.TinyLog (TinyLog)
import Wire.API.Conversation.Protocol
import Wire.API.Error.Galley
import Wire.API.Federation.Error
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Commit
import Wire.API.MLS.Credential
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Proposal
import Wire.API.MLS.ProposalTag
import Wire.API.MLS.SubConversation
import Wire.ConversationStore
import Wire.ConversationStore.MLS.Types
import Wire.ConversationSubsystem.Errors (ConversationSubsystemError (..))
import Wire.ConversationSubsystem.MLS.Commit.Core
import Wire.ConversationSubsystem.MLS.IncomingMessage
import Wire.ConversationSubsystem.MLS.Proposal
import Wire.ConversationSubsystem.MLS.Removal
import Wire.ConversationSubsystem.MLS.Repair (storedLeaves)
import Wire.ConversationSubsystem.MLS.Util

data ExternalCommitAction = ExternalCommitAction
  { add :: LeafIndex,
    remove :: Maybe LeafIndex
  }

getExternalCommitData ::
  forall r.
  ( Member (Error MLSProtocolError) r,
    Member (Error ConversationSubsystemError) r,
    Member ConversationStore r,
    Member TinyLog r
  ) =>
  ClientIdentity ->
  Local ConvOrSubConv ->
  Epoch ->
  Commit ->
  Sem r (IndexMap, ExternalCommitAction)
getExternalCommitData senderIdentity lConvOrSub epoch commit = do
  let convOrSub = tUnqualified lConvOrSub
  activeData <-
    note (mlsProtocolError "The first commit in a group cannot be external") $
      cnvmlsActiveData convOrSub.mlsMeta
  let curEpoch = activeData.epoch
  when (epoch /= curEpoch) $ throw ConversationSubsystemErrorMLSStaleMessage
  when (epoch == Epoch 0) $
    throw $
      mlsProtocolError "The first commit in a group cannot be external"
  proposals <- traverse getInlineProposal commit.proposals

  -- According to the spec, an external commit must contain:
  -- (https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol.html#section-12.2)
  --
  -- > Exactly one ExternalInit
  -- > At most one Remove proposal, with which the joiner removes an old
  -- > version of themselves.
  -- > Zero or more PreSharedKey proposals.
  -- > No other proposals.
  let counts = foldr (\x -> Map.insertWith (+) x.tag (1 :: Int)) mempty proposals

  unless (Map.lookup ExternalInitProposalTag counts == Just 1) $
    throw (mlsProtocolError "External commits must contain exactly one ExternalInit proposal")
  unless (null (Map.keys counts \\ allowedProposals)) $
    throw (mlsProtocolError "Invalid proposal type in an external commit")

  let senderTracked = isJust (cmLookupIndex senderIdentity convOrSub.members)
      notSelf = throw $ mlsProtocolError "Only the self client can be removed by an external commit"
  (im, (action, untracked)) <- runState convOrSub.indexMap $ do
    -- process optional removal
    propAction <- applyProposals AllowUntrackedRemoval activeData.ciphersuite proposals
    removedIndex <- case (cmAssocs propAction.paRemove, toList propAction.paRemoveUntracked) of
      ([(cid, idx)], []) -> Just idx <$ when (cid /= senderIdentity) notSelf
      -- A leaf without a member row can only be the sender's if the sender
      -- has no row either.
      ([], [idx]) -> Just idx <$ when senderTracked notSelf
      -- no remove proposal, make sure the client is not already part of the conversation
      ([], []) ->
        Nothing
          <$ when
            senderTracked
            (throw (mlsProtocolError "External commits for existing members must contain a Remove proposal"))
      _ -> throw (mlsProtocolError "External commits must contain at most one Remove proposal")

    -- add sender client
    im <- get
    let (addedIndex, im') = imAddClient im (RegularClient senderIdentity)
    put im'

    pure
      ( ExternalCommitAction
          { add = addedIndex,
            remove = removedIndex
          },
        propAction.paRemoveUntracked
      )
  for_ untracked $ checkUntrackedSelfRemoval senderIdentity convOrSub epoch
  logUntrackedLeafRemovals (cnvmlsGroupId convOrSub.mlsMeta) epoch untracked
  pure (im, action)
  where
    allowedProposals = [ExternalInitProposalTag, RemoveProposalTag, PreSharedKeyProposalTag]

    getInlineProposal :: ProposalOrRef -> Sem r Proposal
    getInlineProposal (Ref _) =
      throw (mlsProtocolError "External commits cannot reference proposals")
    getInlineProposal (Inline p) = pure p

-- | The server cannot tell whose a leaf without a member row is. If the
-- stored GroupInfo describes the current epoch of this group, its tree must
-- hold the sender at the removed leaf: an external joiner may only remove an
-- old version of itself. Without such a GroupInfo, the receiving clients are
-- left to enforce it (RFC 9420, section 12.4.3.2).
checkUntrackedSelfRemoval ::
  ( Member ConversationStore r,
    Member (Error ConversationSubsystemError) r,
    Member TinyLog r
  ) =>
  ClientIdentity ->
  ConvOrSubConv ->
  Epoch ->
  LeafIndex ->
  Sem r ()
checkUntrackedSelfRemoval senderIdentity convOrSub epoch idx = do
  stored <-
    storedLeaves
      "skipping the identity check of an untracked self-removal"
      convOrSub.id
      (cnvmlsGroupId convOrSub.mlsMeta)
      epoch
  for_ stored $ \(_, leaves) ->
    unless (imLookup leaves idx == Just (RegularClient senderIdentity)) $
      throw ConversationSubsystemErrorMLSInvalidLeafNodeIndex

processExternalCommit ::
  forall r.
  ( Member (Error FederationError) r,
    Member Resource r,
    HasProposalActionEffects r,
    Member MLSCommitLockStore r
  ) =>
  SenderIdentity ->
  Local ConvOrSubConv ->
  CipherSuiteTag ->
  Bool ->
  Epoch ->
  ExternalCommitAction ->
  Maybe UpdatePath ->
  Codensity (Sem r) ()
processExternalCommit senderIdentity lConvOrSub ciphersuite ciphersuiteUpdate epoch action mUpdatePath = do
  let convOrSub = tUnqualified lConvOrSub
      groupId = cnvmlsGroupId convOrSub.mlsMeta

  -- only members can join a subconversation
  forOf_ _SubConv convOrSub $ \(mlsConv, _) ->
    unless (isClientMember senderIdentity.client (mcMembers mlsConv)) $
      lift $
        throw ConversationSubsystemErrorMLSSubConvClientNotInParent

  -- extract update path
  updatePath <-
    lift
      ( note
          (mlsProtocolError "External commits need an update path")
          mUpdatePath
      )
  lift $ checkUpdatePath lConvOrSub senderIdentity ciphersuite updatePath

  withCommitLock (fmap (.id) lConvOrSub) groupId epoch

  lift $ executeExternalCommitAction lConvOrSub senderIdentity.client action

  -- increment epoch number
  lConvOrSub' <- for lConvOrSub $ lift . incrementEpoch

  -- fetch backend remove proposals of the previous epoch
  indices0 <- lift $ getPendingBackendRemoveProposals groupId epoch

  -- skip proposals for clients already removed by the external commit
  let indices = maybe id Set.delete action.remove indices0

  -- set cipher suite
  lift $ when ciphersuiteUpdate $ case convOrSub.id of
    Conv cid -> setConversationCipherSuite cid ciphersuite
    SubConv cid sub -> setSubConversationCipherSuite cid sub ciphersuite

  -- requeue backend remove proposals for the current epoch
  createAndSendRemoveProposals
    lConvOrSub'
    (toList indices)
    (cidQualifiedUser senderIdentity.client)
    (tUnqualified lConvOrSub').members

executeExternalCommitAction ::
  forall r.
  (HasProposalActionEffects r) =>
  Local ConvOrSubConv ->
  ClientIdentity ->
  ExternalCommitAction ->
  Sem r ()
executeExternalCommitAction lconvOrSub senderIdentity action = do
  let mlsMeta = (tUnqualified lconvOrSub).mlsMeta

  -- The optional self-removal (action.remove) is subsumed by this upsert: the removed
  -- client is always the sender's own client (enforced in getExternalCommitData), and
  -- the insert overwrites its leaf index and resets removal_pending. Do NOT delete and
  -- re-insert the same key here: two server-timestamped Cassandra batches can collide
  -- within the same millisecond and the tombstone wins, dropping the member row
  -- (WPB-29648).
  upsertMLSClient
    (cnvmlsGroupId mlsMeta)
    senderIdentity
    action.add
