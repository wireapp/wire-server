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

module Galley.API.MLS.Commit.ExternalCommit
  ( getExternalCommitData,
    processExternalCommit,
  )
where

import Control.Comonad
import Control.Lens (forOf_)
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Set as Set
import Data.Tuple.Extra
import Galley.API.Error
import Galley.API.MLS.Commit.Core
import Galley.API.MLS.Proposal
import Galley.API.MLS.Removal
import Galley.API.MLS.Types
import Galley.API.MLS.Util
import Galley.Effects
import Galley.Effects.MemberStore
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Resource (Resource)
import Polysemy.State
import Wire.API.Conversation.Protocol
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.MLS.Commit
import Wire.API.MLS.Credential
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Proposal
import Wire.API.MLS.ProposalTag
import Wire.API.MLS.Serialisation
import Wire.API.MLS.SubConversation
import Wire.API.MLS.Validation

getExternalCommitData ::
  forall r.
  ( Member (Error MLSProtocolError) r,
    Member (ErrorS 'MLSStaleMessage) r,
    Member (ErrorS 'MLSUnsupportedProposal) r,
    Member (ErrorS 'MLSInvalidLeafNodeIndex) r
  ) =>
  ClientIdentity ->
  Local ConvOrSubConv ->
  Epoch ->
  Commit ->
  Sem r ProposalAction
getExternalCommitData senderIdentity lConvOrSub epoch commit = do
  let convOrSub = tUnqualified lConvOrSub
      mlsMeta = mlsMetaConvOrSub convOrSub
      curEpoch = cnvmlsEpoch mlsMeta
      groupId = cnvmlsGroupId mlsMeta
  when (epoch /= curEpoch) $ throwS @'MLSStaleMessage
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
  unless (Map.findWithDefault 0 RemoveProposalTag counts <= 1) $
    throw (mlsProtocolError "External commits must contain at most one Remove proposal")
  unless (null (Map.keys counts \\ allowedProposals)) $
    throw (mlsProtocolError "Invalid proposal type in an external commit")

  evalState (indexMapConvOrSub convOrSub) $ do
    -- process optional removal
    propAction <- applyProposals mlsMeta groupId proposals
    -- add sender
    selfAction <- addProposedClient senderIdentity
    case cmAssocs (paRemove propAction) of
      [(cid, _)]
        | cid /= senderIdentity ->
            throw $ mlsProtocolError "Only the self client can be removed by an external commit"
      _ -> pure ()

    pure $ propAction <> selfAction
  where
    allowedProposals = [ExternalInitProposalTag, RemoveProposalTag, PreSharedKeyProposalTag]

    getInlineProposal :: ProposalOrRef -> Sem r Proposal
    getInlineProposal (Ref _) =
      throw (mlsProtocolError "External commits cannot reference proposals")
    getInlineProposal (Inline p) = pure p

processExternalCommit ::
  forall r.
  ( Member (ErrorS 'MLSStaleMessage) r,
    Member (ErrorS 'MLSSubConvClientNotInParent) r,
    Member Resource r,
    HasProposalActionEffects r
  ) =>
  ClientIdentity ->
  Local ConvOrSubConv ->
  Epoch ->
  ProposalAction ->
  Maybe UpdatePath ->
  Sem r ()
processExternalCommit senderIdentity lConvOrSub epoch action updatePath = do
  let convOrSub = tUnqualified lConvOrSub

  -- only members can join a subconversation
  forOf_ _SubConv convOrSub $ \(mlsConv, _) ->
    unless (isClientMember senderIdentity (mcMembers mlsConv)) $
      throwS @'MLSSubConvClientNotInParent

  -- get index of the newly added client, as calculated when processing proposals
  idx <- case cmAssocs (paAdd action) of
    [(cid, idx)] | cid == senderIdentity -> pure idx
    _ -> throw (InternalErrorWithDescription "Unexpected Add action for external commit")

  -- extract leaf node from update path and validate it
  leafNode <-
    (.leaf)
      <$> note
        (mlsProtocolError "External commits need an update path")
        updatePath
  let cs = cnvmlsCipherSuite (mlsMetaConvOrSub (tUnqualified lConvOrSub))
  let groupId = cnvmlsGroupId (mlsMetaConvOrSub convOrSub)
  let extra = LeafNodeTBSExtraCommit groupId idx
  case validateLeafNode cs (Just senderIdentity) extra leafNode.value of
    Left errMsg ->
      throw $
        mlsProtocolError ("Tried to add invalid LeafNode: " <> errMsg)
    Right _ -> pure ()

  withCommitLock (fmap idForConvOrSub lConvOrSub) groupId epoch $ do
    executeExtCommitProposalAction senderIdentity lConvOrSub action

    -- increment epoch number
    lConvOrSub' <- for lConvOrSub incrementEpoch

    -- fetch backend remove proposals of the previous epoch
    let remIndices = map snd (cmAssocs (paRemove action))
    indicesInRemoveProposals <-
      -- skip remove proposals of already removed by the external commit
      (\\ remIndices)
        <$> getPendingBackendRemoveProposals groupId epoch

    -- requeue backend remove proposals for the current epoch
    let cm = membersConvOrSub (tUnqualified lConvOrSub')
    createAndSendRemoveProposals
      lConvOrSub'
      indicesInRemoveProposals
      (cidQualifiedUser senderIdentity)
      cm

executeExtCommitProposalAction ::
  forall r.
  HasProposalActionEffects r =>
  ClientIdentity ->
  Local ConvOrSubConv ->
  ProposalAction ->
  Sem r ()
executeExtCommitProposalAction senderIdentity lconvOrSub action = do
  let mlsMeta = mlsMetaConvOrSub $ tUnqualified lconvOrSub
      newCILeaves = cmAssocs (paAdd action)
      deprecatedCILeaves = cmAssocs (paRemove action)

  -- Adding clients: sender's client must be added and no other other client may
  -- be added.
  when (length newCILeaves /= 1 || fst (head newCILeaves) /= senderIdentity) $
    throw (mlsProtocolError "No add proposals are allowed in external commits")

  -- Client removal: only the sender's client can be removed when rejoining the
  -- (sub)conversation.
  when (length deprecatedCILeaves > 1) $
    throw (mlsProtocolError "Up to one client can be removed in an external commit")
  for_ (listToMaybe deprecatedCILeaves) $ \ciLeaf -> do
    when (fst ciLeaf /= senderIdentity) $
      throw (mlsProtocolError "Only the sender can rejoin in an external commit")

  -- Remove deprecated sender client from conversation state.
  for_ deprecatedCILeaves $ \(ci, _) -> do
    removeMLSClients
      (cnvmlsGroupId mlsMeta)
      (cidQualifiedUser ci)
      (Set.singleton $ ciClient ci)

  -- Add new sender client to the conversation state.
  for_ newCILeaves $ \(ci, idx) -> do
    addMLSClients
      (cnvmlsGroupId mlsMeta)
      (cidQualifiedUser ci)
      (Set.singleton (ciClient ci, idx))
