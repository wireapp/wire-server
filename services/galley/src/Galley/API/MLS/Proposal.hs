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

module Galley.API.MLS.Proposal
  ( -- * Proposal processing
    derefOrCheckProposal,
    checkProposal,
    processProposal,
    proposalProcessingStage,
    addProposedClient,
    applyProposals,

    -- * Proposal actions
    paAddClient,
    paRemoveClient,

    -- * Types
    ProposalAction (..),
    HasProposalEffects,
  )
where

import Data.Id
import Data.Map qualified as Map
import Data.Qualified
import Data.Set qualified as Set
import Data.Time
import Galley.API.Error
import Galley.API.MLS.IncomingMessage
import Galley.API.MLS.Types
import Galley.API.Util
import Galley.Effects
import Galley.Effects.BrigAccess
import Galley.Effects.ProposalStore
import Galley.Env
import Galley.Options
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import Polysemy.TinyLog
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Protocol
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.Error
import Wire.API.MLS.AuthenticatedContent
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Validation
import Wire.API.Message
import Wire.NotificationSubsystem

data ProposalAction = ProposalAction
  { paAdd :: ClientMap,
    paRemove :: ClientMap
  }
  deriving (Show)

instance Semigroup ProposalAction where
  ProposalAction add1 rem1 <> ProposalAction add2 rem2 =
    ProposalAction
      (Map.unionWith mappend add1 add2)
      (Map.unionWith mappend rem1 rem2)

instance Monoid ProposalAction where
  mempty = ProposalAction mempty mempty

paAddClient :: ClientIdentity -> LeafIndex -> ProposalAction
paAddClient cid idx = mempty {paAdd = cmSingleton cid idx}

paRemoveClient :: ClientIdentity -> LeafIndex -> ProposalAction
paRemoveClient cid idx = mempty {paRemove = cmSingleton cid idx}

-- | This is used to sort proposals into the correct processing order, as defined by the spec
data ProposalProcessingStage
  = ProposalProcessingStageExtensions
  | ProposalProcessingStageUpdate
  | ProposalProcessingStageRemove
  | ProposalProcessingStageAdd
  | ProposalProcessingStagePreSharedKey
  | ProposalProcessingStageExternalInit
  | ProposalProcessingStageReInit
  deriving (Eq, Ord)

proposalProcessingStage :: Proposal -> ProposalProcessingStage
proposalProcessingStage (AddProposal _) = ProposalProcessingStageAdd
proposalProcessingStage (RemoveProposal _) = ProposalProcessingStageRemove
proposalProcessingStage (UpdateProposal _) = ProposalProcessingStageUpdate
proposalProcessingStage (PreSharedKeyProposal _) = ProposalProcessingStagePreSharedKey
proposalProcessingStage (ReInitProposal _) = ProposalProcessingStageReInit
proposalProcessingStage (ExternalInitProposal _) = ProposalProcessingStageExternalInit
proposalProcessingStage (GroupContextExtensionsProposal _) = ProposalProcessingStageExtensions

type HasProposalEffects r =
  ( Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member NotificationSubsystem r,
    Member (Error InternalError) r,
    Member (Error FederationError) r,
    Member (Error MLSProposalFailure) r,
    Member (Error MLSProtocolError) r,
    Member (ErrorS 'MLSClientMismatch) r,
    Member (ErrorS 'MLSInvalidLeafNodeIndex) r,
    Member (ErrorS 'MLSUnsupportedProposal) r,
    Member (Error NonFederatingBackends) r,
    Member (Error UnreachableBackends) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member (Input Env) r,
    Member (Input (Local ())) r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member MemberStore r,
    Member ProposalStore r,
    Member TeamStore r,
    Member TeamStore r,
    Member TinyLog r
  )

derefOrCheckProposal ::
  ( Member (Error MLSProtocolError) r,
    Member (ErrorS 'MLSInvalidLeafNodeIndex) r,
    Member ProposalStore r,
    Member (State IndexMap) r,
    Member (ErrorS 'MLSProposalNotFound) r
  ) =>
  Epoch ->
  CipherSuiteTag ->
  GroupId ->
  ProposalOrRef ->
  Sem r Proposal
derefOrCheckProposal epoch _ciphersuite groupId (Ref ref) = do
  p <- getProposal groupId epoch ref >>= noteS @'MLSProposalNotFound
  pure p.value
derefOrCheckProposal _epoch ciphersuite _ (Inline p) = do
  im <- get
  checkProposal ciphersuite im p
  pure p

checkProposal ::
  ( Member (Error MLSProtocolError) r,
    Member (ErrorS 'MLSInvalidLeafNodeIndex) r
  ) =>
  CipherSuiteTag ->
  IndexMap ->
  Proposal ->
  Sem r ()
checkProposal ciphersuite im p = case p of
  AddProposal kp -> do
    (cs, _lifetime) <-
      either
        (\msg -> throw (mlsProtocolError ("Invalid key package in Add proposal: " <> msg)))
        pure
        $ validateKeyPackage Nothing kp.value
    -- we are not checking lifetime constraints here
    unless (ciphersuite == cs) $
      throw (mlsProtocolError "Key package ciphersuite does not match conversation")
  RemoveProposal idx -> do
    void $ noteS @'MLSInvalidLeafNodeIndex $ imLookup im idx
  _ -> pure ()

addProposedClient :: (Member (State IndexMap) r) => ClientIdentity -> Sem r ProposalAction
addProposedClient cid = do
  im <- get
  let (idx, im') = imAddClient im cid
  put im'
  pure (paAddClient cid idx)

applyProposals ::
  ( Member (State IndexMap) r,
    Member (Error MLSProtocolError) r,
    Member (ErrorS 'MLSUnsupportedProposal) r,
    Member (ErrorS 'MLSInvalidLeafNodeIndex) r
  ) =>
  CipherSuiteTag ->
  GroupId ->
  [Proposal] ->
  Sem r ProposalAction
applyProposals ciphersuite groupId =
  -- proposals are sorted before processing
  foldMap (applyProposal ciphersuite groupId)
    . sortOn proposalProcessingStage

applyProposal ::
  ( Member (State IndexMap) r,
    Member (Error MLSProtocolError) r,
    Member (ErrorS 'MLSUnsupportedProposal) r,
    Member (ErrorS 'MLSInvalidLeafNodeIndex) r
  ) =>
  CipherSuiteTag ->
  GroupId ->
  Proposal ->
  Sem r ProposalAction
applyProposal ciphersuite _groupId (AddProposal kp) = do
  (cs, _lifetime) <-
    either
      (\msg -> throw (mlsProtocolError ("Invalid key package in Add proposal: " <> msg)))
      pure
      $ validateKeyPackage Nothing kp.value
  unless (ciphersuite == cs) $
    throw (mlsProtocolError "Key package ciphersuite does not match conversation")
  -- we are not checking lifetime constraints here
  cid <- getKeyPackageIdentity kp.value
  addProposedClient cid
applyProposal _ciphersuite _groupId (RemoveProposal idx) = do
  im <- get
  (cid, im') <- noteS @'MLSInvalidLeafNodeIndex $ imRemoveClient im idx
  put im'
  pure (paRemoveClient cid idx)
applyProposal _activeData _groupId _ = pure mempty

processProposal ::
  (HasProposalEffects r) =>
  ( Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MLSStaleMessage) r
  ) =>
  Qualified UserId ->
  Local ConvOrSubConv ->
  GroupId ->
  Epoch ->
  IncomingPublicMessageContent ->
  RawMLS Proposal ->
  Sem r ()
processProposal qusr lConvOrSub groupId epoch pub prop = do
  let mlsMeta = (tUnqualified lConvOrSub).mlsMeta
  -- Check if the group ID matches that of a conversation
  unless (groupId == cnvmlsGroupId mlsMeta) $ throwS @'ConvNotFound

  case cnvmlsActiveData mlsMeta of
    Nothing -> throw $ mlsProtocolError "Bare proposals at epoch 0 are not supported"
    Just activeData -> do
      -- Check if the epoch number matches that of a conversation
      unless (epoch == activeData.epoch) $ throwS @'MLSStaleMessage

      -- FUTUREWORK: validate the member's conversation role
      checkProposal activeData.ciphersuite (tUnqualified lConvOrSub).indexMap prop.value
      when (isExternal pub.sender) $ checkExternalProposalUser qusr prop.value
      let propRef =
            authContentRef
              activeData.ciphersuite
              (incomingMessageAuthenticatedContent pub)
      storeProposal groupId epoch propRef ProposalOriginClient prop

getKeyPackageIdentity ::
  (Member (ErrorS 'MLSUnsupportedProposal) r) =>
  KeyPackage ->
  Sem r ClientIdentity
getKeyPackageIdentity =
  either (\_ -> throwS @'MLSUnsupportedProposal) pure
    . keyPackageIdentity

isExternal :: Sender -> Bool
isExternal (SenderMember _) = False
isExternal _ = True

-- check owner/subject of the key package exists and belongs to the user
checkExternalProposalUser ::
  ( Member BrigAccess r,
    Member (ErrorS 'MLSUnsupportedProposal) r,
    Member (Input (Local ())) r
  ) =>
  Qualified UserId ->
  Proposal ->
  Sem r ()
checkExternalProposalUser qusr prop = do
  loc <- qualifyLocal ()
  foldQualified
    loc
    ( \lusr -> case prop of
        AddProposal kp -> do
          ClientIdentity {ciUser, ciClient} <- getKeyPackageIdentity kp.value
          -- requesting user must match key package owner
          when (tUnqualified lusr /= ciUser) $ throwS @'MLSUnsupportedProposal
          -- client referenced in key package must be one of the user's clients
          UserClients {userClients} <- lookupClients [ciUser]
          maybe
            (throwS @'MLSUnsupportedProposal)
            (flip when (throwS @'MLSUnsupportedProposal) . Set.null . Set.filter (== ciClient))
            $ userClients Map.!? ciUser
        _ -> throwS @'MLSUnsupportedProposal
    )
    (const $ pure ()) -- FUTUREWORK: check external proposals from remote backends
    qusr
