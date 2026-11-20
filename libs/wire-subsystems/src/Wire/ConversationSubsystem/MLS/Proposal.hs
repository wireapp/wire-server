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

module Wire.ConversationSubsystem.MLS.Proposal
  ( -- * Proposal processing
    derefOrCheckProposalFrom,
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
import Data.List.Extra (anySame)
import Data.Map qualified as Map
import Data.Qualified
import Data.Set qualified as Set
import GHC.Generics (Generically (..))
import Galley.Types.Error
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import Polysemy.TinyLog
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Protocol
import Wire.API.Error.Galley
import Wire.API.Federation.Client (FederatorClient)
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
import Wire.API.MLS.Validation.Error (toText)
import Wire.API.Message
import Wire.BackendNotificationQueueAccess
import Wire.BrigAPIAccess
import Wire.ConversationStore (ConversationStore)
import Wire.ConversationStore.MLS.Types
import Wire.ConversationSubsystem.Errors (ConversationSubsystemError (..))
import Wire.ConversationSubsystem.MLS.IncomingMessage
import Wire.ExternalAccess
import Wire.FederationAPIAccess (FederationAPIAccess)
import Wire.LegalHoldStore (LegalHoldStore)
import Wire.NotificationSubsystem
import Wire.ProposalStore
import Wire.Sem.Now (Now)
import Wire.TeamCollaboratorsSubsystem
import Wire.TeamStore
import Wire.Util

-- | The effect of a list of proposals on the backend's view of the group.
-- 'paRemoveUnknown' holds removed leaves without a member row; see
-- 'applyProposal'.
data ProposalAction = ProposalAction
  { paAdd :: ClientMap (LeafIndex, Maybe KeyPackage),
    paRemove :: ClientMap LeafIndex,
    paHistoryClientAdd :: Set (HistoryClientId, LeafIndex),
    paHistoryClientRemove :: Set (HistoryClientId, LeafIndex),
    paRemoveUnknown :: Set LeafIndex
  }
  deriving stock (Show, Generic)
  deriving (Semigroup, Monoid) via Generically ProposalAction

paAddClient :: GroupMember -> LeafIndex -> Maybe KeyPackage -> ProposalAction
paAddClient (RegularClient cid) idx kp = mempty {paAdd = cmSingleton cid (idx, kp)}
paAddClient (HistoryClient hid) idx _ = mempty {paHistoryClientAdd = Set.singleton (hid, idx)}

paRemoveClient :: GroupMember -> LeafIndex -> ProposalAction
paRemoveClient (RegularClient cid) idx = mempty {paRemove = cmSingleton cid idx}
paRemoveClient (HistoryClient hid) idx = mempty {paHistoryClientRemove = Set.singleton (hid, idx)}

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
    Member BrigAPIAccess r,
    Member ConversationStore r,
    Member NotificationSubsystem r,
    Member (Error InternalError) r,
    Member (Error FederationError) r,
    Member (Error MLSProposalFailure) r,
    Member (Error MLSProtocolError) r,
    Member (Error ConversationSubsystemError) r,
    Member (Error NonFederatingBackends) r,
    Member (Error UnreachableBackends) r,
    Member ExternalAccess r,
    Member (FederationAPIAccess FederatorClient) r,
    Member (Input (Local ())) r,
    Member Now r,
    Member LegalHoldStore r,
    Member ProposalStore r,
    Member TeamStore r,
    Member TinyLog r,
    Member TeamCollaboratorsSubsystem r
  )

-- | Dereference a commit proposal, looking up refs in a prefetched list of
-- pending proposals instead of issuing one point-read per ref.
derefOrCheckProposalFrom ::
  ( Member (Error MLSProtocolError) r,
    Member (Error ConversationSubsystemError) r,
    Member (State IndexMap) r
  ) =>
  [StoredProposal] ->
  CipherSuiteTag ->
  ProposalOrRef ->
  Sem r Proposal
derefOrCheckProposalFrom stored _ciphersuite (Ref ref) =
  note ConversationSubsystemErrorMLSProposalNotFound $
    (.proposal.value) <$> find ((== ref) . (.ref)) stored
derefOrCheckProposalFrom _stored ciphersuite (Inline p) = do
  im <- get
  void $ evalState im $ applyProposal ciphersuite p
  pure p

-- | Check a standalone proposal. Unlike in a commit, a Remove proposal for a
-- leaf without a member row is rejected.
checkProposal ::
  ( Member (Error MLSProtocolError) r,
    Member (Error ConversationSubsystemError) r
  ) =>
  CipherSuiteTag ->
  IndexMap ->
  Proposal ->
  Sem r ()
checkProposal ciphersuite im p = do
  action <- evalState im $ applyProposal ciphersuite p
  unless (Set.null action.paRemoveUnknown) $
    throw ConversationSubsystemErrorMLSInvalidLeafNodeIndex

addProposedClient ::
  ( Member (State IndexMap) r,
    Member (Error ConversationSubsystemError) r
  ) =>
  Either GroupMember KeyPackage ->
  Sem r ProposalAction
addProposedClient cidOrKp = do
  (cid, mKp) <- case cidOrKp of
    Left cid -> pure (cid, Nothing)
    Right kp -> do
      cid <- getKeyPackageIdentity kp
      pure (cid, Just kp)
  im <- get
  let (idx, im') = imAddClient im cid
  put im'
  pure (paAddClient cid idx mKp)

applyProposals ::
  ( Member (State IndexMap) r,
    Member (Error MLSProtocolError) r,
    Member (Error ConversationSubsystemError) r
  ) =>
  CipherSuiteTag ->
  [Proposal] ->
  Sem r ProposalAction
applyProposals ciphersuite proposals = do
  when (anySame [idx | RemoveProposal idx <- proposals]) $
    throw (mlsProtocolError "A commit must not remove the same leaf more than once")
  foldMap (applyProposal ciphersuite) (sortOn proposalProcessingStage proposals)

-- | Apply a proposal to the backend's view of the group.  A Remove proposal for
-- a leaf without a member row (lost, see WPB-29648) leaves that view unchanged
-- and is recorded in 'paRemoveUnknown', for the caller to validate against the
-- ratchet tree (see 'resolveUnknownLeaves').
applyProposal ::
  ( Member (State IndexMap) r,
    Member (Error MLSProtocolError) r,
    Member (Error ConversationSubsystemError) r
  ) =>
  CipherSuiteTag ->
  Proposal ->
  Sem r ProposalAction
applyProposal ciphersuite (AddProposal kp) = do
  (cs, _lifetime) <-
    either
      ( \case
          InvalidLeafNodeSignature -> throw ConversationSubsystemErrorMLSInvalidLeafNodeSignature
          validationError -> throw (mlsProtocolError ("Invalid key package in Add proposal: " <> (toText validationError)))
      )
      pure
      $ validateKeyPackage Nothing kp.value
  unless (ciphersuite == cs) $
    throw (mlsProtocolError "Key package ciphersuite does not match conversation")
  -- we are not checking lifetime constraints here
  addProposedClient (Right kp.value)
applyProposal _ciphersuite (RemoveProposal idx) = do
  im <- get
  case imRemoveClient im idx of
    Just (cid, im') -> do
      put im'
      pure (paRemoveClient cid idx)
    Nothing -> pure mempty {paRemoveUnknown = Set.singleton idx}
applyProposal _activeData _ = pure mempty

processProposal ::
  (HasProposalEffects r) =>
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
  unless (groupId == cnvmlsGroupId mlsMeta) $ throw ConversationSubsystemErrorConvNotFound

  -- Proposals from existing members are not authorized independently of the
  -- commit that applies them. They must therefore only be submitted as part
  -- of a commit bundle, where the committer's conversation role can be
  -- checked. External proposals are retained for the join flow and are
  -- restricted by 'checkExternalProposalUser' below.
  unless (isExternal pub.sender) $ throw ConversationSubsystemErrorMLSUnsupportedMessage

  case cnvmlsActiveData mlsMeta of
    Nothing -> throw $ mlsProtocolError "Bare proposals at epoch 0 are not supported"
    Just activeData -> do
      -- Check if the epoch number matches that of a conversation
      unless (epoch == activeData.epoch) $ throw ConversationSubsystemErrorMLSStaleMessage

      -- FUTUREWORK: validate the member's conversation role
      checkProposal activeData.ciphersuite (tUnqualified lConvOrSub).indexMap prop.value
      when (isExternal pub.sender) $ checkExternalProposalUser qusr prop.value
      let propRef =
            authContentRef
              activeData.ciphersuite
              (incomingMessageAuthenticatedContent pub)
      storeProposal
        groupId
        epoch
        StoredProposal
          { ref = propRef,
            origin = Just ProposalOriginClient,
            proposal = prop
          }

getKeyPackageIdentity ::
  (Member (Error ConversationSubsystemError) r) =>
  KeyPackage ->
  Sem r GroupMember
getKeyPackageIdentity =
  either (\_ -> throw ConversationSubsystemErrorMLSUnsupportedProposal) pure
    . keyPackageIdentity

isExternal :: Sender -> Bool
isExternal (SenderMember _) = False
isExternal _ = True

-- check owner/subject of the key package exists and belongs to the user
checkExternalProposalUser ::
  ( Member BrigAPIAccess r,
    Member (Error ConversationSubsystemError) r,
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
          groupMember <- getKeyPackageIdentity kp.value
          case groupMember of
            RegularClient (ClientIdentity {ciUser, ciClient}) -> do
              -- requesting user must match key package owner
              when (tUnqualified lusr /= ciUser) $ throw ConversationSubsystemErrorMLSUnsupportedProposal
              -- client referenced in key package must be one of the user's clients
              UserClients {userClients} <- lookupClients [ciUser]
              maybe
                (throw ConversationSubsystemErrorMLSUnsupportedProposal)
                (flip when (throw ConversationSubsystemErrorMLSUnsupportedProposal) . Set.null . Set.filter (== ciClient))
                $ userClients Map.!? ciUser
            -- We currently do not support history-client adds in external proposals/commits.
            HistoryClient _ -> throw ConversationSubsystemErrorMLSUnsupportedProposal
        _ -> throw ConversationSubsystemErrorMLSUnsupportedProposal
    )
    (const $ pure ()) -- FUTUREWORK: check external proposals from remote backends
    qusr
