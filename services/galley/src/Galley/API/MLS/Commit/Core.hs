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

module Galley.API.MLS.Commit.Core
  ( getCommitData,
    incrementEpoch,
    getClientInfo,
    HasProposalActionEffects,
    ProposalErrors,
    HandleMLSProposalFailures (..),
  )
where

import Control.Comonad
import Data.Id
import Data.Qualified
import Data.Time
import Galley.API.Error
import Galley.API.MLS.Conversation
import Galley.API.MLS.IncomingMessage
import Galley.API.MLS.Proposal
import Galley.API.MLS.Types
import Galley.Effects
import Galley.Effects.BrigAccess
import Galley.Effects.ConversationStore
import Galley.Effects.FederatorAccess
import Galley.Effects.SubConversationStore
import Galley.Env
import Galley.Options
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal
import Polysemy.State
import Polysemy.TinyLog
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig
import Wire.API.Federation.Endpoint
import Wire.API.Federation.Error
import Wire.API.Federation.Version
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Commit
import Wire.API.MLS.Credential
import Wire.API.MLS.Serialisation
import Wire.API.MLS.SubConversation
import Wire.API.User.Client
import Wire.NotificationSubsystem

type HasProposalActionEffects r =
  ( Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member (Error InternalError) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MLSClientMismatch) r,
    Member (Error MLSProposalFailure) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'MLSUnsupportedProposal) r,
    Member (Error MLSProtocolError) r,
    Member (Error NonFederatingBackends) r,
    Member (Error UnreachableBackends) r,
    Member (ErrorS 'MLSSelfRemovalNotAllowed) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member (Input Env) r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member MemberStore r,
    Member ProposalStore r,
    Member SubConversationStore r,
    Member TeamStore r,
    Member TinyLog r,
    Member NotificationSubsystem r,
    Member Random r
  )

getCommitData ::
  ( HasProposalEffects r,
    Member (ErrorS 'MLSProposalNotFound) r
  ) =>
  ClientIdentity ->
  Local ConvOrSubConv ->
  Epoch ->
  CipherSuiteTag ->
  IncomingBundle ->
  Sem r ProposalAction
getCommitData senderIdentity lConvOrSub epoch ciphersuite bundle = do
  let convOrSub = tUnqualified lConvOrSub
      groupId = cnvmlsGroupId convOrSub.mlsMeta

  evalState convOrSub.indexMap $ do
    creatorAction <-
      if epoch == Epoch 0
        then addProposedClient senderIdentity
        else mempty
    proposals <-
      traverse
        (derefOrCheckProposal epoch ciphersuite groupId)
        bundle.commit.value.proposals
    action <- applyProposals ciphersuite groupId proposals
    pure (creatorAction <> action)

incrementEpoch ::
  ( Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member MemberStore r,
    Member SubConversationStore r
  ) =>
  ConvOrSubConv ->
  Sem r ConvOrSubConv
incrementEpoch (Conv c) = do
  let epoch' = succ (cnvmlsEpoch (mcMLSData c))
  setConversationEpoch (mcId c) epoch'
  conv <- getConversation (mcId c) >>= noteS @'ConvNotFound
  fmap Conv (mkMLSConversation conv >>= noteS @'ConvNotFound)
incrementEpoch (SubConv c s) = do
  let epoch' = succ (cnvmlsEpoch (scMLSData s))
  setSubConversationEpoch (scParentConvId s) (scSubConvId s) epoch'
  subconv <-
    getSubConversation (mcId c) (scSubConvId s) >>= noteS @'ConvNotFound
  pure (SubConv c subconv)

getClientInfo ::
  ( Member BrigAccess r,
    Member FederatorAccess r
  ) =>
  Local x ->
  Qualified UserId ->
  CipherSuiteTag ->
  Sem r (Either FederationError (Set ClientInfo))
getClientInfo loc =
  foldQualified loc (\lusr -> fmap Right . getLocalMLSClients lusr) getRemoteMLSClients

getRemoteMLSClients ::
  ( Member FederatorAccess r
  ) =>
  Remote UserId ->
  CipherSuiteTag ->
  Sem r (Either FederationError (Set ClientInfo))
getRemoteMLSClients rusr suite = do
  let mcr =
        MLSClientsRequest
          { userId = tUnqualified rusr,
            cipherSuite = tagCipherSuite suite
          }
  runFederatedEither rusr $ \_version ->
    fedClient @'Brig @"get-mls-clients" mcr
      <|> fedClient @'Brig @(Versioned 'V0 "get-mls-clients") (mlsClientsRequestToV0 mcr)

--------------------------------------------------------------------------------
-- Error handling of proposal execution

-- The following errors are caught by 'executeProposalAction' and wrapped in a
-- 'MLSProposalFailure'. This way errors caused by the execution of proposals are
-- separated from those caused by the commit processing itself.
type ProposalErrors =
  '[ Error FederationError,
     Error InvalidInput,
     ErrorS ('ActionDenied 'AddConversationMember),
     ErrorS ('ActionDenied 'LeaveConversation),
     ErrorS ('ActionDenied 'RemoveConversationMember),
     ErrorS 'ConvAccessDenied,
     ErrorS 'InvalidOperation,
     ErrorS 'NotATeamMember,
     ErrorS 'NotConnected,
     ErrorS 'TooManyMembers
   ]

class HandleMLSProposalFailures effs r where
  handleMLSProposalFailures :: Sem (Append effs r) a -> Sem r a

class HandleMLSProposalFailure eff r where
  handleMLSProposalFailure :: Sem (eff ': r) a -> Sem r a

instance HandleMLSProposalFailures '[] r where
  handleMLSProposalFailures = id

instance
  ( HandleMLSProposalFailures effs r,
    HandleMLSProposalFailure eff (Append effs r)
  ) =>
  HandleMLSProposalFailures (eff ': effs) r
  where
  handleMLSProposalFailures = handleMLSProposalFailures @effs . handleMLSProposalFailure @eff

instance
  (APIError e, Member (Error MLSProposalFailure) r) =>
  HandleMLSProposalFailure (Error e) r
  where
  handleMLSProposalFailure = mapError (MLSProposalFailure . toResponse)
