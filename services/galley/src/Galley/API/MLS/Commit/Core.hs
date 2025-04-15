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
    getSingleClientInfo,
    checkSignatureKey,
    checkUpdatePath,
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
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Serialisation
import Wire.API.MLS.SubConversation
import Wire.API.MLS.Validation
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
  SenderIdentity ->
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
        then addProposedClient (Left senderIdentity.client)
        else mempty
    proposals <-
      traverse
        (derefOrCheckProposal epoch ciphersuite groupId)
        bundle.commit.value.proposals
    action <- applyProposals ciphersuite proposals
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
    Member FederatorAccess r,
    Member (Error FederationError) r
  ) =>
  Local x ->
  Qualified UserId ->
  CipherSuiteTag ->
  Sem r (Set ClientInfo)
getClientInfo loc = foldQualified loc getLocalMLSClients getRemoteMLSClients

getRemoteMLSClients ::
  ( Member FederatorAccess r,
    Member (Error FederationError) r
  ) =>
  Remote UserId ->
  CipherSuiteTag ->
  Sem r (Set ClientInfo)
getRemoteMLSClients rusr suite = do
  let mcr =
        MLSClientsRequest
          { userId = tUnqualified rusr,
            cipherSuite = tagCipherSuite suite
          }
  (either throw pure <=< runFederatedEither rusr) $
    fedClient @'Brig @"get-mls-clients" mcr
      <|> fedClient @'Brig @(Versioned 'V0 "get-mls-clients") (mlsClientsRequestToV0 mcr)

getSingleClientInfo ::
  ( Member BrigAccess r,
    Member FederatorAccess r,
    Member (Error FederationError) r
  ) =>
  Local x ->
  Qualified UserId ->
  ClientId ->
  CipherSuiteTag ->
  Sem r ClientInfo
getSingleClientInfo loc = foldQualified loc getLocalMLSClient getRemoteMLSClient

getRemoteMLSClient ::
  ( Member FederatorAccess r,
    Member (Error FederationError) r
  ) =>
  Remote UserId ->
  ClientId ->
  CipherSuiteTag ->
  Sem r ClientInfo
getRemoteMLSClient rusr cid suite = do
  let mcr =
        MLSClientsRequest
          { userId = tUnqualified rusr,
            cipherSuite = tagCipherSuite suite
          }
      mcr1 =
        MLSClientRequest
          { userId = tUnqualified rusr,
            clientId = cid,
            cipherSuite = tagCipherSuite suite
          }
      extractClient :: Set ClientInfo -> ClientInfo
      extractClient infos = case filter ((== cid) . (.clientId)) (toList infos) of
        (x : _) -> x
        _ ->
          ClientInfo
            { clientId = cid,
              hasKeyPackages = False,
              mlsSignatureKey = Nothing
            }
  -- get single client if the API is available, otherwise get all clients and find the correct one
  (either throw pure <=< runFederatedEither rusr) $
    fedClient @'Brig @"get-mls-client" mcr1
      <|> fmap extractClient (fedClient @'Brig @"get-mls-clients" mcr)
      <|> fmap extractClient (fedClient @'Brig @(Versioned 'V0 "get-mls-clients") (mlsClientsRequestToV0 mcr))

checkSignatureKey ::
  (Member (ErrorS MLSIdentityMismatch) r) =>
  Maybe LeafNode ->
  Maybe ByteString ->
  Sem r ()
checkSignatureKey mLeaf mKey =
  when
    ( case mLeaf of
        Just leaf -> case mKey of
          -- if the key could not be obtained (e.g.
          -- because an older version of the brig
          -- or federation endpoint has been used),
          -- skip this check
          Nothing -> False
          key -> key /= Just leaf.signatureKey
        Nothing -> False
    )
    $ throwS @'MLSIdentityMismatch

-- | Check that the leaf node in an update path, if present, has the correct signature key.
checkUpdatePath ::
  ( Member (ErrorS MLSIdentityMismatch) r,
    Member (Error MLSProtocolError) r,
    Member (Error FederationError) r,
    Member BrigAccess r,
    Member FederatorAccess r
  ) =>
  Local ConvOrSubConv ->
  SenderIdentity ->
  CipherSuiteTag ->
  UpdatePath ->
  Sem r ()
checkUpdatePath lConvOrSub senderIdentity ciphersuite path = for_ senderIdentity.index $ \index -> do
  let groupId = cnvmlsGroupId (tUnqualified lConvOrSub).mlsMeta
  let extra = LeafNodeTBSExtraCommit groupId index
  case validateLeafNode ciphersuite (Just senderIdentity.client) extra path.leaf.value of
    Left errMsg ->
      throw $
        mlsProtocolError ("Tried to add invalid LeafNode: " <> errMsg)
    Right _ -> pure ()
  clientInfo <-
    getSingleClientInfo
      lConvOrSub
      (cidQualifiedUser senderIdentity.client)
      senderIdentity.client.ciClient
      ciphersuite
  checkSignatureKey (Just path.leaf.value) clientInfo.mlsSignatureKey

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
