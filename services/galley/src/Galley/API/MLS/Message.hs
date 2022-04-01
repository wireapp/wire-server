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

module Galley.API.MLS.Message where

import Data.Id
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Set as Set
import Data.Time
import Galley.API.Action
import Galley.API.Error
import Galley.API.MLS.KeyPackage
import Galley.API.Util
import Galley.Data.Conversation.Types
import Galley.Effects.BrigAccess
import Galley.Effects.ConversationStore
import Galley.Effects.ExternalAccess
import Galley.Effects.FederatorAccess
import Galley.Effects.GundeckAccess
import Galley.Effects.LegalHoldStore
import Galley.Effects.MemberStore
import Galley.Effects.TeamStore
import Galley.Options
import Galley.Types.Conversations.Members
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.Error
import Wire.API.MLS.Commit
import Wire.API.MLS.Credential
import Wire.API.MLS.Group
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation

postMLSMessage ::
  ( HasProposalEffects r,
    Members
      '[ Error FederationError,
         ErrorS 'ConvNotFound,
         ErrorS 'UnsupportedMLSMessage,
         ErrorS 'ProposalNotFound
       ]
      r
  ) =>
  Local UserId ->
  ConnId ->
  RawMLS SomeMessage ->
  Sem r [Event]
postMLSMessage lusr con smsg = case rmValue smsg of
  SomeMessage SMLSPlainText msg -> case msgTBS (msgPayload msg) of
    CommitMessage c ->
      processCommit lusr con (rmRaw smsg) (msgGroupId msg) c
    ApplicationMessage _ -> throwS @'UnsupportedMLSMessage
    _ -> pure mempty -- FUTUREWORK: handle other message types
  _ -> pure mempty -- FUTUREWORK: handle encrypted messages

type HasProposalEffects r =
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member (Error MLSProtocolError) r,
    Member (Error ProposalFailure) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'KeyPackageRefNotFound) r,
    Member (ErrorS 'MLSClientMismatch) r,
    Member (ErrorS 'UnsupportedProposal) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member MemberStore r,
    Member TeamStore r
  )

type ClientMap = Map (Qualified UserId) (Set ClientId)

data ProposalAction = ProposalAction
  { paAdd :: ClientMap
  }

instance Semigroup ProposalAction where
  ProposalAction add1 <> ProposalAction add2 =
    ProposalAction $
      Map.unionWith mappend add1 add2

instance Monoid ProposalAction where
  mempty = ProposalAction mempty

paClient :: Qualified (UserId, ClientId) -> ProposalAction
paClient quc = mempty {paAdd = Map.singleton (fmap fst quc) (Set.singleton (snd (qUnqualified quc)))}

processCommit ::
  ( HasProposalEffects r,
    Member (ErrorS 'ProposalNotFound) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (Error FederationError) r
  ) =>
  Local UserId ->
  ConnId ->
  ByteString ->
  GroupId ->
  Commit ->
  Sem r [Event]
processCommit lusr con _raw gid commit = do
  qcnv <- getConversationByGroupId gid >>= noteS @'ConvNotFound
  lcnv <- ensureLocal lusr qcnv
  conv <- getConversation (tUnqualified lcnv) >>= noteS @'ConvNotFound
  action <- foldMap applyProposalRef (cProposals commit)
  executeProposalAction lusr con conv action

applyProposalRef ::
  ( HasProposalEffects r,
    Member (ErrorS 'ProposalNotFound) r
  ) =>
  ProposalOrRef ->
  Sem r ProposalAction
applyProposalRef (Ref _) = throwS @'ProposalNotFound
applyProposalRef (Inline p) = applyProposal p

applyProposal :: HasProposalEffects r => Proposal -> Sem r ProposalAction
applyProposal (AddProposal kp) = do
  ref <-
    kpRef' kp
      & note (mlsProtocolError "Could not compute ref of a key package in an Add proposal")
  qclient <- cidQualifiedClient <$> derefKeyPackage ref
  pure (paClient qclient)
applyProposal _ = throwS @'UnsupportedProposal

executeProposalAction ::
  forall r.
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (Error FederationError) r,
    Member (ErrorS 'MLSClientMismatch) r,
    Member (Error ProposalFailure) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member MemberStore r,
    Member TeamStore r
  ) =>
  Local UserId ->
  ConnId ->
  Conversation ->
  ProposalAction ->
  Sem r [Event]
executeProposalAction lusr con conv action = do
  let cm = convClientMap lusr conv
      newUserClients = Map.assocs (paAdd action)
  -- check that all clients of each user are added to the conversation, and
  -- update the database accordingly
  traverse_ (uncurry (addUserClients cm)) newUserClients
  -- add users to the conversation and send events
  -- TODO: only add new members
  result <- foldMap addMembers . nonEmpty . map fst $ newUserClients
  pure result
  where
    addUserClients :: ClientMap -> Qualified UserId -> Set ClientId -> Sem r ()
    addUserClients cm qtarget newClients = do
      -- compute final set of clients in the conversation
      let cs = newClients <> Map.findWithDefault mempty qtarget cm
      -- get list of mls clients from brig
      allClients <- getMLSClients qtarget
      -- if not all clients have been added to the conversation, return an error
      when (cs /= allClients) $ do
        -- FUTUREWORK: turn this error into a proper response
        throwS @'MLSClientMismatch
      -- add clients to the database
      -- TODO: delay this until after adding them as members to the conversation
      ltarget <- ensureLocal lusr qtarget -- FUTUREWORK: support remote users
      addMLSClients (convId conv) (tUnqualified ltarget) newClients

    addMembers :: NonEmpty (Qualified UserId) -> Sem r [Event]
    addMembers users =
      -- FUTUREWORK: update key package ref mapping to reflect conversation membership
      handleNoChanges
        . handleProposalFailures @ProposalErrors
        . fmap pure
        . updateLocalConversationWithLocalUser @'ConversationJoinTag (qualifyAs lusr (convId conv)) lusr (Just con)
        $ ConversationJoin users roleNameWireMember

handleNoChanges :: Monoid a => Sem (Error NoChanges ': r) a -> Sem r a
handleNoChanges = fmap fold . runError

convClientMap :: Local x -> Conversation -> ClientMap
convClientMap loc =
  mconcat
    [ foldMap localMember . convLocalMembers,
      mempty -- FUTUREWORK: add mls clients of remote members
    ]
  where
    localMember lm = Map.singleton (qUntagged (qualifyAs loc (lmId lm))) (lmMLSClients lm)

--------------------------------------------------------------------------------
-- Error handling of proposal execution

-- The following errors are caught by 'executeProposalAction' and wrapped in a
-- 'ProposalFailure'. This way errors caused by the execution of proposals are
-- separated from those caused by the commit processing itself.
type ProposalErrors =
  '[ Error FederationError,
     Error InvalidInput,
     Error LegalHoldError,
     ErrorS ('ActionDenied 'AddConversationMember),
     ErrorS ('ActionDenied 'LeaveConversation),
     ErrorS 'ConvAccessDenied,
     ErrorS 'InvalidOperation,
     ErrorS 'NotATeamMember,
     ErrorS 'NotConnected,
     ErrorS 'TooManyMembers
   ]

class HandleProposalFailures effs r where
  handleProposalFailures :: Sem (Append effs r) a -> Sem r a

class HandleProposalFailure eff r where
  handleProposalFailure :: Sem (eff ': r) a -> Sem r a

instance HandleProposalFailures '[] r where
  handleProposalFailures = id

instance
  ( HandleProposalFailures effs r,
    HandleProposalFailure eff (Append effs r)
  ) =>
  HandleProposalFailures (eff ': effs) r
  where
  handleProposalFailures = handleProposalFailures @effs . handleProposalFailure @eff

instance
  (APIError e, Member (Error ProposalFailure) r) =>
  HandleProposalFailure (Error e) r
  where
  handleProposalFailure = mapError (ProposalFailure . toWai)
