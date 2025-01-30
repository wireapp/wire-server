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

module Galley.API.MLS.Removal
  ( createAndSendRemoveProposals,
    removeExtraneousClients,
    removeClient,
    RemoveUserIncludeMain (..),
    removeUser,
  )
where

import Control.Monad.Codensity
import Data.Bifunctor
import Data.Id
import Data.Map qualified as Map
import Data.Proxy
import Data.Qualified
import Data.Set qualified as Set
import Data.Time
import Galley.API.MLS.Conversation
import Galley.API.MLS.Keys
import Galley.API.MLS.Propagate
import Galley.API.MLS.Types
import Galley.Data.Conversation.Types
import Galley.Data.Conversation.Types qualified as Data
import Galley.Effects
import Galley.Effects.MemberStore
import Galley.Effects.ProposalStore
import Galley.Effects.SubConversationStore
import Galley.Env
import Galley.Types.Conversations.Members
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import System.Logger qualified as Log
import Wire.API.Conversation.Protocol
import Wire.API.Federation.Error
import Wire.API.MLS.AuthenticatedContent
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation
import Wire.API.MLS.SubConversation
import Wire.NotificationSubsystem
import Wire.Sem.Random

-- | Send remove proposals for a set of clients to clients in the ClientMap.
createAndSendRemoveProposals ::
  forall r t.
  ( Member (Error FederationError) r,
    Member (Input UTCTime) r,
    Member TinyLog r,
    Member BackendNotificationQueueAccess r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member ProposalStore r,
    Member (Input Env) r,
    Member Random r,
    Traversable t
  ) =>
  Local ConvOrSubConv ->
  t LeafIndex ->
  Qualified UserId ->
  -- | The client map that has all the recipients of the message. This is an
  -- argument, and not constructed within the function, because of a special
  -- case of subconversations where everyone but the subconversation leaver
  -- client should get the remove proposal message; in this case the recipients
  -- are a strict subset of all the clients represented by the in-memory
  -- conversation/subconversation client maps.
  ClientMap ->
  Codensity (Sem r) ()
createAndSendRemoveProposals lConvOrSubConv indices qusr cm = Codensity $ \k -> do
  let meta = (tUnqualified lConvOrSubConv).mlsMeta
  case cnvmlsActiveData meta of
    Nothing -> k ()
    Just activeData -> do
      let cs = activeData.ciphersuite
      mKeyPair <- getMLSRemovalKey (csSignatureScheme cs)
      case mKeyPair of
        Nothing -> do
          warn $ Log.msg ("No backend removal key is configured (See 'mlsPrivateKeyPaths' in galley's config). Not able to remove client from MLS conversation." :: Text)
          k ()
        Just (SomeKeyPair (_ :: Proxy ss) kp) -> do
          msgs <- for indices $ \idx -> do
            let proposal = mkRawMLS (RemoveProposal idx)
            pmsg <-
              liftRandom $
                mkSignedPublicMessage @ss
                  kp
                  (cnvmlsGroupId meta)
                  (cnvmlsEpoch meta)
                  (TaggedSenderExternal 0)
                  (FramedContentProposal proposal)
            let msg = mkRawMLS (mkMessage (MessagePublic pmsg))
            storeProposal
              (cnvmlsGroupId meta)
              (cnvmlsEpoch meta)
              (publicMessageRef cs pmsg)
              ProposalOriginBackend
              proposal
            pure msg
          x <- k ()
          for_ msgs $ flip (propagateMessage qusr Nothing lConvOrSubConv Nothing) cm
          pure x

removeClientsWithClientMapRecursively ::
  ( Member (Error FederationError) r,
    Member (Input UTCTime) r,
    Member TinyLog r,
    Member BackendNotificationQueueAccess r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member MemberStore r,
    Member ProposalStore r,
    Member SubConversationStore r,
    Member (Input Env) r,
    Member Random r,
    Traversable f
  ) =>
  Local MLSConversation ->
  -- | A function returning the "list" of clients to be removed from either the
  -- main conversation or each of its subconversations.
  (ConvOrSubConv -> f (ClientIdentity, LeafIndex)) ->
  -- | Originating user. The resulting proposals will appear to be sent by this user.
  Qualified UserId ->
  Sem r ()
removeClientsWithClientMapRecursively lMlsConv getClients qusr = do
  let mainConv = fmap Conv lMlsConv
      cm = mcMembers (tUnqualified lMlsConv)
  do
    let gid = cnvmlsGroupId . mcMLSData . tUnqualified $ lMlsConv
        clients = getClients (tUnqualified mainConv)

    planClientRemoval gid (fmap fst clients)
    lowerCodensity $ createAndSendRemoveProposals mainConv (fmap snd clients) qusr cm

  removeClientsFromSubConvs lMlsConv getClients qusr

removeClientsFromSubConvs ::
  ( Member (Error FederationError) r,
    Member (Input UTCTime) r,
    Member TinyLog r,
    Member BackendNotificationQueueAccess r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member MemberStore r,
    Member ProposalStore r,
    Member SubConversationStore r,
    Member (Input Env) r,
    Member Random r,
    Traversable f
  ) =>
  Local MLSConversation ->
  -- | A function returning the "list" of clients to be removed from either the
  -- main conversation or each of its subconversations.
  (ConvOrSubConv -> f (ClientIdentity, LeafIndex)) ->
  -- | Originating user. The resulting proposals will appear to be sent by this user.
  Qualified UserId ->
  Sem r ()
removeClientsFromSubConvs lMlsConv getClients qusr = do
  let cm = mcMembers (tUnqualified lMlsConv)

  -- remove this client from all subconversations
  subs <- listSubConversations' (mcId (tUnqualified lMlsConv))
  for_ subs $ \sub -> do
    let subConv = fmap (flip SubConv sub) lMlsConv
        sgid = cnvmlsGroupId . scMLSData $ sub
        clients = getClients (tUnqualified subConv)

    planClientRemoval sgid (fmap fst clients)
    lowerCodensity $
      createAndSendRemoveProposals
        subConv
        (fmap snd clients)
        qusr
        cm

-- | Send remove proposals for a single client of a user to the local conversation.
removeClient ::
  ( Member BackendNotificationQueueAccess r,
    Member (Error FederationError) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member ProposalStore r,
    Member Random r,
    Member SubConversationStore r,
    Member TinyLog r
  ) =>
  Local Data.Conversation ->
  Qualified UserId ->
  ClientId ->
  Sem r ()
removeClient lc qusr c = do
  mMlsConv <- mkMLSConversation (tUnqualified lc)
  for_ mMlsConv $ \mlsConv -> do
    let cid = mkClientIdentity qusr c
    let getClients = fmap (cid,) . cmLookupIndex cid . (.members)
    removeClientsWithClientMapRecursively (qualifyAs lc mlsConv) getClients qusr

-- | A flag to determine whether 'removeUser' should operate on the parent
-- conversation as well as all the subconversations.
data RemoveUserIncludeMain
  = -- | Remove user clients from all subconversations, including the parent.
    RemoveUserIncludeMain
  | -- | Remove user clients from all subconversations, but not the parent.
    --
    -- This can be used when the clients are already in the process of being
    -- removed from the main conversation, for example as a result of a commit
    -- containing a remove proposal.
    RemoveUserExcludeMain

-- | Send remove proposals for all clients of the user to the local conversation.
removeUser ::
  ( Member BackendNotificationQueueAccess r,
    Member (Error FederationError) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member ProposalStore r,
    Member Random r,
    Member SubConversationStore r,
    Member TinyLog r
  ) =>
  Local Data.Conversation ->
  RemoveUserIncludeMain ->
  Qualified UserId ->
  Sem r ()
removeUser lc includeMain qusr = do
  mMlsConv <- mkMLSConversation (tUnqualified lc)
  for_ mMlsConv $ \mlsConv -> do
    let getClients :: ConvOrSubConv -> [(ClientIdentity, LeafIndex)]
        getClients =
          map (first (mkClientIdentity qusr))
            . Map.assocs
            . Map.findWithDefault mempty qusr
            . (.members)
    case includeMain of
      RemoveUserIncludeMain ->
        removeClientsWithClientMapRecursively
          (qualifyAs lc mlsConv)
          getClients
          qusr
      RemoveUserExcludeMain ->
        removeClientsFromSubConvs (qualifyAs lc mlsConv) getClients qusr

-- | Convert cassandra subconv maps into SubConversations
listSubConversations' ::
  (Member SubConversationStore r) =>
  ConvId ->
  Sem r [SubConversation]
listSubConversations' cid = do
  subs <- listSubConversations cid
  msubs <- for (Map.assocs subs) $ \(subId, _) -> do
    getSubConversation cid subId
  pure (catMaybes msubs)

-- | Send remove proposals for clients of users that are not part of a conversation
removeExtraneousClients ::
  ( Member BackendNotificationQueueAccess r,
    Member (Error FederationError) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member ProposalStore r,
    Member Random r,
    Member SubConversationStore r,
    Member TinyLog r
  ) =>
  Qualified UserId ->
  Local Conversation ->
  Sem r ()
removeExtraneousClients qusr lconv = do
  mMlsConv <- mkMLSConversation (tUnqualified lconv)
  for_ mMlsConv $ \mlsConv -> do
    let allMembers =
          Set.fromList $
            map (tUntagged . qualifyAs lconv . lmId) (mcLocalMembers mlsConv)
              <> map (tUntagged . rmId) (mcRemoteMembers mlsConv)
    let getClients c =
          filter
            (\(cid, _) -> cidQualifiedUser cid `Set.notMember` allMembers)
            (cmAssocs c.members)
    removeClientsWithClientMapRecursively (qualifyAs lconv mlsConv) getClients qusr
