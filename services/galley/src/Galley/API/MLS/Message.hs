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

module Galley.API.MLS.Message
  ( IncomingBundle (..),
    mkIncomingBundle,
    IncomingMessage (..),
    mkIncomingMessage,
    postMLSCommitBundle,
    postMLSCommitBundleFromLocalUser,
    postMLSMessageFromLocalUser,
    postMLSMessage,
    MLSMessageStaticErrors,
    MLSBundleStaticErrors,
  )
where

import Control.Comonad
import Data.Domain
import Data.Id
import Data.Json.Util
import qualified Data.List.NonEmpty as NE
import Data.Qualified
import qualified Data.Set as Set
import qualified Data.Text.Lazy as LT
import Data.Tuple.Extra
import Galley.API.Action
import Galley.API.Error
import Galley.API.MLS.Commit
import Galley.API.MLS.Conversation
import Galley.API.MLS.Enabled
import Galley.API.MLS.IncomingMessage
import Galley.API.MLS.Propagate
import Galley.API.MLS.Proposal
import Galley.API.MLS.Types
import Galley.API.MLS.Util
import Galley.API.MLS.Welcome (sendWelcomes)
import Galley.API.Util
import Galley.Data.Conversation.Types
import Galley.Effects
import Galley.Effects.ConversationStore
import Galley.Effects.FederatorAccess
import Galley.Effects.MemberStore
import Galley.Effects.SubConversationStore
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal
import Polysemy.Resource (Resource)
import Polysemy.TinyLog
import Wire.API.Conversation.Protocol
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.MLS.Commit
import Wire.API.MLS.CommitBundle
import Wire.API.MLS.Credential
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.Message
import Wire.API.MLS.Serialisation
import Wire.API.MLS.SubConversation
import Wire.API.Unreachable

-- FUTUREWORK
-- - Check that the capabilities of a leaf node in an add proposal contains all
--   the required_capabilities of the group context. This would require fetching
--   the group info from the DB in order to read the group context.
-- - Verify message signature, this also requires the group context. (see above)

type MLSMessageStaticErrors =
  '[ ErrorS 'ConvAccessDenied,
     ErrorS 'ConvMemberNotFound,
     ErrorS 'ConvNotFound,
     ErrorS 'MLSNotEnabled,
     ErrorS 'MLSUnsupportedMessage,
     ErrorS 'MLSStaleMessage,
     ErrorS 'MLSProposalNotFound,
     ErrorS 'MissingLegalholdConsent,
     ErrorS 'MLSInvalidLeafNodeIndex,
     ErrorS 'MLSClientMismatch,
     ErrorS 'MLSUnsupportedProposal,
     ErrorS 'MLSCommitMissingReferences,
     ErrorS 'MLSSelfRemovalNotAllowed,
     ErrorS 'MLSClientSenderUserMismatch,
     ErrorS 'MLSGroupConversationMismatch,
     ErrorS 'MLSSubConvClientNotInParent
   ]

type MLSBundleStaticErrors =
  Append
    MLSMessageStaticErrors
    '[ErrorS 'MLSWelcomeMismatch]

postMLSMessageFromLocalUser ::
  ( HasProposalEffects r,
    Member (Error FederationError) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'ConvMemberNotFound) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'MLSClientSenderUserMismatch) r,
    Member (ErrorS 'MLSCommitMissingReferences) r,
    Member (ErrorS 'MLSGroupConversationMismatch) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (ErrorS 'MLSProposalNotFound) r,
    Member (ErrorS 'MLSSelfRemovalNotAllowed) r,
    Member (ErrorS 'MLSStaleMessage) r,
    Member (ErrorS 'MLSUnsupportedMessage) r,
    Member (ErrorS 'MLSSubConvClientNotInParent) r,
    Member SubConversationStore r
  ) =>
  Local UserId ->
  ClientId ->
  ConnId ->
  RawMLS Message ->
  Sem r MLSMessageSendingStatus
postMLSMessageFromLocalUser lusr c conn smsg = do
  assertMLSEnabled
  imsg <- noteS @'MLSUnsupportedMessage $ mkIncomingMessage smsg
  cnvOrSub <- lookupConvByGroupId imsg.groupId >>= noteS @'ConvNotFound
  (events, unreachables) <-
    first (map lcuEvent)
      <$> postMLSMessage lusr (tUntagged lusr) c cnvOrSub (Just conn) imsg
  t <- toUTCTimeMillis <$> input
  pure $ MLSMessageSendingStatus events t unreachables

postMLSCommitBundle ::
  ( HasProposalEffects r,
    Members MLSBundleStaticErrors r,
    Member (Error FederationError) r,
    Member Resource r,
    Member SubConversationStore r
  ) =>
  Local x ->
  Qualified UserId ->
  ClientId ->
  Qualified ConvOrSubConvId ->
  Maybe ConnId ->
  IncomingBundle ->
  Sem r [LocalConversationUpdate]
postMLSCommitBundle loc qusr c qConvOrSub conn bundle =
  foldQualified
    loc
    (postMLSCommitBundleToLocalConv qusr c conn bundle)
    (postMLSCommitBundleToRemoteConv loc qusr c conn bundle)
    qConvOrSub

postMLSCommitBundleFromLocalUser ::
  ( HasProposalEffects r,
    Members MLSBundleStaticErrors r,
    Member (Error FederationError) r,
    Member Resource r,
    Member SubConversationStore r
  ) =>
  Local UserId ->
  ClientId ->
  ConnId ->
  RawMLS CommitBundle ->
  Sem r MLSMessageSendingStatus
postMLSCommitBundleFromLocalUser lusr c conn bundle = do
  assertMLSEnabled
  ibundle <- noteS @'MLSUnsupportedMessage $ mkIncomingBundle bundle
  qConvOrSub <- lookupConvByGroupId ibundle.groupId >>= noteS @'ConvNotFound
  events <-
    map lcuEvent
      <$> postMLSCommitBundle lusr (tUntagged lusr) c qConvOrSub (Just conn) ibundle
  t <- toUTCTimeMillis <$> input
  pure $ MLSMessageSendingStatus events t mempty

postMLSCommitBundleToLocalConv ::
  ( HasProposalEffects r,
    Member (Error FederationError) r,
    Members MLSBundleStaticErrors r,
    Member Resource r,
    Member SubConversationStore r
  ) =>
  Qualified UserId ->
  ClientId ->
  Maybe ConnId ->
  IncomingBundle ->
  Local ConvOrSubConvId ->
  Sem r [LocalConversationUpdate]
postMLSCommitBundleToLocalConv qusr c conn bundle lConvOrSubId = do
  lConvOrSub <- fetchConvOrSub qusr lConvOrSubId
  senderIdentity <- getSenderIdentity qusr c bundle.sender lConvOrSub

  (events, newClients) <- case bundle.sender of
    SenderMember _index -> do
      action <- getCommitData senderIdentity lConvOrSub bundle.epoch bundle.commit.value
      events <-
        processInternalCommit
          senderIdentity
          conn
          lConvOrSub
          bundle.epoch
          action
          bundle.commit.value
      pure (events, cmIdentities (paAdd action))
    SenderExternal _ -> throw (mlsProtocolError "Unexpected sender")
    SenderNewMemberProposal -> throw (mlsProtocolError "Unexpected sender")
    SenderNewMemberCommit -> do
      action <- getExternalCommitData senderIdentity lConvOrSub bundle.epoch bundle.commit.value
      processExternalCommit
        senderIdentity
        lConvOrSub
        bundle.epoch
        action
        bundle.commit.value.path
      pure ([], [])

  storeGroupInfo (tUnqualified lConvOrSub).id bundle.groupInfo

  propagateMessage qusr lConvOrSub conn bundle.rawMessage (tUnqualified lConvOrSub).members
    >>= mapM_ throwUnreachableUsers

  traverse_ (sendWelcomes lConvOrSub conn newClients) bundle.welcome
  pure events

postMLSCommitBundleToRemoteConv ::
  ( Members MLSBundleStaticErrors r,
    ( Member BrigAccess r,
      Member (Error FederationError) r,
      Member (Error InternalError) r,
      Member (Error MLSProtocolError) r,
      Member (Error MLSProposalFailure) r,
      Member ExternalAccess r,
      Member FederatorAccess r,
      Member GundeckAccess r,
      Member MemberStore r,
      Member TinyLog r
    )
  ) =>
  Local x ->
  Qualified UserId ->
  ClientId ->
  Maybe ConnId ->
  IncomingBundle ->
  Remote ConvOrSubConvId ->
  Sem r [LocalConversationUpdate]
postMLSCommitBundleToRemoteConv loc qusr c con bundle rConvOrSubId = do
  -- only local users can send messages to remote conversations
  lusr <- foldQualified loc pure (\_ -> throwS @'ConvAccessDenied) qusr
  -- only members may send commit bundles to a remote conversation

  flip unless (throwS @'ConvMemberNotFound) =<< checkLocalMemberRemoteConv (tUnqualified lusr) ((.conv) <$> rConvOrSubId)

  resp <-
    runFederated rConvOrSubId $
      fedClient @'Galley @"send-mls-commit-bundle" $
        MLSMessageSendRequest
          { mmsrConvOrSubId = tUnqualified rConvOrSubId,
            mmsrSender = tUnqualified lusr,
            mmsrSenderClient = c,
            mmsrRawMessage = Base64ByteString bundle.serialized
          }
  case resp of
    MLSMessageResponseError e -> rethrowErrors @MLSBundleStaticErrors e
    MLSMessageResponseProtocolError e -> throw (mlsProtocolError e)
    MLSMessageResponseProposalFailure e -> throw (MLSProposalFailure e)
    MLSMessageResponseUnreachableBackends ds -> throwUnreachableDomains ds
    MLSMessageResponseUpdates updates unreachables -> do
      for_ unreachables $ \us ->
        throw . InternalErrorWithDescription $
          "A commit to a remote conversation should not ever return a \
          \non-empty list of users an application message could not be \
          \sent to. The remote end returned: "
            <> LT.pack (intercalate ", " (show <$> NE.toList (unreachableUsers us)))
      for updates $ \update -> do
        e <- notifyRemoteConversationAction loc (qualifyAs rConvOrSubId update) con
        pure (LocalConversationUpdate e update)

postMLSMessage ::
  ( HasProposalEffects r,
    Member (Error FederationError) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'ConvMemberNotFound) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'MLSClientSenderUserMismatch) r,
    Member (ErrorS 'MLSCommitMissingReferences) r,
    Member (ErrorS 'MLSGroupConversationMismatch) r,
    Member (ErrorS 'MLSProposalNotFound) r,
    Member (ErrorS 'MLSSelfRemovalNotAllowed) r,
    Member (ErrorS 'MLSStaleMessage) r,
    Member (ErrorS 'MLSUnsupportedMessage) r,
    Member (ErrorS 'MLSSubConvClientNotInParent) r,
    Member SubConversationStore r
  ) =>
  Local x ->
  Qualified UserId ->
  ClientId ->
  Qualified ConvOrSubConvId ->
  Maybe ConnId ->
  IncomingMessage ->
  Sem r ([LocalConversationUpdate], Maybe UnreachableUsers)
postMLSMessage loc qusr c qconvOrSub con msg = do
  foldQualified
    loc
    (postMLSMessageToLocalConv qusr c con msg)
    (postMLSMessageToRemoteConv loc qusr c con msg)
    qconvOrSub

getSenderIdentity ::
  ( Member (ErrorS 'MLSClientSenderUserMismatch) r,
    Member (Error MLSProtocolError) r
  ) =>
  Qualified UserId ->
  ClientId ->
  Sender ->
  Local ConvOrSubConv ->
  Sem r ClientIdentity
getSenderIdentity qusr c mSender lConvOrSubConv = do
  let cid = mkClientIdentity qusr c
  let epoch = epochNumber . cnvmlsEpoch . (.meta) . tUnqualified $ lConvOrSubConv
  case mSender of
    SenderMember idx | epoch > 0 -> do
      cid' <- note (mlsProtocolError "unknown sender leaf index") $ imLookup (tUnqualified lConvOrSubConv).indexMap idx
      unless (cid' == cid) $ throwS @'MLSClientSenderUserMismatch
    _ -> pure ()
  pure cid

postMLSMessageToLocalConv ::
  ( HasProposalEffects r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MLSClientSenderUserMismatch) r,
    Member (ErrorS 'MLSStaleMessage) r,
    Member (ErrorS 'MLSUnsupportedMessage) r,
    Member SubConversationStore r
  ) =>
  Qualified UserId ->
  ClientId ->
  Maybe ConnId ->
  IncomingMessage ->
  Local ConvOrSubConvId ->
  Sem r ([LocalConversationUpdate], Maybe UnreachableUsers)
postMLSMessageToLocalConv qusr c con msg convOrSubId = do
  lConvOrSub <- fetchConvOrSub qusr convOrSubId

  for_ msg.sender $ \sender ->
    void $ getSenderIdentity qusr c sender lConvOrSub

  -- validate message
  case msg.content of
    IncomingMessageContentPublic pub -> case pub.content of
      FramedContentCommit _commit -> throwS @'MLSUnsupportedMessage
      FramedContentApplicationData _ -> throwS @'MLSUnsupportedMessage
      FramedContentProposal prop ->
        processProposal qusr lConvOrSub msg.groupId msg.epoch pub prop
    IncomingMessageContentPrivate -> do
      when ((tUnqualified lConvOrSub).migrationState == MLSMigrationMixed) $
        throwS @'MLSUnsupportedMessage

  unreachables <- propagateMessage qusr lConvOrSub con msg.rawMessage (tUnqualified lConvOrSub).members
  pure ([], unreachables)

postMLSMessageToRemoteConv ::
  ( Members MLSMessageStaticErrors r,
    ( Member (Error FederationError) r,
      Member TinyLog r
    ),
    HasProposalEffects r
  ) =>
  Local x ->
  Qualified UserId ->
  ClientId ->
  Maybe ConnId ->
  IncomingMessage ->
  Remote ConvOrSubConvId ->
  Sem r ([LocalConversationUpdate], Maybe UnreachableUsers)
postMLSMessageToRemoteConv loc qusr senderClient con msg rConvOrSubId = do
  -- only local users can send messages to remote conversations
  lusr <- foldQualified loc pure (\_ -> throwS @'ConvAccessDenied) qusr
  -- only members may send messages to the remote conversation
  flip unless (throwS @'ConvMemberNotFound) =<< checkLocalMemberRemoteConv (tUnqualified lusr) ((.conv) <$> rConvOrSubId)

  resp <-
    runFederated rConvOrSubId $
      fedClient @'Galley @"send-mls-message" $
        MLSMessageSendRequest
          { mmsrConvOrSubId = tUnqualified rConvOrSubId,
            mmsrSender = tUnqualified lusr,
            mmsrSenderClient = senderClient,
            mmsrRawMessage = Base64ByteString msg.rawMessage.raw
          }
  case resp of
    MLSMessageResponseError e -> rethrowErrors @MLSMessageStaticErrors e
    MLSMessageResponseProtocolError e ->
      throw (mlsProtocolError e)
    MLSMessageResponseProposalFailure e -> throw (MLSProposalFailure e)
    MLSMessageResponseUnreachableBackends ds ->
      throw . InternalErrorWithDescription $
        "An application or proposal message to a remote conversation should \
        \not ever return a non-empty list of domains a commit could not be \
        \sent to. The remote end returned: "
          <> LT.pack (intercalate ", " (show <$> Set.toList (Set.map domainText ds)))
    MLSMessageResponseUpdates updates unreachables -> do
      lcus <- for updates $ \update -> do
        e <- notifyRemoteConversationAction loc (qualifyAs rConvOrSubId update) con
        pure (LocalConversationUpdate e update)
      pure (lcus, unreachables)

storeGroupInfo ::
  ( Member ConversationStore r,
    Member SubConversationStore r
  ) =>
  ConvOrSubConvId ->
  GroupInfoData ->
  Sem r ()
storeGroupInfo convOrSub ginfo = case convOrSub of
  Conv cid -> setGroupInfo cid ginfo
  SubConv cid subconvid -> setSubConversationGroupInfo cid subconvid (Just ginfo)

fetchConvOrSub ::
  forall r.
  ( Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member MemberStore r,
    Member SubConversationStore r
  ) =>
  Qualified UserId ->
  Local ConvOrSubConvId ->
  Sem r (Local ConvOrSubConv)
fetchConvOrSub qusr convOrSubId = for convOrSubId $ \case
  Conv convId -> Conv <$> getMLSConv qusr (qualifyAs convOrSubId convId)
  SubConv convId sconvId -> do
    let lconv = qualifyAs convOrSubId convId
    c <- getMLSConv qusr lconv
    subconv <- getSubConversation convId sconvId >>= noteS @'ConvNotFound
    pure (SubConv c subconv)
  where
    getMLSConv :: Qualified UserId -> Local ConvId -> Sem r MLSConversation
    getMLSConv u =
      getLocalConvForUser u
        >=> mkMLSConversation
        >=> noteS @'ConvNotFound
