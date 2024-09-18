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

import Data.Domain
import Data.Id
import Data.Json.Util
import Data.LegalHold
import Data.Qualified
import Data.Set qualified as Set
import Data.Tagged (Tagged)
import Data.Text.Lazy qualified as LT
import Data.Tuple.Extra
import Galley.API.Action
import Galley.API.Error
import Galley.API.LegalHold.Get (getUserStatus)
import Galley.API.MLS.Commit.Core (getCommitData)
import Galley.API.MLS.Commit.ExternalCommit
import Galley.API.MLS.Commit.InternalCommit
import Galley.API.MLS.Conversation
import Galley.API.MLS.Enabled
import Galley.API.MLS.IncomingMessage
import Galley.API.MLS.One2One
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
import Galley.Effects.TeamStore (getUserTeams)
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.Input
import Polysemy.Internal
import Polysemy.Output
import Polysemy.Resource (Resource)
import Polysemy.TinyLog
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Protocol
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Commit hiding (output)
import Wire.API.MLS.CommitBundle
import Wire.API.MLS.Credential
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.Message
import Wire.API.MLS.Serialisation
import Wire.API.MLS.SubConversation
import Wire.API.Team.LegalHold
import Wire.NotificationSubsystem

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
  (ctype, cnvOrSub) <- getConvFromGroupId imsg.groupId
  events <-
    map lcuEvent
      <$> postMLSMessage lusr (tUntagged lusr) c ctype cnvOrSub (Just conn) imsg
  t <- toUTCTimeMillis <$> input
  pure $ MLSMessageSendingStatus events t

postMLSCommitBundle ::
  ( HasProposalEffects r,
    Members MLSBundleStaticErrors r,
    Member Fail r,
    Member (ErrorS MLSLegalholdIncompatible) r,
    Member Random r,
    Member Resource r,
    Member SubConversationStore r
  ) =>
  Local x ->
  Qualified UserId ->
  ClientId ->
  ConvType ->
  Qualified ConvOrSubConvId ->
  Maybe ConnId ->
  IncomingBundle ->
  Sem r [LocalConversationUpdate]
postMLSCommitBundle loc qusr c ctype qConvOrSub conn bundle =
  foldQualified
    loc
    (postMLSCommitBundleToLocalConv qusr c conn bundle ctype)
    (postMLSCommitBundleToRemoteConv loc qusr c conn bundle ctype)
    qConvOrSub

postMLSCommitBundleFromLocalUser ::
  ( HasProposalEffects r,
    Members MLSBundleStaticErrors r,
    Member Random r,
    Member Fail r,
    Member (ErrorS MLSLegalholdIncompatible) r,
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
  (ctype, qConvOrSub) <- getConvFromGroupId ibundle.groupId
  events <-
    map lcuEvent
      <$> postMLSCommitBundle lusr (tUntagged lusr) c ctype qConvOrSub (Just conn) ibundle
  t <- toUTCTimeMillis <$> input
  pure $ MLSMessageSendingStatus events t

postMLSCommitBundleToLocalConv ::
  ( HasProposalEffects r,
    Members MLSBundleStaticErrors r,
    Member Resource r,
    Member (ErrorS MLSLegalholdIncompatible) r,
    Member SubConversationStore r,
    Member Random r,
    Member Fail r
  ) =>
  Qualified UserId ->
  ClientId ->
  Maybe ConnId ->
  IncomingBundle ->
  ConvType ->
  Local ConvOrSubConvId ->
  Sem r [LocalConversationUpdate]
postMLSCommitBundleToLocalConv qusr c conn bundle ctype lConvOrSubId = do
  lConvOrSub <- fetchConvOrSub qusr bundle.groupId ctype lConvOrSubId
  let convOrSub = tUnqualified lConvOrSub

  ciphersuite <-
    note (mlsProtocolError "Unsupported ciphersuite") $
      cipherSuiteTag bundle.groupInfo.value.groupContext.cipherSuite

  -- when a user tries to join any mls conversation while being legalholded
  -- they receive a 409 stating that mls and legalhold are incompatible
  case qusr `relativeTo` lConvOrSubId of
    Local luid ->
      when (isNothing convOrSub.mlsMeta.cnvmlsActiveData) do
        usrTeams <- getUserTeams (tUnqualified luid)
        for_ usrTeams \tid -> do
          -- this would only return 'Left' if the team member did vanish directly in the process of this
          -- request or if the legalhold state was somehow inconsistent. We can safely assume that this
          -- should be a server error
          Right resp <- runError @(Tagged TeamMemberNotFound ()) $ getUserStatus luid tid (tUnqualified luid)
          case resp.ulhsrStatus of
            UserLegalHoldPending -> throwS @MLSLegalholdIncompatible
            UserLegalHoldEnabled -> throwS @MLSLegalholdIncompatible
            UserLegalHoldDisabled -> pure ()
            UserLegalHoldNoConsent -> pure ()

    -- we can skip the remote case because we currently to not support creating conversations on the remote backend
    Remote _ -> pure ()

  ciphersuiteUpdate <- case convOrSub.mlsMeta.cnvmlsActiveData of
    -- if this is the first commit of the conversation, update ciphersuite
    Nothing -> pure True
    -- otherwise, make sure the ciphersuite matches
    Just activeData -> do
      unless (ciphersuite == activeData.ciphersuite) $
        throw $
          mlsProtocolError "GroupInfo ciphersuite does not match conversation"
      unless (bundle.epoch == activeData.epoch) $ throwS @'MLSStaleMessage
      pure False

  senderIdentity <- getSenderIdentity qusr c bundle.sender lConvOrSub

  (events, newClients) <- case bundle.sender of
    SenderMember _index -> do
      -- extract added/removed clients from bundle
      action <- getCommitData senderIdentity lConvOrSub bundle.epoch ciphersuite bundle

      -- process additions and removals
      events <-
        processInternalCommit
          senderIdentity
          conn
          lConvOrSub
          ciphersuite
          ciphersuiteUpdate
          bundle.epoch
          action
          bundle.commit.value
      -- the sender client is included in the Add action on the first commit,
      -- but it doesn't need to get a welcome message, so we filter it out here
      let newClients = filter ((/=) senderIdentity) (cmIdentities (paAdd action))
      pure (events, newClients)
    SenderExternal _ -> throw (mlsProtocolError "Unexpected sender")
    SenderNewMemberProposal -> throw (mlsProtocolError "Unexpected sender")
    SenderNewMemberCommit -> do
      action <- getExternalCommitData senderIdentity lConvOrSub bundle.epoch bundle.commit.value
      processExternalCommit
        senderIdentity
        lConvOrSub
        ciphersuite
        ciphersuiteUpdate
        bundle.epoch
        action
        bundle.commit.value.path
      pure ([], [])

  storeGroupInfo (tUnqualified lConvOrSub).id (GroupInfoData bundle.groupInfo.raw)

  propagateMessage qusr (Just c) lConvOrSub conn bundle.rawMessage (tUnqualified lConvOrSub).members

  for_ bundle.welcome $ \welcome ->
    sendWelcomes lConvOrSubId qusr conn newClients welcome

  pure events

postMLSCommitBundleToRemoteConv ::
  ( Member BrigAccess r,
    Members MLSBundleStaticErrors r,
    Member (Error FederationError) r,
    Member (Error MLSProtocolError) r,
    Member (Error MLSProposalFailure) r,
    Member (Error NonFederatingBackends) r,
    Member (Error UnreachableBackends) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member MemberStore r,
    Member TinyLog r
  ) =>
  Local x ->
  Qualified UserId ->
  ClientId ->
  Maybe ConnId ->
  IncomingBundle ->
  ConvType ->
  Remote ConvOrSubConvId ->
  Sem r [LocalConversationUpdate]
postMLSCommitBundleToRemoteConv loc qusr c con bundle ctype rConvOrSubId = do
  -- only local users can send messages to remote conversations
  lusr <- foldQualified loc pure (\_ -> throwS @'ConvAccessDenied) qusr

  unless (bundle.epoch == Epoch 0 && ctype == One2OneConv) $
    -- only members may send commit bundles to a remote conversation
    flip unless (throwS @'ConvMemberNotFound) =<< checkLocalMemberRemoteConv (tUnqualified lusr) ((.conv) <$> rConvOrSubId)

  resp <-
    runFederated rConvOrSubId $
      fedClient @'Galley @"send-mls-commit-bundle" $
        MLSMessageSendRequest
          { convOrSubId = tUnqualified rConvOrSubId,
            sender = tUnqualified lusr,
            senderClient = c,
            rawMessage = Base64ByteString bundle.serialized
          }
  case resp of
    MLSMessageResponseError e -> rethrowErrors @MLSBundleStaticErrors e
    MLSMessageResponseProtocolError e -> throw (mlsProtocolError e)
    MLSMessageResponseProposalFailure e -> throw (MLSProposalFailure e)
    MLSMessageResponseUnreachableBackends ds -> throw (UnreachableBackends (toList ds))
    MLSMessageResponseUpdates updates -> do
      fmap fst . runOutputList . runInputConst (void loc) $
        for_ updates $ \update -> do
          me <- updateLocalStateOfRemoteConv (qualifyAs rConvOrSubId update) con
          for_ me $ \e -> output (LocalConversationUpdate e update)
    MLSMessageResponseNonFederatingBackends e -> throw e

postMLSMessage ::
  ( HasProposalEffects r,
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
  ConvType ->
  Qualified ConvOrSubConvId ->
  Maybe ConnId ->
  IncomingMessage ->
  Sem r [LocalConversationUpdate]
postMLSMessage loc qusr c ctype qconvOrSub con msg = do
  foldQualified
    loc
    (postMLSMessageToLocalConv qusr c con msg ctype)
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
  let epoch = epochNumber . cnvmlsEpoch . (.mlsMeta) . tUnqualified $ lConvOrSubConv
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
  ConvType ->
  Local ConvOrSubConvId ->
  Sem r [LocalConversationUpdate]
postMLSMessageToLocalConv qusr c con msg ctype convOrSubId = do
  lConvOrSub <- fetchConvOrSub qusr msg.groupId ctype convOrSubId
  let convOrSub = tUnqualified lConvOrSub

  for_ msg.sender $ \sender ->
    void $ getSenderIdentity qusr c sender lConvOrSub

  -- validate message
  case msg.content of
    IncomingMessageContentPublic pub -> case pub.content of
      FramedContentCommit _commit -> throwS @'MLSUnsupportedMessage
      FramedContentApplicationData _ -> throwS @'MLSUnsupportedMessage
      -- proposal message
      FramedContentProposal prop ->
        processProposal qusr lConvOrSub msg.groupId msg.epoch pub prop
    IncomingMessageContentPrivate -> do
      -- application message:

      -- reject all application messages if the conv is in mixed state
      when (convOrSub.migrationState == MLSMigrationMixed) $
        throwS @'MLSUnsupportedMessage

      -- reject application messages older than 2 epochs
      -- FUTUREWORK: consider rejecting this message if the conversation epoch is 0
      let epochInt :: Epoch -> Integer
          epochInt = fromIntegral . epochNumber
      case convOrSub.mlsMeta.cnvmlsActiveData of
        Nothing -> throw $ mlsProtocolError "Application messages at epoch 0 are not supported"
        Just activeData ->
          when
            ( epochInt msg.epoch < epochInt activeData.epoch - 2
                || epochInt msg.epoch > epochInt activeData.epoch
            )
            $ throwS @'MLSStaleMessage

  propagateMessage qusr (Just c) lConvOrSub con msg.rawMessage (tUnqualified lConvOrSub).members
  pure []

postMLSMessageToRemoteConv ::
  ( Members MLSMessageStaticErrors r,
    HasProposalEffects r
  ) =>
  Local x ->
  Qualified UserId ->
  ClientId ->
  Maybe ConnId ->
  IncomingMessage ->
  Remote ConvOrSubConvId ->
  Sem r [LocalConversationUpdate]
postMLSMessageToRemoteConv loc qusr senderClient con msg rConvOrSubId = do
  -- only local users can send messages to remote conversations
  lusr <- foldQualified loc pure (\_ -> throwS @'ConvAccessDenied) qusr
  -- only members may send messages to the remote conversation
  flip unless (throwS @'ConvMemberNotFound) =<< checkLocalMemberRemoteConv (tUnqualified lusr) ((.conv) <$> rConvOrSubId)

  resp <-
    runFederated rConvOrSubId $
      fedClient @'Galley @"send-mls-message" $
        MLSMessageSendRequest
          { convOrSubId = tUnqualified rConvOrSubId,
            sender = tUnqualified lusr,
            senderClient = senderClient,
            rawMessage = Base64ByteString msg.rawMessage.raw
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
    MLSMessageResponseUpdates updates -> do
      fmap fst . runOutputList $
        for_ updates $ \update -> do
          me <- updateLocalStateOfRemoteConv (qualifyAs rConvOrSubId update) con
          for_ me $ \e -> output (LocalConversationUpdate e update)
    MLSMessageResponseNonFederatingBackends e -> throw e

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
    Member (Error MLSProtocolError) r,
    Member MemberStore r,
    Member SubConversationStore r
  ) =>
  Qualified UserId ->
  GroupId ->
  ConvType ->
  Local ConvOrSubConvId ->
  Sem r (Local ConvOrSubConv)
fetchConvOrSub qusr groupId ctype convOrSubId = for convOrSubId $ \case
  Conv convId -> Conv <$> getMLSConv qusr (Just groupId) ctype (qualifyAs convOrSubId convId)
  SubConv convId sconvId -> do
    let lconv = qualifyAs convOrSubId convId
    c <- getMLSConv qusr Nothing ctype lconv
    msubconv <- getSubConversation convId sconvId
    subconv <- case msubconv of
      Nothing -> pure $ newSubConversationFromParent lconv sconvId
      Just subconv -> do
        when (groupId /= subconv.scMLSData.cnvmlsGroupId) $
          throw (mlsProtocolError "The message group ID does not match the subconversation")
        pure subconv
    pure (SubConv c subconv)

getMLSConv ::
  ( Member (ErrorS 'ConvNotFound) r,
    Member (Error MLSProtocolError) r,
    Member ConversationStore r,
    Member MemberStore r
  ) =>
  Qualified UserId ->
  Maybe GroupId ->
  ConvType ->
  Local ConvId ->
  Sem r MLSConversation
getMLSConv u mGroupId ctype lcnv = do
  mlsConv <- case ctype of
    One2OneConv -> do
      mconv <- getConversation (tUnqualified lcnv)
      case mconv of
        Just conv -> mkMLSConversation conv >>= noteS @'ConvNotFound
        Nothing ->
          let (meta, mlsData) = localMLSOne2OneConversationMetadata (tUntagged lcnv)
           in pure (newMLSConversation lcnv meta mlsData)
    _ ->
      getLocalConvForUser u lcnv
        >>= mkMLSConversation
        >>= noteS @'ConvNotFound
  -- check that the group ID in the message matches that of the conversation
  for_ mGroupId $ \groupId ->
    when (groupId /= mlsConv.mcMLSData.cnvmlsGroupId) $
      throw (mlsProtocolError "The message group ID does not match the conversation")
  pure mlsConv
