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
    postMLSMessageFromLocalUserV1,
    postMLSMessage,
    MLSMessageStaticErrors,
    MLSBundleStaticErrors,
  )
where

import Control.Comonad
import Control.Error.Util (hush)
import Control.Lens (forOf_, preview)
import Control.Lens.Extras (is)
import Data.Id
import Data.Json.Util
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time
import Data.Tuple.Extra
import GHC.Records
import Galley.API.Action
import Galley.API.Error
import Galley.API.MLS.Conversation
import Galley.API.MLS.Enabled
import Galley.API.MLS.Propagate
import Galley.API.MLS.Removal
import Galley.API.MLS.Types
import Galley.API.MLS.Util
import Galley.API.MLS.Welcome (sendWelcomes)
import Galley.API.Util
import Galley.Data.Conversation.Types hiding (Conversation)
import qualified Galley.Data.Conversation.Types as Data
import Galley.Effects
import Galley.Effects.BrigAccess
import Galley.Effects.ConversationStore
import Galley.Effects.FederatorAccess
import Galley.Effects.MemberStore
import Galley.Effects.ProposalStore
import Galley.Effects.SubConversationStore
import Galley.Env
import Galley.Options
import Galley.Types.Conversations.Members
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal
import Polysemy.Resource (Resource)
import Polysemy.State
import Polysemy.TinyLog
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.MLS.AuthenticatedContent
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Commit
import Wire.API.MLS.CommitBundle
import Wire.API.MLS.Credential
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import qualified Wire.API.MLS.Proposal as Proposal
import Wire.API.MLS.ProposalTag
import Wire.API.MLS.Serialisation
import Wire.API.MLS.SubConversation
import Wire.API.MLS.Validation
import Wire.API.MLS.Welcome
import Wire.API.Message
import Wire.API.User.Client

-- TODO:
-- [x] replace ref with index in remove proposals
-- [ ] validate leaf nodes and key packages locally on galley
--   - [x] extract validation function to wire-api
--   - [x] validate lifetime and public key consistency only on brig
--   - [x] check that ciphersuite matches conversation on galley
--   - [x] check the signature on the LeafNode
--   - [ ] ? verify capabilities
--   - [ ] verify that all extensions are present in the capabilities
--   - [ ] ? in the update case (in galley), verify that the encryption_key is different
-- [ ] validate proposals when processing proposal and commit messages
-- [x] remove MissingSenderClient error
-- [x] remove all key package ref mapping
-- [x] initialise index maps
-- [x] compute new indices for add proposals
-- [x] remove prefixes from value and raw
-- [x] remove PublicGroupState and GroupInfoBundle modules
-- [x] remove prefixes from fields in Commit and Proposal
-- [ ] move external commit logic to a separate module and improve types
-- [x] check epoch inside commit lock
-- [x] split executeProposalAction for internal and external commits

-- [ ] ? consider adding more integration tests
-- [ ] ? rename public_group_state field in conversation table
-- [ ] ? PreSharedKey proposal
-- [ ] ? newtype for leaf node indices

data IncomingMessage = IncomingMessage
  { epoch :: Epoch,
    groupId :: GroupId,
    content :: IncomingMessageContent,
    rawMessage :: RawMLS Message
  }

instance HasField "sender" IncomingMessage (Maybe Sender) where
  getField msg = case msg.content of
    IncomingMessageContentPublic pub -> Just pub.sender
    _ -> Nothing

data IncomingMessageContent
  = IncomingMessageContentPublic IncomingPublicMessageContent
  | IncomingMessageContentPrivate

data IncomingPublicMessageContent = IncomingPublicMessageContent
  { sender :: Sender,
    content :: FramedContentData,
    -- for verification
    framedContent :: RawMLS FramedContent,
    authData :: RawMLS FramedContentAuthData
  }

data IncomingBundle = IncomingBundle
  { epoch :: Epoch,
    groupId :: GroupId,
    sender :: Sender,
    commit :: RawMLS Commit,
    rawMessage :: RawMLS Message,
    welcome :: Maybe (RawMLS Welcome),
    groupInfo :: GroupInfoData,
    serialized :: ByteString
  }

mkIncomingMessage :: RawMLS Message -> Maybe IncomingMessage
mkIncomingMessage msg = case msg.value.content of
  MessagePublic pmsg ->
    Just
      IncomingMessage
        { epoch = pmsg.content.value.epoch,
          groupId = pmsg.content.value.groupId,
          content =
            IncomingMessageContentPublic
              IncomingPublicMessageContent
                { sender = pmsg.content.value.sender,
                  content = pmsg.content.value.content,
                  framedContent = pmsg.content,
                  authData = pmsg.authData
                },
          rawMessage = msg
        }
  MessagePrivate pmsg
    | pmsg.value.tag == FramedContentApplicationDataTag ->
        Just
          IncomingMessage
            { epoch = pmsg.value.epoch,
              groupId = pmsg.value.groupId,
              content = IncomingMessageContentPrivate,
              rawMessage = msg
            }
  _ -> Nothing

incomingMessageAuthenticatedContent :: IncomingPublicMessageContent -> AuthenticatedContent
incomingMessageAuthenticatedContent pmsg =
  AuthenticatedContent
    { wireFormat = WireFormatPublicTag,
      content = pmsg.framedContent,
      authData = pmsg.authData
    }

mkIncomingBundle :: RawMLS CommitBundle -> Maybe IncomingBundle
mkIncomingBundle bundle = do
  imsg <- mkIncomingMessage bundle.value.commitMsg
  content <- case imsg.content of
    IncomingMessageContentPublic c -> pure c
    _ -> Nothing
  commit <- case content.content of
    FramedContentCommit c -> pure c
    _ -> Nothing
  pure
    IncomingBundle
      { epoch = imsg.epoch,
        groupId = imsg.groupId,
        sender = content.sender,
        commit = commit,
        rawMessage = bundle.value.commitMsg,
        welcome = bundle.value.welcome,
        groupInfo = GroupInfoData bundle.value.groupInfo.raw,
        serialized = bundle.raw
      }

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

postMLSMessageFromLocalUserV1 ::
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
  Sem r [Event]
postMLSMessageFromLocalUserV1 lusr c conn smsg = do
  assertMLSEnabled
  imsg <- noteS @'MLSUnsupportedMessage $ mkIncomingMessage smsg
  cnvOrSub <- lookupConvByGroupId imsg.groupId >>= noteS @'ConvNotFound
  map lcuEvent . fst
    <$> postMLSMessage lusr (tUntagged lusr) c cnvOrSub (Just conn) imsg

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
  Sem r ([LocalConversationUpdate], UnreachableUsers)
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
  (events, unreachables) <-
    first (map lcuEvent)
      <$> postMLSCommitBundle lusr (tUntagged lusr) c qConvOrSub (Just conn) ibundle
  t <- toUTCTimeMillis <$> input
  pure $ MLSMessageSendingStatus events t unreachables

postMLSCommitBundleToLocalConv ::
  ( HasProposalEffects r,
    Members MLSBundleStaticErrors r,
    Member Resource r,
    Member SubConversationStore r
  ) =>
  Qualified UserId ->
  ClientId ->
  Maybe ConnId ->
  IncomingBundle ->
  Local ConvOrSubConvId ->
  Sem r ([LocalConversationUpdate], UnreachableUsers)
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

  storeGroupInfo (idForConvOrSub . tUnqualified $ lConvOrSub) bundle.groupInfo

  let cm = membersConvOrSub (tUnqualified lConvOrSub)
  unreachables <- propagateMessage qusr lConvOrSub conn bundle.commit.raw cm
  traverse_ (sendWelcomes lConvOrSub conn newClients) bundle.welcome
  pure (events, unreachables)

postMLSCommitBundleToRemoteConv ::
  ( Members MLSBundleStaticErrors r,
    ( Member BrigAccess r,
      Member (Error FederationError) r,
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
  Sem r ([LocalConversationUpdate], UnreachableUsers)
postMLSCommitBundleToRemoteConv loc qusr c con bundle rConvOrSubId = do
  -- only local users can send messages to remote conversations
  lusr <- foldQualified loc pure (\_ -> throwS @'ConvAccessDenied) qusr
  -- only members may send commit bundles to a remote conversation

  flip unless (throwS @'ConvMemberNotFound) =<< checkLocalMemberRemoteConv (tUnqualified lusr) (convOfConvOrSub <$> rConvOrSubId)

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
    MLSMessageResponseUpdates updates unreachables -> do
      ups <- for updates $ \update -> do
        e <- notifyRemoteConversationAction loc (qualifyAs rConvOrSubId update) con
        pure (LocalConversationUpdate e update)
      pure (ups, unreachables)

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
  Sem r ([LocalConversationUpdate], UnreachableUsers)
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
  let idxMap = indexMapConvOrSub $ tUnqualified lConvOrSubConv
  let epoch = epochNumber . cnvmlsEpoch . mlsMetaConvOrSub . tUnqualified $ lConvOrSubConv
  case mSender of
    SenderMember idx | epoch > 0 -> do
      cid' <- note (mlsProtocolError "unknown sender leaf index") $ imLookup idxMap idx
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
  Sem r ([LocalConversationUpdate], UnreachableUsers)
postMLSMessageToLocalConv qusr c con msg convOrSubId = do
  lConvOrSub <- fetchConvOrSub qusr convOrSubId

  for_ msg.sender $ \sender ->
    void $ getSenderIdentity qusr c sender lConvOrSub

  -- validate message
  events <- case msg.content of
    IncomingMessageContentPublic pub -> case pub.content of
      FramedContentCommit _commit -> throwS @'MLSUnsupportedMessage
      FramedContentApplicationData _ -> throwS @'MLSUnsupportedMessage
      FramedContentProposal prop ->
        processProposal qusr lConvOrSub msg pub prop $> mempty
    IncomingMessageContentPrivate -> pure mempty

  let cm = membersConvOrSub (tUnqualified lConvOrSub)
  unreachables <- propagateMessage qusr lConvOrSub con msg.rawMessage.raw cm
  pure (events, unreachables)

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
  Sem r ([LocalConversationUpdate], UnreachableUsers)
postMLSMessageToRemoteConv loc qusr senderClient con msg rConvOrSubId = do
  -- only local users can send messages to remote conversations
  lusr <- foldQualified loc pure (\_ -> throwS @'ConvAccessDenied) qusr
  -- only members may send messages to the remote conversation
  flip unless (throwS @'ConvMemberNotFound) =<< checkLocalMemberRemoteConv (tUnqualified lusr) (convOfConvOrSub <$> rConvOrSubId)

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
    MLSMessageResponseUpdates updates unreachables -> do
      lcus <- for updates $ \update -> do
        e <- notifyRemoteConversationAction loc (qualifyAs rConvOrSubId update) con
        pure (LocalConversationUpdate e update)
      pure (lcus, unreachables)

type HasProposalEffects r =
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member (Error InternalError) r,
    Member (Error MLSProposalFailure) r,
    Member (Error MLSProtocolError) r,
    Member (ErrorS 'MLSClientMismatch) r,
    Member (ErrorS 'MLSInvalidLeafNodeIndex) r,
    Member (ErrorS 'MLSUnsupportedProposal) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member GundeckAccess r,
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

data ProposalAction = ProposalAction
  { paAdd :: ClientMap,
    paRemove :: ClientMap,
    -- The backend does not process external init proposals, but still it needs
    -- to know if a commit has one when processing external commits
    paExternalInit :: Any
  }
  deriving (Show)

instance Semigroup ProposalAction where
  ProposalAction add1 rem1 init1 <> ProposalAction add2 rem2 init2 =
    ProposalAction
      (Map.unionWith mappend add1 add2)
      (Map.unionWith mappend rem1 rem2)
      (init1 <> init2)

instance Monoid ProposalAction where
  mempty = ProposalAction mempty mempty mempty

paAddClient :: ClientIdentity -> LeafIndex -> ProposalAction
paAddClient cid idx = mempty {paAdd = cmSingleton cid idx}

paRemoveClient :: ClientIdentity -> LeafIndex -> ProposalAction
paRemoveClient cid idx = mempty {paRemove = cmSingleton cid idx}

paExternalInitPresent :: ProposalAction
paExternalInitPresent = mempty {paExternalInit = Any True}

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

getCommitData ::
  ( HasProposalEffects r,
    Member (ErrorS 'MLSProposalNotFound) r,
    Member (ErrorS 'MLSStaleMessage) r
  ) =>
  ClientIdentity ->
  Local ConvOrSubConv ->
  Epoch ->
  Commit ->
  Sem r ProposalAction
getCommitData senderIdentity lConvOrSub epoch commit = do
  let convOrSub = tUnqualified lConvOrSub
      mlsMeta = mlsMetaConvOrSub convOrSub
      curEpoch = cnvmlsEpoch mlsMeta
      groupId = cnvmlsGroupId mlsMeta

  -- check epoch number
  -- TODO: is this really needed?
  when (epoch /= curEpoch) $ throwS @'MLSStaleMessage
  evalState (indexMapConvOrSub convOrSub) $ do
    creatorAction <-
      if epoch == Epoch 0
        then addProposedClient senderIdentity
        else mempty
    proposals <- traverse (derefProposal groupId epoch) commit.proposals
    action <- applyProposals mlsMeta groupId proposals
    pure (creatorAction <> action)

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

processInternalCommit ::
  forall r.
  ( HasProposalEffects r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MLSCommitMissingReferences) r,
    Member (ErrorS 'MLSSelfRemovalNotAllowed) r,
    Member (ErrorS 'MLSStaleMessage) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member SubConversationStore r,
    Member Resource r
  ) =>
  ClientIdentity ->
  Maybe ConnId ->
  Local ConvOrSubConv ->
  Epoch ->
  ProposalAction ->
  Commit ->
  Sem r [LocalConversationUpdate]
processInternalCommit senderIdentity con lConvOrSub epoch action commit = do
  let convOrSub = tUnqualified lConvOrSub
      mlsMeta = mlsMetaConvOrSub convOrSub

  withCommitLock (fmap idForConvOrSub lConvOrSub) (cnvmlsGroupId (mlsMetaConvOrSub convOrSub)) epoch $ do
    -- check all pending proposals are referenced in the commit
    allPendingProposals <- getAllPendingProposalRefs (cnvmlsGroupId mlsMeta) epoch
    let referencedProposals = Set.fromList $ mapMaybe (\x -> preview Proposal._Ref x) commit.proposals
    unless (all (`Set.member` referencedProposals) allPendingProposals) $
      throwS @'MLSCommitMissingReferences

    -- process and execute proposals
    updates <- executeIntCommitProposalAction senderIdentity con lConvOrSub action

    -- increment epoch number
    for_ lConvOrSub incrementEpoch

    pure updates

derefProposal ::
  ( Member ProposalStore r,
    Member (ErrorS 'MLSProposalNotFound) r
  ) =>
  GroupId ->
  Epoch ->
  ProposalOrRef ->
  Sem r Proposal
derefProposal groupId epoch (Ref ref) = do
  p <- getProposal groupId epoch ref >>= noteS @'MLSProposalNotFound
  pure p.value
derefProposal _ _ (Inline p) = pure p

addProposedClient :: Member (State IndexMap) r => ClientIdentity -> Sem r ProposalAction
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
  ConversationMLSData ->
  GroupId ->
  [Proposal] ->
  Sem r ProposalAction
applyProposals mlsMeta groupId =
  -- proposals are sorted before processing
  foldMap (applyProposal mlsMeta groupId)
    . sortOn proposalProcessingStage

applyProposal ::
  ( Member (State IndexMap) r,
    Member (Error MLSProtocolError) r,
    Member (ErrorS 'MLSUnsupportedProposal) r,
    Member (ErrorS 'MLSInvalidLeafNodeIndex) r
  ) =>
  ConversationMLSData ->
  GroupId ->
  Proposal ->
  Sem r ProposalAction
applyProposal mlsMeta _groupId (AddProposal kp) = do
  (cs, _lifetime) <-
    either
      (\msg -> throw (mlsProtocolError ("Invalid key package in Add proposal: " <> msg)))
      pure
      $ validateKeyPackage Nothing kp.value
  unless (mlsMeta.cnvmlsCipherSuite == cs) $
    throw (mlsProtocolError "Key package ciphersuite does not match conversation")
  -- we are not checking lifetime constraints here
  cid <- getKeyPackageIdentity kp.value
  addProposedClient cid
applyProposal _mlsMeta _groupId (RemoveProposal idx) = do
  im <- get
  (cid, im') <- noteS @'MLSInvalidLeafNodeIndex $ imRemoveClient im idx
  put im'
  pure (paRemoveClient cid idx)
applyProposal _mlsMeta _groupId (ExternalInitProposal _) =
  -- only record the fact there was an external init proposal, but do not
  -- process it in any way.
  pure paExternalInitPresent
applyProposal _mlsMeta _groupId _ = pure mempty

checkProposalCipherSuite ::
  Member (Error MLSProtocolError) r =>
  CipherSuiteTag ->
  Proposal ->
  Sem r ()
checkProposalCipherSuite suite (AddProposal kpRaw) = do
  let kp = value kpRaw
  unless (kp.cipherSuite == tagCipherSuite suite)
    . throw
    . mlsProtocolError
    . T.pack
    $ "The group's cipher suite "
      <> show (cipherSuiteNumber (tagCipherSuite suite))
      <> " and the cipher suite of the proposal's key package "
      <> show (cipherSuiteNumber kp.cipherSuite)
      <> " do not match."
checkProposalCipherSuite _suite _prop = pure ()

processProposal ::
  HasProposalEffects r =>
  ( Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MLSStaleMessage) r
  ) =>
  Qualified UserId ->
  Local ConvOrSubConv ->
  IncomingMessage -> -- TODO: just pass header?
  IncomingPublicMessageContent ->
  RawMLS Proposal ->
  Sem r ()
processProposal qusr lConvOrSub msg pub prop = do
  let mlsMeta = mlsMetaConvOrSub (tUnqualified lConvOrSub)
  checkEpoch msg.epoch mlsMeta
  checkGroup msg.groupId mlsMeta
  let suiteTag = cnvmlsCipherSuite mlsMeta
  let cid = mcId . convOfConvOrSub . tUnqualified $ lConvOrSub

  -- validate the proposal
  --
  -- is the user a member of the conversation?
  loc <- qualifyLocal ()
  isMember' <-
    foldQualified
      loc
      ( fmap isJust
          . getLocalMember cid
          . tUnqualified
      )
      ( fmap isJust
          . getRemoteMember cid
      )
      qusr
  unless isMember' $ throwS @'ConvNotFound

  -- FUTUREWORK: validate the member's conversation role
  let propValue = value prop
  checkProposalCipherSuite suiteTag propValue
  when (isExternal pub.sender) $ do
    checkExternalProposalSignature pub prop
    checkExternalProposalUser qusr propValue
  let propRef = authContentRef suiteTag (incomingMessageAuthenticatedContent pub)
  storeProposal msg.groupId msg.epoch propRef ProposalOriginClient prop

isExternal :: Sender -> Bool
isExternal (SenderMember _) = False
isExternal _ = True

checkExternalProposalSignature ::
  Member (ErrorS 'MLSUnsupportedProposal) r =>
  IncomingPublicMessageContent ->
  RawMLS Proposal ->
  Sem r ()
checkExternalProposalSignature msg prop = case value prop of
  AddProposal kp -> do
    let pubkey = kp.value.leafNode.signatureKey
        ctx = error "TODO: get group context"
    unless (verifyMessageSignature ctx msg.framedContent msg.authData pubkey) $ throwS @'MLSUnsupportedProposal
  _ -> pure () -- FUTUREWORK: check signature of other proposals as well

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

type HasProposalActionEffects r =
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member (Error InternalError) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MLSClientMismatch) r,
    Member (Error MLSProposalFailure) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'MLSUnsupportedProposal) r,
    Member (Error MLSProtocolError) r,
    Member (ErrorS 'MLSSelfRemovalNotAllowed) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input Env) r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member MemberStore r,
    Member ProposalStore r,
    Member SubConversationStore r,
    Member TeamStore r,
    Member TinyLog r
  )

executeIntCommitProposalAction ::
  forall r.
  HasProposalActionEffects r =>
  ClientIdentity ->
  Maybe ConnId ->
  Local ConvOrSubConv ->
  ProposalAction ->
  Sem r [LocalConversationUpdate]
executeIntCommitProposalAction senderIdentity con lconvOrSub action = do
  let qusr = cidQualifiedUser senderIdentity
      convOrSub = tUnqualified lconvOrSub
      mlsMeta = mlsMetaConvOrSub convOrSub
      cm = membersConvOrSub convOrSub
      ss = csSignatureScheme (cnvmlsCipherSuite mlsMeta)
      newUserClients = Map.assocs (paAdd action)

  -- FUTUREWORK: remove this check after remote admins are implemented in federation https://wearezeta.atlassian.net/browse/FS-216
  foldQualified lconvOrSub (\_ -> pure ()) (\_ -> throwS @'MLSUnsupportedProposal) qusr

  -- no client can be directly added to a subconversation
  when (is _SubConv convOrSub && any ((senderIdentity /=) . fst) (cmAssocs (paAdd action))) $
    throw (mlsProtocolError "Add proposals in subconversations are not supported")

  -- Note [client removal]
  -- We support two types of removals:
  --  1. when a user is removed from a group, all their clients have to be removed
  --  2. when a client is deleted, that particular client (but not necessarily
  --     other clients of the same user) has to be removed.
  --
  -- Type 2 requires no special processing on the backend, so here we filter
  -- out all removals of that type, so that further checks and processing can
  -- be applied only to type 1 removals.
  --
  -- Furthermore, subconversation clients can be removed arbitrarily, so this
  -- processing is only necessary for main conversations. In the
  -- subconversation case, an empty list is returned.
  removedUsers <- case convOrSub of
    SubConv _ _ -> pure []
    Conv _ -> mapMaybe hush <$$> for (Map.assocs (paRemove action)) $
      \(qtarget, Map.keysSet -> clients) -> runError @() $ do
        -- fetch clients from brig
        clientInfo <- Set.map ciId <$> getClientInfo lconvOrSub qtarget ss
        -- if the clients being removed don't exist, consider this as a removal of
        -- type 2, and skip it
        when (Set.null (clientInfo `Set.intersection` clients)) $
          throw ()
        pure (qtarget, clients)

  membersToRemove <- catMaybes <$> for removedUsers (uncurry (checkRemoval (is _SubConv convOrSub) cm))

  -- for each user, we compare their clients with the ones being added to the conversation
  for_ newUserClients $ \(qtarget, newclients) -> case Map.lookup qtarget cm of
    -- user is already present, skip check in this case
    Just _ -> pure ()
    -- new user
    Nothing -> do
      -- final set of clients in the conversation
      let clients = Map.keysSet (newclients <> Map.findWithDefault mempty qtarget cm)
      -- get list of mls clients from brig
      clientInfo <- getClientInfo lconvOrSub qtarget ss
      let allClients = Set.map ciId clientInfo
      let allMLSClients = Set.map ciId (Set.filter ciMLS clientInfo)
      -- We check the following condition:
      --   allMLSClients ⊆ clients ⊆ allClients
      -- i.e.
      -- - if a client has at least 1 key package, it has to be added
      -- - if a client is being added, it has to still exist
      --
      -- The reason why we can't simply check that clients == allMLSClients is
      -- that a client with no remaining key packages might be added by a user
      -- who just fetched its last key package.
      unless
        ( Set.isSubsetOf allMLSClients clients
            && Set.isSubsetOf clients allClients
        )
        $ do
          -- unless (Set.isSubsetOf allClients clients) $ do
          -- FUTUREWORK: turn this error into a proper response
          throwS @'MLSClientMismatch

  -- remove users from the conversation and send events
  removeEvents <-
    foldMap
      (removeMembers qusr con lconvOrSub)
      (nonEmpty membersToRemove)

  -- Remove clients from the conversation state. This includes client removals
  -- of all types (see Note [client removal]).
  for_ (Map.assocs (paRemove action)) $ \(qtarget, clients) -> do
    removeMLSClients (cnvmlsGroupId mlsMeta) qtarget (Map.keysSet clients)

  -- if this is a new subconversation, call `on-new-remote-conversation` on all
  -- the remote backends involved in the main conversation
  forOf_ _SubConv convOrSub $ \(mlsConv, subConv) -> do
    when (cnvmlsEpoch (scMLSData subConv) == Epoch 0) $ do
      let remoteDomains =
            Set.fromList
              ( map
                  (void . rmId)
                  (mcRemoteMembers mlsConv)
              )
      let nrc =
            NewRemoteSubConversation
              { nrscConvId = mcId mlsConv,
                nrscSubConvId = scSubConvId subConv,
                nrscMlsData = scMLSData subConv
              }
      runFederatedConcurrently_ (toList remoteDomains) $ \_ -> do
        void $ fedClient @'Galley @"on-new-remote-subconversation" nrc

  -- add users to the conversation and send events
  addEvents <-
    foldMap (addMembers qusr con lconvOrSub)
      . nonEmpty
      . map fst
      $ newUserClients

  -- add clients in the conversation state
  for_ newUserClients $ \(qtarget, newClients) -> do
    addMLSClients (cnvmlsGroupId mlsMeta) qtarget (Set.fromList (Map.assocs newClients))

  -- TODO: increment epoch here instead of in the calling site

  pure (addEvents <> removeEvents)
  where
    checkRemoval ::
      Bool ->
      ClientMap ->
      Qualified UserId ->
      Set ClientId ->
      Sem r (Maybe (Qualified UserId))
    checkRemoval isSubConv cm qtarget clients = do
      let clientsInConv = Map.keysSet (Map.findWithDefault mempty qtarget cm)
      -- FUTUREWORK: add tests against this situation for conv v subconv
      when (not isSubConv && clients /= clientsInConv) $ do
        -- FUTUREWORK: turn this error into a proper response
        throwS @'MLSClientMismatch
      when (cidQualifiedUser senderIdentity == qtarget) $
        throwS @'MLSSelfRemovalNotAllowed
      pure (Just qtarget)

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

  -- TODO required for external proposals?
  -- FUTUREWORK: remove this check after remote admins are implemented in federation https://wearezeta.atlassian.net/browse/FS-216
  -- foldQualified lconvOrSub (\_ -> pure ()) (\_ -> throwS @'MLSUnsupportedProposal) qusr

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

existingLocalMembers :: Local Data.Conversation -> Set (Qualified UserId)
existingLocalMembers lconv =
  (Set.fromList . map (fmap lmId . tUntagged)) (traverse convLocalMembers lconv)

existingRemoteMembers :: Local Data.Conversation -> Set (Qualified UserId)
existingRemoteMembers lconv =
  Set.fromList . map (tUntagged . rmId) . convRemoteMembers . tUnqualified $
    lconv

existingMembers :: Local Data.Conversation -> Set (Qualified UserId)
existingMembers lconv = existingLocalMembers lconv <> existingRemoteMembers lconv

addMembers ::
  HasProposalActionEffects r =>
  Qualified UserId ->
  Maybe ConnId ->
  Local ConvOrSubConv ->
  NonEmpty (Qualified UserId) ->
  Sem r [LocalConversationUpdate]
addMembers qusr con lconvOrSub users = case tUnqualified lconvOrSub of
  Conv mlsConv -> do
    let lconv = qualifyAs lconvOrSub (mcConv mlsConv)
    -- FUTUREWORK: update key package ref mapping to reflect conversation membership
    foldMap
      ( handleNoChanges
          . handleMLSProposalFailures @ProposalErrors
          . fmap pure
          . updateLocalConversationUnchecked @'ConversationJoinTag lconv qusr con
          . flip ConversationJoin roleNameWireMember
      )
      . nonEmpty
      . filter (flip Set.notMember (existingMembers lconv))
      . toList
      $ users
  SubConv _ _ -> pure []

removeMembers ::
  HasProposalActionEffects r =>
  Qualified UserId ->
  Maybe ConnId ->
  Local ConvOrSubConv ->
  NonEmpty (Qualified UserId) ->
  Sem r [LocalConversationUpdate]
removeMembers qusr con lconvOrSub users = case tUnqualified lconvOrSub of
  Conv mlsConv -> do
    let lconv = qualifyAs lconvOrSub (mcConv mlsConv)
    foldMap
      ( handleNoChanges
          . handleMLSProposalFailures @ProposalErrors
          . fmap pure
          . updateLocalConversationUnchecked @'ConversationRemoveMembersTag lconv qusr con
      )
      . nonEmpty
      . filter (flip Set.member (existingMembers lconv))
      . toList
      $ users
  SubConv _ _ -> pure []

getKeyPackageIdentity ::
  Member (ErrorS 'MLSUnsupportedProposal) r =>
  KeyPackage ->
  Sem r ClientIdentity
getKeyPackageIdentity =
  either (\_ -> throwS @'MLSUnsupportedProposal) pure
    . keyPackageIdentity

handleNoChanges :: Monoid a => Sem (Error NoChanges ': r) a -> Sem r a
handleNoChanges = fmap fold . runError

getClientInfo ::
  ( Member BrigAccess r,
    Member FederatorAccess r
  ) =>
  Local x ->
  Qualified UserId ->
  SignatureSchemeTag ->
  Sem r (Set ClientInfo)
getClientInfo loc = foldQualified loc getLocalMLSClients getRemoteMLSClients

getRemoteMLSClients ::
  ( Member FederatorAccess r
  ) =>
  Remote UserId ->
  SignatureSchemeTag ->
  Sem r (Set ClientInfo)
getRemoteMLSClients rusr ss = do
  runFederated rusr $
    fedClient @'Brig @"get-mls-clients" $
      MLSClientsRequest
        { mcrUserId = tUnqualified rusr,
          mcrSignatureScheme = ss
        }

-- | Check if the epoch number matches that of a conversation
checkEpoch ::
  Member (ErrorS 'MLSStaleMessage) r =>
  Epoch ->
  ConversationMLSData ->
  Sem r ()
checkEpoch epoch mlsMeta = do
  unless (epoch == cnvmlsEpoch mlsMeta) $ throwS @'MLSStaleMessage

-- | Check if the group ID matches that of a conversation
checkGroup ::
  Member (ErrorS 'ConvNotFound) r =>
  GroupId ->
  ConversationMLSData ->
  Sem r ()
checkGroup gId mlsMeta = do
  unless (gId == cnvmlsGroupId mlsMeta) $ throwS @'ConvNotFound

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
  handleMLSProposalFailure = mapError (MLSProposalFailure . toWai)

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
