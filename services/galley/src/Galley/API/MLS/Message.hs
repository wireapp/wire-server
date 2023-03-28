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
  ( postMLSCommitBundle,
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
import Control.Lens (preview)
import Data.Id
import Data.Json.Util
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time
import Data.Tuple.Extra
import Galley.API.Action
import Galley.API.Error
import Galley.API.MLS.Enabled
import Galley.API.MLS.KeyPackage
import Galley.API.MLS.Propagate
import Galley.API.MLS.Removal
import Galley.API.MLS.Types
import Galley.API.MLS.Util
import Galley.API.MLS.Welcome (postMLSWelcome)
import Galley.API.Util
import Galley.Data.Conversation.Types hiding (Conversation)
import qualified Galley.Data.Conversation.Types as Data
import Galley.Data.Types
import Galley.Effects
import Galley.Effects.BrigAccess
import Galley.Effects.ConversationStore
import Galley.Effects.FederatorAccess
import Galley.Effects.MemberStore
import Galley.Effects.ProposalStore
import Galley.Env
import Galley.Options
import Galley.Types.Conversations.Members
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal
import Polysemy.Resource (Resource, bracket)
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
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Commit
import Wire.API.MLS.CommitBundle
import Wire.API.MLS.Credential
import Wire.API.MLS.GroupInfoBundle
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import qualified Wire.API.MLS.Proposal as Proposal
import Wire.API.MLS.PublicGroupState
import Wire.API.MLS.Serialisation
import Wire.API.MLS.SubConversation
import Wire.API.MLS.Welcome
import Wire.API.Message
import Wire.API.Routes.Internal.Brig
import Wire.API.User.Client

type MLSMessageStaticErrors =
  '[ ErrorS 'ConvAccessDenied,
     ErrorS 'ConvMemberNotFound,
     ErrorS 'ConvNotFound,
     ErrorS 'MLSNotEnabled,
     ErrorS 'MLSUnsupportedMessage,
     ErrorS 'MLSStaleMessage,
     ErrorS 'MLSProposalNotFound,
     ErrorS 'MissingLegalholdConsent,
     ErrorS 'MLSKeyPackageRefNotFound,
     ErrorS 'MLSClientMismatch,
     ErrorS 'MLSUnsupportedProposal,
     ErrorS 'MLSCommitMissingReferences,
     ErrorS 'MLSSelfRemovalNotAllowed,
     ErrorS 'MLSClientSenderUserMismatch,
     ErrorS 'MLSGroupConversationMismatch,
     ErrorS 'MLSMissingSenderClient
   ]

type MLSBundleStaticErrors =
  Append
    MLSMessageStaticErrors
    '[ErrorS 'MLSWelcomeMismatch]

postMLSMessageFromLocalUserV1 ::
  ( HasProposalEffects r,
    ( Member (Error FederationError) r,
      Member (Error InternalError) r,
      Member (ErrorS 'ConvAccessDenied) r,
      Member (ErrorS 'ConvMemberNotFound) r,
      Member (ErrorS 'ConvNotFound) r,
      Member (ErrorS 'MissingLegalholdConsent) r,
      Member (ErrorS 'MLSClientSenderUserMismatch) r,
      Member (ErrorS 'MLSCommitMissingReferences) r,
      Member (ErrorS 'MLSGroupConversationMismatch) r,
      Member (ErrorS 'MLSMissingSenderClient) r,
      Member (ErrorS 'MLSNotEnabled) r,
      Member (ErrorS 'MLSProposalNotFound) r,
      Member (ErrorS 'MLSSelfRemovalNotAllowed) r,
      Member (ErrorS 'MLSStaleMessage) r,
      Member (ErrorS 'MLSUnsupportedMessage) r,
      Member (Input (Local ())) r,
      Member ProposalStore r,
      Member Resource r,
      Member TinyLog r
    )
  ) =>
  Local UserId ->
  Maybe ClientId ->
  ConnId ->
  RawMLS SomeMessage ->
  Sem r [Event]
postMLSMessageFromLocalUserV1 lusr mc conn smsg = do
  assertMLSEnabled
  case rmValue smsg of
    SomeMessage _ msg -> do
      qcnv <- getConversationIdByGroupId (msgGroupId msg) >>= noteS @'ConvNotFound
      fst . first (map lcuEvent)
        <$> postMLSMessage lusr (tUntagged lusr) mc qcnv (Just conn) smsg

postMLSMessageFromLocalUser ::
  ( HasProposalEffects r,
    ( Member (Error FederationError) r,
      Member (Error InternalError) r,
      Member (ErrorS 'ConvAccessDenied) r,
      Member (ErrorS 'ConvMemberNotFound) r,
      Member (ErrorS 'ConvNotFound) r,
      Member (ErrorS 'MissingLegalholdConsent) r,
      Member (ErrorS 'MLSClientSenderUserMismatch) r,
      Member (ErrorS 'MLSCommitMissingReferences) r,
      Member (ErrorS 'MLSGroupConversationMismatch) r,
      Member (ErrorS 'MLSMissingSenderClient) r,
      Member (ErrorS 'MLSNotEnabled) r,
      Member (ErrorS 'MLSProposalNotFound) r,
      Member (ErrorS 'MLSSelfRemovalNotAllowed) r,
      Member (ErrorS 'MLSStaleMessage) r,
      Member (ErrorS 'MLSUnsupportedMessage) r,
      Member (Input (Local ())) r,
      Member ProposalStore r,
      Member Resource r,
      Member TinyLog r
    )
  ) =>
  Local UserId ->
  Maybe ClientId ->
  ConnId ->
  RawMLS SomeMessage ->
  Sem r MLSMessageSendingStatus
postMLSMessageFromLocalUser lusr mc conn smsg = do
  -- FUTUREWORK: Inline the body of 'postMLSMessageFromLocalUserV1' once version
  -- V1 is dropped
  assertMLSEnabled
  (events, unreachables) <- case rmValue smsg of
    SomeMessage _ msg -> do
      qcnv <- getConversationIdByGroupId (msgGroupId msg) >>= noteS @'ConvNotFound
      first (map lcuEvent)
        <$> postMLSMessage lusr (tUntagged lusr) mc qcnv (Just conn) smsg
  t <- toUTCTimeMillis <$> input
  pure $ MLSMessageSendingStatus events t unreachables

postMLSCommitBundle ::
  ( HasProposalEffects r,
    Members MLSBundleStaticErrors r,
    ( Member BrigAccess r,
      Member (Error FederationError) r,
      Member (Error InternalError) r,
      Member (Error MLSProtocolError) r,
      Member (Input (Local ())) r,
      Member (Input Opts) r,
      Member (Input UTCTime) r,
      Member MemberStore r,
      Member ProposalStore r,
      Member Resource r,
      Member TinyLog r
    )
  ) =>
  Local x ->
  Qualified UserId ->
  Maybe ClientId ->
  Qualified ConvId ->
  Maybe ConnId ->
  CommitBundle ->
  Sem r ([LocalConversationUpdate], UnreachableUserList)
postMLSCommitBundle loc qusr mc qcnv conn rawBundle =
  foldQualified
    loc
    (postMLSCommitBundleToLocalConv qusr mc conn rawBundle)
    (postMLSCommitBundleToRemoteConv loc qusr conn rawBundle)
    qcnv

postMLSCommitBundleFromLocalUser ::
  ( HasProposalEffects r,
    Members MLSBundleStaticErrors r,
    ( Member BrigAccess r,
      Member (Error FederationError) r,
      Member (Error InternalError) r,
      Member (ErrorS 'MLSNotEnabled) r,
      Member (Input (Local ())) r,
      Member (Input Opts) r,
      Member (Input UTCTime) r,
      Member MemberStore r,
      Member ProposalStore r,
      Member Resource r,
      Member TinyLog r
    )
  ) =>
  Local UserId ->
  Maybe ClientId ->
  ConnId ->
  CommitBundle ->
  Sem r MLSMessageSendingStatus
postMLSCommitBundleFromLocalUser lusr mc conn bundle = do
  assertMLSEnabled
  let msg = rmValue (cbCommitMsg bundle)
  qcnv <- getConversationIdByGroupId (msgGroupId msg) >>= noteS @'ConvNotFound
  (events, unreachables) <-
    first (map lcuEvent)
      <$> postMLSCommitBundle lusr (tUntagged lusr) mc qcnv (Just conn) bundle
  t <- toUTCTimeMillis <$> input
  pure $ MLSMessageSendingStatus events t unreachables

postMLSCommitBundleToLocalConv ::
  ( HasProposalEffects r,
    Members MLSBundleStaticErrors r,
    ( Member BrigAccess r,
      Member (Error FederationError) r,
      Member (Error InternalError) r,
      Member (Error MLSProtocolError) r,
      Member (Input Opts) r,
      Member (Input UTCTime) r,
      Member ProposalStore r,
      Member Resource r,
      Member TinyLog r
    )
  ) =>
  Qualified UserId ->
  Maybe ClientId ->
  Maybe ConnId ->
  CommitBundle ->
  Local ConvId ->
  Sem r ([LocalConversationUpdate], UnreachableUserList)
postMLSCommitBundleToLocalConv qusr mc conn bundle lcnv = do
  let msg = rmValue (cbCommitMsg bundle)
  conv <- getLocalConvForUser qusr lcnv
  mlsMeta <- Data.mlsMetadata conv & noteS @'ConvNotFound

  let lconv = qualifyAs lcnv conv
  cm <- lookupMLSClients (cnvmlsGroupId mlsMeta)

  senderClient <- fmap ciClient <$> getSenderIdentity qusr mc SMLSPlainText msg

  events <- case msgPayload msg of
    CommitMessage commit ->
      do
        action <- getCommitData lconv mlsMeta (msgEpoch msg) commit
        -- check that the welcome message matches the action
        for_ (cbWelcome bundle) $ \welcome ->
          when
            ( Set.fromList (map gsNewMember (welSecrets (rmValue welcome)))
                /= Set.fromList (map (snd . snd) (cmAssocs (paAdd action)))
            )
            $ throwS @'MLSWelcomeMismatch
        updates <-
          processCommitWithAction
            qusr
            senderClient
            conn
            lconv
            mlsMeta
            cm
            (msgEpoch msg)
            action
            (msgSender msg)
            commit
        storeGroupInfoBundle lconv (cbGroupInfoBundle bundle)
        pure updates
    ApplicationMessage _ -> throwS @'MLSUnsupportedMessage
    ProposalMessage _ -> throwS @'MLSUnsupportedMessage

  unreachables <- propagateMessage qusr (qualifyAs lcnv conv) cm conn (rmRaw (cbCommitMsg bundle))

  for_ (cbWelcome bundle) $
    postMLSWelcome lcnv conn

  pure (events, unreachables)

postMLSCommitBundleToRemoteConv ::
  ( Members MLSBundleStaticErrors r,
    ( Member (Error FederationError) r,
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
  Maybe ConnId ->
  CommitBundle ->
  Remote ConvId ->
  Sem r ([LocalConversationUpdate], UnreachableUserList)
postMLSCommitBundleToRemoteConv loc qusr con bundle rcnv = do
  -- only local users can send messages to remote conversations
  lusr <- foldQualified loc pure (\_ -> throwS @'ConvAccessDenied) qusr
  -- only members may send commit bundles to a remote conversation
  flip unless (throwS @'ConvMemberNotFound) =<< checkLocalMemberRemoteConv (tUnqualified lusr) rcnv

  resp <-
    runFederated rcnv $
      fedClient @'Galley @"send-mls-commit-bundle" $
        MLSMessageSendRequest
          { mmsrConvOrSubId = Conv $ tUnqualified rcnv,
            mmsrSender = tUnqualified lusr,
            mmsrRawMessage = Base64ByteString (serializeCommitBundle bundle)
          }
  case resp of
    MLSMessageResponseError e -> rethrowErrors @MLSBundleStaticErrors e
    MLSMessageResponseProtocolError e -> throw (mlsProtocolError e)
    MLSMessageResponseProposalFailure e -> throw (MLSProposalFailure e)
    MLSMessageResponseUpdates updates unreachables -> do
      ups <- for updates $ \update -> do
        e <- notifyRemoteConversationAction loc (qualifyAs rcnv update) con
        pure (LocalConversationUpdate e update)
      pure (ups, unreachables)

postMLSMessage ::
  ( HasProposalEffects r,
    ( Member (Error FederationError) r,
      Member (Error InternalError) r,
      Member (ErrorS 'ConvAccessDenied) r,
      Member (ErrorS 'ConvMemberNotFound) r,
      Member (ErrorS 'ConvNotFound) r,
      Member (ErrorS 'MLSNotEnabled) r,
      Member (ErrorS 'MissingLegalholdConsent) r,
      Member (ErrorS 'MLSClientSenderUserMismatch) r,
      Member (ErrorS 'MLSCommitMissingReferences) r,
      Member (ErrorS 'MLSGroupConversationMismatch) r,
      Member (ErrorS 'MLSMissingSenderClient) r,
      Member (ErrorS 'MLSProposalNotFound) r,
      Member (ErrorS 'MLSSelfRemovalNotAllowed) r,
      Member (ErrorS 'MLSStaleMessage) r,
      Member (ErrorS 'MLSUnsupportedMessage) r,
      Member (Input (Local ())) r,
      Member ProposalStore r,
      Member Resource r,
      Member TinyLog r
    )
  ) =>
  Local x ->
  Qualified UserId ->
  Maybe ClientId ->
  Qualified ConvId ->
  Maybe ConnId ->
  RawMLS SomeMessage ->
  Sem r ([LocalConversationUpdate], UnreachableUserList)
postMLSMessage loc qusr mc qcnv con smsg =
  case rmValue smsg of
    SomeMessage tag msg -> do
      mSender <- fmap ciClient <$> getSenderIdentity qusr mc tag msg
      foldQualified
        loc
        (postMLSMessageToLocalConv qusr mSender con smsg)
        (postMLSMessageToRemoteConv loc qusr mSender con smsg)
        qcnv

-- Check that the MLS client who created the message belongs to the user who
-- is the sender of the REST request, identified by HTTP header.
--
-- The check is skipped in case of conversation creation and encrypted messages.
getSenderClient ::
  ( Member (ErrorS 'MLSKeyPackageRefNotFound) r,
    Member (ErrorS 'MLSClientSenderUserMismatch) r,
    Member BrigAccess r
  ) =>
  Qualified UserId ->
  SWireFormatTag tag ->
  Message tag ->
  Sem r (Maybe ClientId)
getSenderClient _ SMLSCipherText _ = pure Nothing
getSenderClient _ _ msg | msgEpoch msg == Epoch 0 = pure Nothing
getSenderClient qusr SMLSPlainText msg = case msgSender msg of
  PreconfiguredSender _ -> pure Nothing
  NewMemberSender -> pure Nothing
  MemberSender ref -> do
    cid <- derefKeyPackage ref
    when (fmap fst (cidQualifiedClient cid) /= qusr) $
      throwS @'MLSClientSenderUserMismatch
    pure (Just (ciClient cid))

-- FUTUREWORK: once we can assume that the Z-Client header is present (i.e.
-- when v2 is dropped), remove the Maybe in the return type.
getSenderIdentity ::
  ( Member (ErrorS 'MLSKeyPackageRefNotFound) r,
    Member (ErrorS 'MLSClientSenderUserMismatch) r,
    Member BrigAccess r
  ) =>
  Qualified UserId ->
  Maybe ClientId ->
  SWireFormatTag tag ->
  Message tag ->
  Sem r (Maybe ClientIdentity)
getSenderIdentity qusr mc fmt msg = do
  mSender <- getSenderClient qusr fmt msg
  -- At this point, mc is the client ID of the request, while mSender is the
  -- one contained in the message. We throw an error if the two don't match.
  when (((==) <$> mc <*> mSender) == Just False) $
    throwS @'MLSClientSenderUserMismatch
  pure (mkClientIdentity qusr <$> (mc <|> mSender))

postMLSMessageToLocalConv ::
  ( HasProposalEffects r,
    ( Member (Error FederationError) r,
      Member (Error InternalError) r,
      Member (ErrorS 'ConvNotFound) r,
      Member (ErrorS 'MissingLegalholdConsent) r,
      Member (ErrorS 'MLSClientSenderUserMismatch) r,
      Member (ErrorS 'MLSCommitMissingReferences) r,
      Member (ErrorS 'MLSMissingSenderClient) r,
      Member (ErrorS 'MLSProposalNotFound) r,
      Member (ErrorS 'MLSSelfRemovalNotAllowed) r,
      Member (ErrorS 'MLSStaleMessage) r,
      Member (ErrorS 'MLSUnsupportedMessage) r,
      Member ProposalStore r,
      Member Resource r,
      Member TinyLog r
    )
  ) =>
  Qualified UserId ->
  Maybe ClientId ->
  Maybe ConnId ->
  RawMLS SomeMessage ->
  Local ConvId ->
  Sem r ([LocalConversationUpdate], UnreachableUserList)
postMLSMessageToLocalConv qusr senderClient con smsg lcnv =
  case rmValue smsg of
    SomeMessage tag msg -> do
      conv <- getLocalConvForUser qusr lcnv
      mlsMeta <- Data.mlsMetadata conv & noteS @'ConvNotFound

      -- construct client map
      cm <- lookupMLSClients (cnvmlsGroupId mlsMeta)
      let lconv = qualifyAs lcnv conv

      -- validate message
      events <- case tag of
        SMLSPlainText -> case msgPayload msg of
          CommitMessage c ->
            processCommit qusr senderClient con lconv mlsMeta cm (msgEpoch msg) (msgSender msg) c
          ApplicationMessage _ -> throwS @'MLSUnsupportedMessage
          ProposalMessage prop ->
            processProposal qusr conv mlsMeta msg prop $> mempty
        SMLSCipherText -> case toMLSEnum' (msgContentType (msgPayload msg)) of
          Right CommitMessageTag -> throwS @'MLSUnsupportedMessage
          Right ProposalMessageTag -> throwS @'MLSUnsupportedMessage
          Right ApplicationMessageTag -> pure mempty
          Left _ -> throwS @'MLSUnsupportedMessage

      -- forward message
      unreachables <- propagateMessage qusr lconv cm con (rmRaw smsg)
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
  Maybe ClientId ->
  Maybe ConnId ->
  RawMLS SomeMessage ->
  Remote ConvId ->
  Sem r ([LocalConversationUpdate], UnreachableUserList)
postMLSMessageToRemoteConv loc qusr _senderClient con smsg rcnv = do
  -- only local users can send messages to remote conversations
  lusr <- foldQualified loc pure (\_ -> throwS @'ConvAccessDenied) qusr
  -- only members may send messages to the remote conversation

  flip unless (throwS @'ConvMemberNotFound) =<< checkLocalMemberRemoteConv (tUnqualified lusr) rcnv
  resp <-
    runFederated rcnv $
      fedClient @'Galley @"send-mls-message" $
        MLSMessageSendRequest
          { mmsrConvOrSubId = Conv $ tUnqualified rcnv,
            mmsrSender = tUnqualified lusr,
            mmsrRawMessage = Base64ByteString (rmRaw smsg)
          }
  case resp of
    MLSMessageResponseError e -> rethrowErrors @MLSMessageStaticErrors e
    MLSMessageResponseProtocolError e ->
      throw (mlsProtocolError e)
    MLSMessageResponseProposalFailure e -> throw (MLSProposalFailure e)
    MLSMessageResponseUpdates updates unreachables -> do
      lcus <- for updates $ \update -> do
        e <- notifyRemoteConversationAction loc (qualifyAs rcnv update) con
        pure (LocalConversationUpdate e update)

      pure (lcus, unreachables)

type HasProposalEffects r =
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member (Error InternalError) r,
    Member (Error MLSProposalFailure) r,
    Member (Error MLSProtocolError) r,
    Member (ErrorS 'MLSClientMismatch) r,
    Member (ErrorS 'MLSKeyPackageRefNotFound) r,
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

instance Semigroup ProposalAction where
  ProposalAction add1 rem1 init1 <> ProposalAction add2 rem2 init2 =
    ProposalAction
      (Map.unionWith mappend add1 add2)
      (Map.unionWith mappend rem1 rem2)
      (init1 <> init2)

instance Monoid ProposalAction where
  mempty = ProposalAction mempty mempty mempty

paAddClient :: Qualified (UserId, (ClientId, KeyPackageRef)) -> ProposalAction
paAddClient quc = mempty {paAdd = Map.singleton (fmap fst quc) (Set.singleton (snd (qUnqualified quc)))}

paRemoveClient :: Qualified (UserId, (ClientId, KeyPackageRef)) -> ProposalAction
paRemoveClient quc = mempty {paRemove = Map.singleton (fmap fst quc) (Set.singleton (snd (qUnqualified quc)))}

paExternalInitPresent :: ProposalAction
paExternalInitPresent = mempty {paExternalInit = Any True}

getCommitData ::
  ( HasProposalEffects r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MLSProposalNotFound) r,
    Member (ErrorS 'MLSStaleMessage) r
  ) =>
  Local Data.Conversation ->
  ConversationMLSData ->
  Epoch ->
  Commit ->
  Sem r ProposalAction
getCommitData lconv mlsMeta epoch commit = do
  let curEpoch = cnvmlsEpoch mlsMeta
      groupId = cnvmlsGroupId mlsMeta
      suite = cnvmlsCipherSuite mlsMeta

  -- check epoch number
  when (epoch /= curEpoch) $ throwS @'MLSStaleMessage
  foldMap (applyProposalRef (tUnqualified lconv) mlsMeta groupId epoch suite) (cProposals commit)

processCommit ::
  ( HasProposalEffects r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MLSClientSenderUserMismatch) r,
    Member (ErrorS 'MLSCommitMissingReferences) r,
    Member (ErrorS 'MLSMissingSenderClient) r,
    Member (ErrorS 'MLSProposalNotFound) r,
    Member (ErrorS 'MLSSelfRemovalNotAllowed) r,
    Member (ErrorS 'MLSStaleMessage) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member Resource r
  ) =>
  Qualified UserId ->
  Maybe ClientId ->
  Maybe ConnId ->
  Local Data.Conversation ->
  ConversationMLSData ->
  ClientMap ->
  Epoch ->
  Sender 'MLSPlainText ->
  Commit ->
  Sem r [LocalConversationUpdate]
processCommit qusr senderClient con lconv mlsMeta cm epoch sender commit = do
  action <- getCommitData lconv mlsMeta epoch commit
  processCommitWithAction qusr senderClient con lconv mlsMeta cm epoch action sender commit

processExternalCommit ::
  forall r.
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member (Error MLSProtocolError) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MLSClientSenderUserMismatch) r,
    Member (ErrorS 'MLSKeyPackageRefNotFound) r,
    Member (ErrorS 'MLSStaleMessage) r,
    Member (ErrorS 'MLSMissingSenderClient) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input Env) r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member ProposalStore r,
    Member Resource r,
    Member TinyLog r
  ) =>
  Qualified UserId ->
  Maybe ClientId ->
  Local Data.Conversation ->
  ConversationMLSData ->
  ClientMap ->
  Epoch ->
  ProposalAction ->
  Maybe UpdatePath ->
  Sem r ()
processExternalCommit qusr mSenderClient lconv mlsMeta cm epoch action updatePath = withCommitLock (cnvmlsGroupId mlsMeta) epoch $ do
  newKeyPackage <-
    upLeaf
      <$> note
        (mlsProtocolError "External commits need an update path")
        updatePath
  when (paExternalInit action == mempty) $
    throw . mlsProtocolError $
      "The external commit is missing an external init proposal"
  unless (paAdd action == mempty) $
    throw . mlsProtocolError $
      "The external commit must not have add proposals"

  newRef <-
    kpRef' newKeyPackage
      & note (mlsProtocolError "An invalid key package in the update path")

  -- validate and update mapping in brig
  eithCid <-
    nkpresClientIdentity
      <$$> validateAndAddKeyPackageRef
        NewKeyPackage
          { nkpConversation = Data.convId <$> tUntagged lconv,
            nkpKeyPackage = KeyPackageData (rmRaw newKeyPackage)
          }
  cid <- either (\errMsg -> throw (mlsProtocolError ("Tried to add invalid KeyPackage: " <> errMsg))) pure eithCid

  unless (cidQualifiedUser cid == qusr) $
    throw . mlsProtocolError $
      "The external commit attempts to add another user"

  senderClient <- noteS @'MLSMissingSenderClient mSenderClient

  unless (ciClient cid == senderClient) $
    throw . mlsProtocolError $
      "The external commit attempts to add another client of the user, it must only add itself"

  -- check if there is a key package ref in the remove proposal
  remRef <-
    if Map.null (paRemove action)
      then pure Nothing
      else do
        (remCid, r) <- derefUser (paRemove action) qusr
        unless (cidQualifiedUser cid == cidQualifiedUser remCid)
          . throw
          . mlsProtocolError
          $ "The external commit attempts to remove a client from a user other than themselves"
        pure (Just r)

  updateKeyPackageMapping lconv (cnvmlsGroupId mlsMeta) qusr (ciClient cid) remRef newRef

  -- increment epoch number
  setConversationEpoch (Data.convId (tUnqualified lconv)) (succ epoch)
  -- fetch local conversation with new epoch
  lc <- qualifyAs lconv <$> getLocalConvForUser qusr (convId <$> lconv)
  -- fetch backend remove proposals of the previous epoch
  kpRefs <- getPendingBackendRemoveProposals (cnvmlsGroupId mlsMeta) epoch
  -- requeue backend remove proposals for the current epoch
  removeClientsWithClientMap lc kpRefs cm qusr
  where
    derefUser :: ClientMap -> Qualified UserId -> Sem r (ClientIdentity, KeyPackageRef)
    derefUser (Map.toList -> l) user = case l of
      [(u, s)] -> do
        unless (user == u) $
          throwS @'MLSClientSenderUserMismatch
        ref <- snd <$> ensureSingleton s
        ci <- derefKeyPackage ref
        unless (cidQualifiedUser ci == user) $
          throwS @'MLSClientSenderUserMismatch
        pure (ci, ref)
      _ -> throwRemProposal
    ensureSingleton :: Set a -> Sem r a
    ensureSingleton (Set.toList -> l) = case l of
      [e] -> pure e
      _ -> throwRemProposal
    throwRemProposal =
      throw . mlsProtocolError $
        "The external commit must have at most one remove proposal"

processCommitWithAction ::
  forall r.
  ( HasProposalEffects r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MLSClientSenderUserMismatch) r,
    Member (ErrorS 'MLSCommitMissingReferences) r,
    Member (ErrorS 'MLSMissingSenderClient) r,
    Member (ErrorS 'MLSSelfRemovalNotAllowed) r,
    Member (ErrorS 'MLSStaleMessage) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member Resource r
  ) =>
  Qualified UserId ->
  Maybe ClientId ->
  Maybe ConnId ->
  Local Data.Conversation ->
  ConversationMLSData ->
  ClientMap ->
  Epoch ->
  ProposalAction ->
  Sender 'MLSPlainText ->
  Commit ->
  Sem r [LocalConversationUpdate]
processCommitWithAction qusr senderClient con lconv mlsMeta cm epoch action sender commit =
  case sender of
    MemberSender ref -> processInternalCommit qusr senderClient con lconv mlsMeta cm epoch action ref commit
    NewMemberSender -> processExternalCommit qusr senderClient lconv mlsMeta cm epoch action (cPath commit) $> []
    _ -> throw (mlsProtocolError "Unexpected sender")

processInternalCommit ::
  forall r.
  ( HasProposalEffects r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MLSCommitMissingReferences) r,
    Member (ErrorS 'MLSMissingSenderClient) r,
    Member (ErrorS 'MLSSelfRemovalNotAllowed) r,
    Member (ErrorS 'MLSStaleMessage) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member Resource r
  ) =>
  Qualified UserId ->
  Maybe ClientId ->
  Maybe ConnId ->
  Local Data.Conversation ->
  ConversationMLSData ->
  ClientMap ->
  Epoch ->
  ProposalAction ->
  KeyPackageRef ->
  Commit ->
  Sem r [LocalConversationUpdate]
processInternalCommit qusr senderClient con lconv mlsMeta cm epoch action senderRef commit = do
  self <- noteS @'ConvNotFound $ getConvMember lconv (tUnqualified lconv) qusr

  withCommitLock (cnvmlsGroupId mlsMeta) epoch $ do
    postponedKeyPackageRefUpdate <-
      if epoch == Epoch 0
        then do
          let cType = cnvmType . convMetadata . tUnqualified $ lconv
          case (self, cType, cmAssocs cm) of
            (Left _, SelfConv, []) -> do
              creatorClient <- noteS @'MLSMissingSenderClient senderClient
              creatorRef <-
                maybe
                  (pure senderRef)
                  ( note (mlsProtocolError "Could not compute key package ref")
                      . kpRef'
                      . upLeaf
                  )
                  $ cPath commit
              addMLSClients
                (cnvmlsGroupId mlsMeta)
                qusr
                (Set.singleton (creatorClient, creatorRef))
            (Left _, SelfConv, _) ->
              throw . InternalErrorWithDescription $
                "Unexpected creator client set in a self-conversation"
            -- this is a newly created conversation, and it should contain exactly one
            -- client (the creator)
            (Left lm, _, [(qu, (creatorClient, _))])
              | qu == tUntagged (qualifyAs lconv (lmId lm)) -> do
                  -- use update path as sender reference and if not existing fall back to sender
                  senderRef' <-
                    maybe
                      (pure senderRef)
                      ( note (mlsProtocolError "Could not compute key package ref")
                          . kpRef'
                          . upLeaf
                      )
                      $ cPath commit
                  -- register the creator client
                  updateKeyPackageMapping lconv (cnvmlsGroupId mlsMeta) qusr creatorClient Nothing senderRef'
            -- remote clients cannot send the first commit
            (Right _, _, _) -> throwS @'MLSStaleMessage
            -- uninitialised conversations should contain exactly one client
            (_, _, _) ->
              throw (InternalErrorWithDescription "Unexpected creator client set")
          pure $ pure () -- no key package ref update necessary
        else case upLeaf <$> cPath commit of
          Just updatedKeyPackage -> do
            updatedRef <- kpRef' updatedKeyPackage & note (mlsProtocolError "Could not compute key package ref")
            -- postpone key package ref update until other checks/processing passed
            case senderClient of
              Just cli -> pure (updateKeyPackageMapping lconv (cnvmlsGroupId mlsMeta) qusr cli (Just senderRef) updatedRef)
              Nothing -> pure (pure ())
          Nothing -> pure (pure ()) -- ignore commits without update path

    -- check all pending proposals are referenced in the commit
    allPendingProposals <- getAllPendingProposalRefs (cnvmlsGroupId mlsMeta) epoch
    let referencedProposals = Set.fromList $ mapMaybe (\x -> preview Proposal._Ref x) (cProposals commit)
    unless (all (`Set.member` referencedProposals) allPendingProposals) $
      throwS @'MLSCommitMissingReferences

    -- process and execute proposals
    updates <- executeProposalAction qusr con lconv mlsMeta cm action

    -- update key package ref if necessary
    postponedKeyPackageRefUpdate
    -- increment epoch number
    setConversationEpoch (Data.convId (tUnqualified lconv)) (succ epoch)

    pure updates

-- | Note: Use this only for KeyPackage that are already validated
updateKeyPackageMapping ::
  ( Member BrigAccess r,
    Member MemberStore r
  ) =>
  Local Data.Conversation ->
  GroupId ->
  Qualified UserId ->
  ClientId ->
  Maybe KeyPackageRef ->
  KeyPackageRef ->
  Sem r ()
updateKeyPackageMapping lconv groupId qusr cid mOld new = do
  let lcnv = fmap Data.convId lconv
  -- update actual mapping in brig
  case mOld of
    Nothing ->
      addKeyPackageRef new qusr cid (tUntagged lcnv)
    Just old ->
      updateKeyPackageRef
        KeyPackageUpdate
          { kpupPrevious = old,
            kpupNext = new
          }

  -- remove old (client, key package) pair
  removeMLSClients groupId qusr (Set.singleton cid)
  -- add new (client, key package) pair
  addMLSClients groupId qusr (Set.singleton (cid, new))

applyProposalRef ::
  ( HasProposalEffects r,
    ( Member (ErrorS 'ConvNotFound) r,
      Member (ErrorS 'MLSProposalNotFound) r,
      Member (ErrorS 'MLSStaleMessage) r,
      Member ProposalStore r
    )
  ) =>
  Data.Conversation ->
  ConversationMLSData ->
  GroupId ->
  Epoch ->
  CipherSuiteTag ->
  ProposalOrRef ->
  Sem r ProposalAction
applyProposalRef conv mlsMeta groupId epoch _suite (Ref ref) = do
  p <- getProposal groupId epoch ref >>= noteS @'MLSProposalNotFound
  checkEpoch epoch mlsMeta
  checkGroup groupId mlsMeta
  applyProposal (convId conv) groupId (rmValue p)
applyProposalRef conv _mlsMeta groupId _epoch suite (Inline p) = do
  checkProposalCipherSuite suite p
  applyProposal (convId conv) groupId p

applyProposal ::
  forall r.
  HasProposalEffects r =>
  ConvId ->
  GroupId ->
  Proposal ->
  Sem r ProposalAction
applyProposal convId groupId (AddProposal kp) = do
  ref <- kpRef' kp & note (mlsProtocolError "Could not compute ref of a key package in an Add proposal")
  mbClientIdentity <- getClientByKeyPackageRef ref
  clientIdentity <- case mbClientIdentity of
    Nothing -> do
      -- external add proposal for a new key package unknown to the backend
      lconvId <- qualifyLocal convId
      addKeyPackageMapping lconvId ref (KeyPackageData (rmRaw kp))
    Just ci ->
      -- ad-hoc add proposal in commit, the key package has been claimed before
      pure ci
  pure (paAddClient . (<$$>) (,ref) . cidQualifiedClient $ clientIdentity)
  where
    addKeyPackageMapping :: Local ConvId -> KeyPackageRef -> KeyPackageData -> Sem r ClientIdentity
    addKeyPackageMapping lconv ref kpdata = do
      -- validate and update mapping in brig
      eithCid <-
        nkpresClientIdentity
          <$$> validateAndAddKeyPackageRef
            NewKeyPackage
              { nkpConversation = tUntagged lconv,
                nkpKeyPackage = kpdata
              }
      cid <- either (\errMsg -> throw (mlsProtocolError ("Tried to add invalid KeyPackage: " <> errMsg))) pure eithCid
      let qcid = cidQualifiedClient cid
      let qusr = fst <$> qcid
      -- update mapping in galley
      addMLSClients groupId qusr (Set.singleton (ciClient cid, ref))
      pure cid
applyProposal _conv _groupId (RemoveProposal ref) = do
  qclient <- cidQualifiedClient <$> derefKeyPackage ref
  pure (paRemoveClient ((,ref) <$$> qclient))
applyProposal _conv _groupId (ExternalInitProposal _) =
  -- only record the fact there was an external init proposal, but do not
  -- process it in any way.
  pure paExternalInitPresent
applyProposal _conv _groupId _ = pure mempty

checkProposalCipherSuite ::
  Member (Error MLSProtocolError) r =>
  CipherSuiteTag ->
  Proposal ->
  Sem r ()
checkProposalCipherSuite suite (AddProposal kpRaw) = do
  let kp = rmValue kpRaw
  unless (kpCipherSuite kp == tagCipherSuite suite)
    . throw
    . mlsProtocolError
    . T.pack
    $ "The group's cipher suite "
      <> show (cipherSuiteNumber (tagCipherSuite suite))
      <> " and the cipher suite of the proposal's key package "
      <> show (cipherSuiteNumber (kpCipherSuite kp))
      <> " do not match."
checkProposalCipherSuite _suite _prop = pure ()

processProposal ::
  HasProposalEffects r =>
  ( Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MLSStaleMessage) r
  ) =>
  Qualified UserId ->
  Data.Conversation ->
  ConversationMLSData ->
  Message 'MLSPlainText ->
  RawMLS Proposal ->
  Sem r ()
processProposal qusr conv mlsMeta msg prop = do
  checkEpoch (msgEpoch msg) mlsMeta
  checkGroup (msgGroupId msg) mlsMeta
  let suiteTag = cnvmlsCipherSuite mlsMeta

  -- validate the proposal
  --
  -- is the user a member of the conversation?
  loc <- qualifyLocal ()
  isMember' <-
    foldQualified
      loc
      ( fmap isJust
          . getLocalMember (convId conv)
          . tUnqualified
      )
      ( fmap isJust
          . getRemoteMember (convId conv)
      )
      qusr
  unless isMember' $ throwS @'ConvNotFound

  -- FUTUREWORK: validate the member's conversation role
  let propValue = rmValue prop
  checkProposalCipherSuite suiteTag propValue
  when (isExternalProposal msg) $ do
    checkExternalProposalSignature suiteTag msg prop
    checkExternalProposalUser qusr propValue
  let propRef = proposalRef suiteTag prop
  storeProposal (msgGroupId msg) (msgEpoch msg) propRef ProposalOriginClient prop

checkExternalProposalSignature ::
  Member (ErrorS 'MLSUnsupportedProposal) r =>
  CipherSuiteTag ->
  Message 'MLSPlainText ->
  RawMLS Proposal ->
  Sem r ()
checkExternalProposalSignature csTag msg prop = case rmValue prop of
  AddProposal kp -> do
    let pubKey = bcSignatureKey . kpCredential $ rmValue kp
    unless (verifyMessageSignature csTag msg pubKey) $ throwS @'MLSUnsupportedProposal
  _ -> pure () -- FUTUREWORK: check signature of other proposals as well

isExternalProposal :: Message 'MLSPlainText -> Bool
isExternalProposal msg = case msgSender msg of
  NewMemberSender -> True
  PreconfiguredSender _ -> True
  _ -> False

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
        AddProposal keyPackage -> do
          ClientIdentity {ciUser, ciClient} <-
            either
              (const $ throwS @'MLSUnsupportedProposal)
              pure
              . kpIdentity
              . rmValue
              $ keyPackage
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

executeProposalAction ::
  forall r.
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member (Error InternalError) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MLSClientMismatch) r,
    Member (Error MLSProposalFailure) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'MLSUnsupportedProposal) r,
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
    Member TeamStore r,
    Member TinyLog r
  ) =>
  Qualified UserId ->
  Maybe ConnId ->
  Local Data.Conversation ->
  ConversationMLSData ->
  ClientMap ->
  ProposalAction ->
  Sem r [LocalConversationUpdate]
executeProposalAction qusr con lconv mlsMeta cm action = do
  let ss = csSignatureScheme (cnvmlsCipherSuite mlsMeta)
      newUserClients = Map.assocs (paAdd action)

  -- Note [client removal]
  -- We support two types of removals:
  --  1. when a user is removed from a group, all their clients have to be removed
  --  2. when a client is deleted, that particular client (but not necessarily
  --     other clients of the same user), has to be removed.
  --
  -- Type 2 requires no special processing on the backend, so here we filter
  -- out all removals of that type, so that further checks and processing can
  -- be applied only to type 1 removals.
  removedUsers <- mapMaybe hush <$$> for (Map.assocs (paRemove action)) $
    \(qtarget, Set.map fst -> clients) -> runError @() $ do
      -- fetch clients from brig
      clientInfo <- Set.map ciId <$> getClientInfo lconv qtarget ss
      -- if the clients being removed don't exist, consider this as a removal of
      -- type 2, and skip it
      when (Set.null (clientInfo `Set.intersection` clients)) $
        throw ()
      pure (qtarget, clients)

  -- FUTUREWORK: remove this check after remote admins are implemented in federation https://wearezeta.atlassian.net/browse/FS-216
  foldQualified lconv (\_ -> pure ()) (\_ -> throwS @'MLSUnsupportedProposal) qusr

  -- for each user, we compare their clients with the ones being added to the conversation
  for_ newUserClients $ \(qtarget, newclients) -> case Map.lookup qtarget cm of
    -- user is already present, skip check in this case
    Just _ -> pure ()
    -- new user
    Nothing -> do
      -- final set of clients in the conversation
      let clients = Set.map fst (newclients <> Map.findWithDefault mempty qtarget cm)
      -- get list of mls clients from brig
      clientInfo <- getClientInfo lconv qtarget ss
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

  membersToRemove <- catMaybes <$> for removedUsers (uncurry checkRemoval)

  -- add users to the conversation and send events
  addEvents <- foldMap addMembers . nonEmpty . map fst $ newUserClients

  -- add clients in the conversation state
  for_ newUserClients $ \(qtarget, newClients) -> do
    addMLSClients (cnvmlsGroupId mlsMeta) qtarget newClients

  -- remove users from the conversation and send events
  removeEvents <- foldMap removeMembers (nonEmpty membersToRemove)

  -- Remove clients from the conversation state. This includes client removals
  -- of all types (see Note [client removal]).
  for_ (Map.assocs (paRemove action)) $ \(qtarget, clients) -> do
    removeMLSClients (cnvmlsGroupId mlsMeta) qtarget (Set.map fst clients)

  pure (addEvents <> removeEvents)
  where
    checkRemoval ::
      Qualified UserId ->
      Set ClientId ->
      Sem r (Maybe (Qualified UserId))
    checkRemoval qtarget clients = do
      let clientsInConv = Set.map fst (Map.findWithDefault mempty qtarget cm)
      when (clients /= clientsInConv) $ do
        -- FUTUREWORK: turn this error into a proper response
        throwS @'MLSClientMismatch
      when (qusr == qtarget) $
        throwS @'MLSSelfRemovalNotAllowed
      pure (Just qtarget)

    existingLocalMembers :: Set (Qualified UserId)
    existingLocalMembers =
      (Set.fromList . map (fmap lmId . tUntagged)) (traverse convLocalMembers lconv)

    existingRemoteMembers :: Set (Qualified UserId)
    existingRemoteMembers =
      Set.fromList . map (tUntagged . rmId) . convRemoteMembers . tUnqualified $
        lconv

    existingMembers :: Set (Qualified UserId)
    existingMembers = existingLocalMembers <> existingRemoteMembers

    addMembers :: NonEmpty (Qualified UserId) -> Sem r [LocalConversationUpdate]
    addMembers =
      -- FUTUREWORK: update key package ref mapping to reflect conversation membership
      foldMap
        ( handleNoChanges
            . handleMLSProposalFailures @ProposalErrors
            . fmap pure
            . updateLocalConversationUnchecked @'ConversationJoinTag lconv qusr con
            . flip ConversationJoin roleNameWireMember
        )
        . nonEmpty
        . filter (flip Set.notMember existingMembers)
        . toList

    removeMembers :: NonEmpty (Qualified UserId) -> Sem r [LocalConversationUpdate]
    removeMembers =
      foldMap
        ( handleNoChanges
            . handleMLSProposalFailures @ProposalErrors
            . fmap pure
            . updateLocalConversationUnchecked @'ConversationRemoveMembersTag lconv qusr con
        )
        . nonEmpty
        . filter (flip Set.member existingMembers)
        . toList

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

withCommitLock ::
  forall r a.
  ( Member Resource r,
    Member ConversationStore r,
    Member (ErrorS 'MLSStaleMessage) r
  ) =>
  GroupId ->
  Epoch ->
  Sem r a ->
  Sem r a
withCommitLock gid epoch action =
  bracket
    ( acquireCommitLock gid epoch ttl >>= \lockAcquired ->
        when (lockAcquired == NotAcquired) $
          throwS @'MLSStaleMessage
    )
    (const $ releaseCommitLock gid epoch)
    $ \_ -> do
      -- FUTUREWORK: fetch epoch again and check that is matches
      action
  where
    ttl = fromIntegral (600 :: Int) -- 10 minutes

storeGroupInfoBundle ::
  Member ConversationStore r =>
  Local Data.Conversation ->
  GroupInfoBundle ->
  Sem r ()
storeGroupInfoBundle lconv =
  setPublicGroupState (Data.convId (tUnqualified lconv))
    . toOpaquePublicGroupState
    . gipGroupState
