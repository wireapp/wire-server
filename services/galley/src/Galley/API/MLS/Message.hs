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
{-# LANGUAGE RecordWildCards #-}

module Galley.API.MLS.Message
  ( postMLSMessageFromLocalUser,
    postMLSMessage,
    MLSMessageStaticErrors,
  )
where

import Control.Comonad
import Control.Lens (preview, to)
import Data.Bifunctor
import Data.Domain
import Data.Id
import Data.Json.Util
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time
import Galley.API.Action
import Galley.API.Error
import Galley.API.MLS.KeyPackage
import Galley.API.Push
import Galley.API.Util
import Galley.Data.Conversation.Types hiding (Conversation)
import qualified Galley.Data.Conversation.Types as Data
import Galley.Data.Services
import Galley.Data.Types
import Galley.Effects
import Galley.Effects.BrigAccess
import Galley.Effects.ConversationStore
import Galley.Effects.FederatorAccess
import Galley.Effects.MemberStore
import Galley.Effects.ProposalStore
import Galley.Options
import Galley.Types.Conversations.Members
import Imports
import Network.Wai.Utilities.Server
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal
import Polysemy.Resource (Resource, bracket)
import Polysemy.TinyLog
import qualified System.Logger.Class as Logger
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
import Wire.API.MLS.Credential
import Wire.API.MLS.Group
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation
import Wire.API.Message

type MLSMessageStaticErrors =
  '[ ErrorS 'ConvAccessDenied,
     ErrorS 'ConvNotFound,
     ErrorS 'MLSUnsupportedMessage,
     ErrorS 'MLSStaleMessage,
     ErrorS 'MLSProposalNotFound,
     ErrorS 'MissingLegalholdConsent,
     ErrorS 'MLSKeyPackageRefNotFound,
     ErrorS 'MLSClientMismatch,
     ErrorS 'MLSUnsupportedProposal
   ]

postMLSMessageFromLocalUser ::
  ( HasProposalEffects r,
    Members
      '[ Resource,
         Error FederationError,
         ErrorS 'ConvAccessDenied,
         ErrorS 'ConvNotFound,
         Error InternalError,
         ErrorS 'MLSProposalNotFound,
         ErrorS 'MLSUnsupportedMessage,
         ErrorS 'MLSStaleMessage,
         ErrorS 'MissingLegalholdConsent,
         ProposalStore,
         TinyLog,
         Input (Local ())
       ]
      r
  ) =>
  Local UserId ->
  ConnId ->
  RawMLS SomeMessage ->
  Sem r [Event]
postMLSMessageFromLocalUser lusr conn msg =
  map lcuEvent
    <$> postMLSMessage lusr (qUntagged lusr) (Just conn) msg

postMLSMessage ::
  ( HasProposalEffects r,
    Members
      '[ Error FederationError,
         Error InternalError,
         ErrorS 'ConvAccessDenied,
         ErrorS 'ConvNotFound,
         ErrorS 'MLSUnsupportedMessage,
         ErrorS 'MLSStaleMessage,
         ErrorS 'MLSProposalNotFound,
         ErrorS 'MissingLegalholdConsent,
         Resource,
         TinyLog,
         ProposalStore,
         Input (Local ())
       ]
      r
  ) =>
  Local x ->
  Qualified UserId ->
  Maybe ConnId ->
  RawMLS SomeMessage ->
  Sem r [LocalConversationUpdate]
postMLSMessage loc qusr con smsg = case rmValue smsg of
  SomeMessage _ msg -> do
    -- fetch conversation ID
    qcnv <- getConversationIdByGroupId (msgGroupId msg) >>= noteS @'ConvNotFound
    foldQualified
      loc
      (postMLSMessageToLocalConv qusr con smsg)
      (postMLSMessageToRemoteConv loc qusr con smsg)
      qcnv

postMLSMessageToLocalConv ::
  ( HasProposalEffects r,
    Members
      '[ Error FederationError,
         Error InternalError,
         ErrorS 'ConvNotFound,
         ErrorS 'MLSUnsupportedMessage,
         ErrorS 'MLSStaleMessage,
         ErrorS 'MLSProposalNotFound,
         ErrorS 'MissingLegalholdConsent,
         Resource,
         TinyLog,
         ProposalStore,
         Input (Local ())
       ]
      r
  ) =>
  Qualified UserId ->
  Maybe ConnId ->
  RawMLS SomeMessage ->
  Local ConvId ->
  Sem r [LocalConversationUpdate]
postMLSMessageToLocalConv qusr con smsg lcnv = case rmValue smsg of
  SomeMessage tag msg -> do
    conv <- getConversation (tUnqualified lcnv) >>= noteS @'ConvNotFound

    -- validate message
    events <- case tag of
      SMLSPlainText -> case msgTBS (msgPayload msg) of
        CommitMessage c ->
          processCommit qusr con (qualifyAs lcnv conv) (msgEpoch msg) (msgSender msg) c
        ApplicationMessage _ -> throwS @'MLSUnsupportedMessage
        ProposalMessage prop ->
          processProposal qusr conv msg prop $> mempty
      SMLSCipherText -> case toMLSEnum' (msgContentType (msgPayload msg)) of
        Right CommitMessageTag -> throwS @'MLSUnsupportedMessage
        Right ProposalMessageTag -> throwS @'MLSUnsupportedMessage
        Right ApplicationMessageTag -> pure mempty
        Left _ -> throwS @'MLSUnsupportedMessage

    -- forward message
    propagateMessage lcnv qusr conv con (rmRaw smsg)

    pure events

postMLSMessageToRemoteConv ::
  ( Members MLSMessageStaticErrors r,
    Members '[Error FederationError, TinyLog] r,
    HasProposalEffects r
  ) =>
  Local x ->
  Qualified UserId ->
  Maybe ConnId ->
  RawMLS SomeMessage ->
  Remote ConvId ->
  Sem r [LocalConversationUpdate]
postMLSMessageToRemoteConv loc qusr con smsg rcnv = do
  -- only local users can send messages to remote conversations
  lusr <- foldQualified loc pure (\_ -> throwS @'ConvAccessDenied) qusr
  resp <-
    runFederated rcnv $
      fedClient @'Galley @"send-mls-message" $
        MessageSendRequest
          { msrConvId = tUnqualified rcnv,
            msrSender = tUnqualified lusr,
            msrRawMessage = Base64ByteString (rmRaw smsg)
          }
  updates <- case resp of
    MLSMessageResponseError e -> rethrowErrors @MLSMessageStaticErrors e
    MLSMessageResponseProtocolError e -> throw (mlsProtocolError e)
    MLSMessageResponseProposalFailure e -> throw (MLSProposalFailure e)
    MLSMessageResponseUpdates updates -> pure updates

  for updates $ \update -> do
    e <- notifyRemoteConversationAction loc (qualifyAs rcnv update) con
    pure (LocalConversationUpdate e update)

type HasProposalEffects r =
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member (Error MLSProposalFailure) r,
    Member (Error MLSProtocolError) r,
    Member (ErrorS 'MLSKeyPackageRefNotFound) r,
    Member (ErrorS 'MLSClientMismatch) r,
    Member (ErrorS 'MLSUnsupportedProposal) r,
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
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MLSStaleMessage) r,
    Member (ErrorS 'MLSProposalNotFound) r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member Resource r,
    Member ProposalStore r
  ) =>
  Qualified UserId ->
  Maybe ConnId ->
  Local Data.Conversation ->
  Epoch ->
  Sender 'MLSPlainText ->
  Commit ->
  Sem r [LocalConversationUpdate]
processCommit qusr con lconv epoch sender commit = do
  self <- noteS @'ConvNotFound $ getConvMember lconv (tUnqualified lconv) qusr

  -- check epoch number
  convMeta <-
    preview (to convProtocol . _ProtocolMLS) (tUnqualified lconv)
      & noteS @'ConvNotFound

  let curEpoch = cnvmlsEpoch convMeta
      groupId = cnvmlsGroupId convMeta

  when (epoch /= curEpoch) $ throwS @'MLSStaleMessage

  let ttlSeconds :: Int = 600 -- 10 minutes
  withCommitLock groupId epoch (fromIntegral ttlSeconds) $ do
    when (epoch == Epoch 0) $ do
      -- this is a newly created conversation, and it should contain exactly one
      -- client (the creator)
      case (sender, first (toList . lmMLSClients) self) of
        (MemberSender ref, Left [creatorClient]) -> do
          -- register the creator client
          addKeyPackageRef
            ref
            qusr
            creatorClient
            (qUntagged (fmap Data.convId lconv))
        -- remote clients cannot send the first commit
        (_, Right _) -> throwS @'MLSStaleMessage
        -- uninitialised conversations should contain exactly one client
        (MemberSender _, _) ->
          throw (InternalErrorWithDescription "Unexpected creator client set")
        -- the sender of the first commit must be a member
        _ -> throw (mlsProtocolError "Unexpected sender")

    -- process and execute proposals
    action <- foldMap applyProposalRef (cProposals commit)
    updates <- executeProposalAction qusr con lconv action

    -- increment epoch number
    setConversationEpoch (Data.convId (tUnqualified lconv)) (succ epoch)

    pure updates

applyProposalRef ::
  ( HasProposalEffects r,
    Members
      '[ ErrorS 'MLSProposalNotFound,
         ProposalStore
       ]
      r
  ) =>
  ProposalOrRef ->
  Sem r ProposalAction
applyProposalRef (Ref ref) = do
  (p, _gid, _epoch) <- getProposal ref >>= noteS @'MLSProposalNotFound
  applyProposal (rmValue p)
applyProposalRef (Inline p) = applyProposal p

applyProposal :: HasProposalEffects r => Proposal -> Sem r ProposalAction
applyProposal (AddProposal kp) = do
  ref <-
    kpRef' kp
      & note (mlsProtocolError "Could not compute ref of a key package in an Add proposal")
  qclient <- cidQualifiedClient <$> derefKeyPackage ref
  pure (paClient qclient)
applyProposal _ = throwS @'MLSUnsupportedProposal

checkProposal ::
  Members
    '[ Error MLSProtocolError,
       ProposalStore
     ]
    r =>
  CipherSuite ->
  Proposal ->
  ProposalRef ->
  Sem r ()
checkProposal suite (AddProposal kpRaw) ref = do
  let kp = rmValue kpRaw
  unless (kpCipherSuite kp == suite)
    . throw
    . mlsProtocolError
    . T.pack
    $ "The group's cipher suite "
      <> show (cipherSuiteNumber suite)
      <> " and the cipher suite of the proposal's key package "
      <> show (cipherSuiteNumber (kpCipherSuite kp))
      <> " do not match."
  unlessM (isNothing <$> getProposal ref)
    . throw
    . mlsProtocolError
    . T.pack
    $ "A proposal with reference " <> show ref <> " already exists."
checkProposal _suite _prop _ref = pure ()

processProposal ::
  HasProposalEffects r =>
  Members
    '[ Error MLSProtocolError,
       ErrorS 'ConvNotFound,
       ErrorS 'MLSStaleMessage,
       ProposalStore,
       Input (Local ())
     ]
    r =>
  Qualified UserId ->
  Data.Conversation ->
  Message 'MLSPlainText ->
  RawMLS Proposal ->
  Sem r ()
processProposal qusr conv msg prop = do
  -- check epoch number
  curEpoch <-
    preview (to convProtocol . _ProtocolMLS . to cnvmlsEpoch) conv
      & noteS @'ConvNotFound
  unless (msgEpoch msg == curEpoch) $ throwS @'MLSStaleMessage
  -- check group ID
  groupId <-
    preview (to convProtocol . _ProtocolMLS . to cnvmlsGroupId) conv
      & noteS @'ConvNotFound
  unless (msgGroupId msg == groupId) $ throwS @'ConvNotFound

  suite <-
    preview (to convProtocol . _ProtocolMLS . to cnvmlsCipherSuite) conv
      & noteS @'ConvNotFound
  suiteTag <-
    cipherSuiteTag suite
      & note
        ( mlsProtocolError
            . T.pack
            . ("An unknown cipher suite associated with a conversation: " <>)
            . show
            . cipherSuiteNumber
            $ suite
        )
  let propRef = proposalRef suiteTag prop

  -- validate the proposal
  --
  -- is the user a member of the conversation?
  loc <- qualifyLocal ()
  isMember' <- foldQualified loc (fmap isJust . getLocalMember (convId conv) . tUnqualified) (fmap isJust . getRemoteMember (convId conv)) qusr
  unless isMember' $ throwS @'ConvNotFound

  -- FUTUREWORK: validate the member's conversation role
  checkProposal suite (rmValue prop) propRef

  storeProposal propRef prop (msgGroupId msg) (msgEpoch msg)

executeProposalAction ::
  forall r.
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (Error FederationError) r,
    Member (ErrorS 'MLSClientMismatch) r,
    Member (Error MLSProposalFailure) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'MLSUnsupportedProposal) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member MemberStore r,
    Member TeamStore r
  ) =>
  Qualified UserId ->
  Maybe ConnId ->
  Local Data.Conversation ->
  ProposalAction ->
  Sem r [LocalConversationUpdate]
executeProposalAction qusr con lconv action = do
  -- For the moment, assume a fixed ciphersuite.
  -- FUTUREWORK: store ciphersuite with the conversation
  let cs = MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
      ss = csSignatureScheme cs
      cm = convClientMap lconv
      newUserClients = Map.assocs (paAdd action)
  -- check that all clients of each user are added to the conversation, and
  -- update the database accordingly
  traverse_ (uncurry (addUserClients ss cm)) newUserClients
  -- FUTUREWORK: remove this check after remote admins are implemented in federation https://wearezeta.atlassian.net/browse/FS-216
  foldQualified lconv (\_ -> pure ()) (\_ -> throwS @'MLSUnsupportedProposal) qusr
  -- add users to the conversation and send events
  result <- foldMap addMembers . nonEmpty . map fst $ newUserClients
  -- add clients to the database
  for_ newUserClients $ \(qtarget, newClients) -> do
    addMLSClients (fmap convId lconv) qtarget newClients
  pure result
  where
    addUserClients :: SignatureSchemeTag -> ClientMap -> Qualified UserId -> Set ClientId -> Sem r ()
    addUserClients ss cm qtarget newClients = do
      -- compute final set of clients in the conversation
      let cs = newClients <> Map.findWithDefault mempty qtarget cm
      -- get list of mls clients from brig
      allClients <- getMLSClients lconv qtarget ss
      -- if not all clients have been added to the conversation, return an error
      when (cs /= allClients) $ do
        -- FUTUREWORK: turn this error into a proper response
        throwS @'MLSClientMismatch

    addMembers :: NonEmpty (Qualified UserId) -> Sem r [LocalConversationUpdate]
    addMembers users =
      -- FUTUREWORK: update key package ref mapping to reflect conversation membership
      handleNoChanges
        . handleMLSProposalFailures @ProposalErrors
        . fmap (pure)
        . updateLocalConversationUnchecked
          @'ConversationJoinTag
          lconv
          qusr
          con
        $ ConversationJoin users roleNameWireMember

handleNoChanges :: Monoid a => Sem (Error NoChanges ': r) a -> Sem r a
handleNoChanges = fmap fold . runError

convClientMap :: Local Data.Conversation -> ClientMap
convClientMap lconv =
  mconcat
    [ foldMap localMember . convLocalMembers,
      foldMap remoteMember . convRemoteMembers
    ]
    (tUnqualified lconv)
  where
    localMember lm = Map.singleton (qUntagged (qualifyAs lconv (lmId lm))) (lmMLSClients lm)
    remoteMember rm = Map.singleton (qUntagged (rmId rm)) (rmMLSClients rm)

-- | Propagate a message.
propagateMessage ::
  ( Member ExternalAccess r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input UTCTime) r,
    Member TinyLog r
  ) =>
  Local x ->
  Qualified UserId ->
  Data.Conversation ->
  Maybe ConnId ->
  ByteString ->
  Sem r ()
propagateMessage loc qusr conv con raw = do
  -- FUTUREWORK: check the epoch
  let lmems = Data.convLocalMembers conv
      botMap = Map.fromList $ do
        m <- lmems
        b <- maybeToList $ newBotMember m
        pure (lmId m, b)
      mm = defMessageMetadata
  now <- input @UTCTime
  let lcnv = qualifyAs loc (Data.convId conv)
      qcnv = qUntagged lcnv
      e = Event qcnv qusr now $ EdMLSMessage raw
      lclients = tUnqualified . clients <$> lmems
      mkPush :: UserId -> ClientId -> MessagePush 'NormalMessage
      mkPush u c = newMessagePush lcnv botMap con mm (u, c) e
  runMessagePush loc (Just qcnv) $
    foldMap (uncurry mkPush) (cToList =<< lclients)

  -- send to remotes
  (traverse_ handleError =<<)
    . runFederatedConcurrentlyEither (map remoteMemberQualify (Data.convRemoteMembers conv))
    $ \(tUnqualified -> rs) ->
      fedClient @'Galley @"on-mls-message-sent" $
        RemoteMLSMessage
          { rmmTime = now,
            rmmSender = qusr,
            rmmMetadata = mm,
            rmmConversation = tUnqualified lcnv,
            rmmRecipients = rs >>= remoteMemberMLSClients,
            rmmMessage = Base64ByteString raw
          }
  where
    cToList :: (UserId, Set ClientId) -> [(UserId, ClientId)]
    cToList (u, s) = (u,) <$> Set.toList s

    clients :: LocalMember -> Local (UserId, Set ClientId)
    clients LocalMember {..} = qualifyAs loc (lmId, lmMLSClients)

    remoteMemberMLSClients :: RemoteMember -> [(UserId, ClientId)]
    remoteMemberMLSClients rm =
      map
        (tUnqualified (rmId rm),)
        (toList (rmMLSClients rm))

    handleError :: Member TinyLog r => Either (Remote [a], FederationError) x -> Sem r ()
    handleError (Right _) = pure ()
    handleError (Left (r, e)) =
      warn $
        Logger.msg ("A message could not be delivered to a remote backend" :: ByteString)
          . Logger.field "remote_domain" (domainText (tDomain r))
          . logErrorMsg (toWai e)

getMLSClients ::
  Members '[BrigAccess, FederatorAccess] r =>
  Local x ->
  Qualified UserId ->
  SignatureSchemeTag ->
  Sem r (Set ClientId)
getMLSClients loc = foldQualified loc getLocalMLSClients getRemoteMLSClients

getRemoteMLSClients :: Member FederatorAccess r => Remote UserId -> SignatureSchemeTag -> Sem r (Set ClientId)
getRemoteMLSClients rusr ss = do
  runFederated rusr $
    fedClient @'Brig @"get-mls-clients" $
      MLSClientsRequest
        { mcrUserId = tUnqualified rusr,
          mcrSignatureScheme = ss
        }

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
  ( Members
      '[ Resource,
         ConversationStore,
         ErrorS 'MLSStaleMessage
       ]
      r
  ) =>
  GroupId ->
  Epoch ->
  NominalDiffTime ->
  Sem r a ->
  Sem r a
withCommitLock gid epoch ttl action =
  bracket
    ( acquireCommitLock gid epoch ttl >>= \lockAcquired ->
        when (lockAcquired == NotAcquired) $
          throwS @'MLSStaleMessage
    )
    (const $ releaseCommitLock gid epoch)
    (const action)
