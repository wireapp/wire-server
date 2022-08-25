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
    postMLSMessageFromLocalUserV1,
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
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import qualified Wire.API.MLS.Proposal as Proposal
import Wire.API.MLS.Serialisation
import Wire.API.Message
import Wire.API.User.Client

type MLSMessageStaticErrors =
  '[ ErrorS 'ConvAccessDenied,
     ErrorS 'ConvMemberNotFound,
     ErrorS 'ConvNotFound,
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
     ErrorS 'MLSGroupConversationMismatch
   ]

postMLSMessageFromLocalUserV1 ::
  ( HasProposalEffects r,
    Members
      '[ Error FederationError,
         Error InternalError,
         ErrorS 'ConvAccessDenied,
         ErrorS 'ConvMemberNotFound,
         ErrorS 'ConvNotFound,
         ErrorS 'MLSCommitMissingReferences,
         ErrorS 'MLSProposalNotFound,
         ErrorS 'MLSSelfRemovalNotAllowed,
         ErrorS 'MLSStaleMessage,
         ErrorS 'MLSUnsupportedMessage,
         ErrorS 'MLSClientSenderUserMismatch,
         ErrorS 'MLSGroupConversationMismatch,
         ErrorS 'MissingLegalholdConsent,
         Input (Local ()),
         ProposalStore,
         Resource,
         TinyLog
       ]
      r
  ) =>
  Local UserId ->
  ConnId ->
  RawMLS SomeMessage ->
  Sem r [Event]
postMLSMessageFromLocalUserV1 lusr conn smsg = case rmValue smsg of
  SomeMessage _ msg -> do
    qcnv <- getConversationIdByGroupId (msgGroupId msg) >>= noteS @'ConvNotFound
    map lcuEvent
      <$> postMLSMessage lusr (qUntagged lusr) qcnv (Just conn) smsg

postMLSMessageFromLocalUser ::
  ( HasProposalEffects r,
    Members
      '[ Error FederationError,
         Error InternalError,
         ErrorS 'ConvAccessDenied,
         ErrorS 'ConvMemberNotFound,
         ErrorS 'ConvNotFound,
         ErrorS 'MLSCommitMissingReferences,
         ErrorS 'MLSProposalNotFound,
         ErrorS 'MLSSelfRemovalNotAllowed,
         ErrorS 'MLSStaleMessage,
         ErrorS 'MLSUnsupportedMessage,
         ErrorS 'MLSClientSenderUserMismatch,
         ErrorS 'MLSGroupConversationMismatch,
         ErrorS 'MissingLegalholdConsent,
         Input (Local ()),
         ProposalStore,
         Resource,
         TinyLog
       ]
      r
  ) =>
  Local UserId ->
  ConnId ->
  RawMLS SomeMessage ->
  Sem r MLSMessageSendingStatus
postMLSMessageFromLocalUser lusr conn msg = do
  -- FUTUREWORK: Inline the body of 'postMLSMessageFromLocalUserV1' once version
  -- V1 is dropped
  events <- postMLSMessageFromLocalUserV1 lusr conn msg
  t <- toUTCTimeMillis <$> input
  pure $ MLSMessageSendingStatus events t

postMLSMessage ::
  ( HasProposalEffects r,
    Members
      '[ Error FederationError,
         Error InternalError,
         ErrorS 'ConvAccessDenied,
         ErrorS 'ConvNotFound,
         ErrorS 'ConvMemberNotFound,
         ErrorS 'MLSUnsupportedMessage,
         ErrorS 'MLSStaleMessage,
         ErrorS 'MLSProposalNotFound,
         ErrorS 'MissingLegalholdConsent,
         ErrorS 'MLSCommitMissingReferences,
         ErrorS 'MLSSelfRemovalNotAllowed,
         ErrorS 'MLSClientSenderUserMismatch,
         ErrorS 'MLSGroupConversationMismatch,
         Resource,
         TinyLog,
         ProposalStore,
         Input (Local ())
       ]
      r
  ) =>
  Local x ->
  Qualified UserId ->
  Qualified ConvId ->
  Maybe ConnId ->
  RawMLS SomeMessage ->
  Sem r [LocalConversationUpdate]
postMLSMessage loc qusr qcnv con smsg = case rmValue smsg of
  SomeMessage _ msg -> do
    unless (msgEpoch msg == Epoch 0) $
      flip unless (throwS @'MLSClientSenderUserMismatch) =<< isUserSender qusr smsg
    foldQualified
      loc
      (postMLSMessageToLocalConv qusr con smsg)
      (postMLSMessageToRemoteConv loc qusr con smsg)
      qcnv

-- | Check that the MLS client who created the message belongs to the user who
-- is the sender of the REST request, identified by HTTP header.
--
-- This is only relevant in an ongoing conversation. The check should be skipped
-- in case of
-- * encrypted messages in which we don't have access to the sending client's
--   key package,
-- * messages sent by the backend, and
-- * external add proposals which propose fresh key packages for new clients and
--   thus the validity of the key package cannot be known at the time of this
--   check.
-- For these cases the function will return True.
isUserSender ::
  ( Members
      '[ ErrorS 'MLSKeyPackageRefNotFound,
         BrigAccess
       ]
      r
  ) =>
  Qualified UserId ->
  RawMLS SomeMessage ->
  Sem r Bool
isUserSender qusr smsg = case rmValue smsg of
  SomeMessage tag msg -> case tag of
    -- skip encrypted message
    SMLSCipherText -> pure True
    SMLSPlainText -> case msgSender msg of
      -- skip message sent by backend
      PreconfiguredSender _ -> pure True
      -- skip external add proposal
      NewMemberSender -> pure True
      MemberSender ref -> do
        ci <- derefKeyPackage ref
        pure $ fmap fst (cidQualifiedClient ci) == qusr

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
         ErrorS 'MLSCommitMissingReferences,
         ErrorS 'MLSSelfRemovalNotAllowed,
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

    -- check that sender is part of conversation
    loc <- qualifyLocal ()
    isMember' <- foldQualified loc (fmap isJust . getLocalMember (convId conv) . tUnqualified) (fmap isJust . getRemoteMember (convId conv)) qusr
    unless isMember' $ throwS @'ConvNotFound

    -- validate message
    events <- case tag of
      SMLSPlainText -> case msgPayload msg of
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
  -- only members may send messages to the remote conversation
  flip unless (throwS @'ConvMemberNotFound) =<< checkLocalMemberRemoteConv (tUnqualified lusr) rcnv

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

type ClientMap = Map (Qualified UserId) (Set (ClientId, KeyPackageRef))

data ProposalAction = ProposalAction
  { paAdd :: ClientMap,
    paRemove :: ClientMap
  }

instance Semigroup ProposalAction where
  ProposalAction add1 rem1 <> ProposalAction add2 rem2 =
    ProposalAction
      (Map.unionWith mappend add1 add2)
      (Map.unionWith mappend rem1 rem2)

instance Monoid ProposalAction where
  mempty = ProposalAction mempty mempty

paAddClient :: Qualified (UserId, (ClientId, KeyPackageRef)) -> ProposalAction
paAddClient quc = mempty {paAdd = Map.singleton (fmap fst quc) (Set.singleton (snd (qUnqualified quc)))}

paRemoveClient :: Qualified (UserId, (ClientId, KeyPackageRef)) -> ProposalAction
paRemoveClient quc = mempty {paRemove = Map.singleton (fmap fst quc) (Set.singleton (snd (qUnqualified quc)))}

processCommit ::
  ( HasProposalEffects r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MLSCommitMissingReferences) r,
    Member (ErrorS 'MLSProposalNotFound) r,
    Member (ErrorS 'MLSSelfRemovalNotAllowed) r,
    Member (ErrorS 'MLSStaleMessage) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (Input (Local ())) r,
    Member ProposalStore r,
    Member BrigAccess r,
    Member Resource r
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
    checkEpoch epoch (tUnqualified lconv)
    postponedKeyPackageRefUpdate <-
      if epoch == Epoch 0
        then do
          -- this is a newly created conversation, and it should contain exactly one
          -- client (the creator)
          case (sender, first (fmap fst . toList . lmMLSClients) self) of
            (MemberSender currentRef, Left [creatorClient]) -> do
              -- use update path as sender reference and if not existing fall back to sender
              senderRef <-
                maybe
                  (pure currentRef)
                  ( note (mlsProtocolError "Could not compute key package ref")
                      . kpRef'
                      . upLeaf
                  )
                  $ cPath commit
              -- register the creator client
              addKeyPackageRef
                senderRef
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
          pure $ pure () -- no key package ref update necessary
        else case (sender, upLeaf <$> cPath commit) of
          (MemberSender senderRef, Just updatedKeyPackage) -> do
            updatedRef <- kpRef' updatedKeyPackage & note (mlsProtocolError "Could not compute key package ref")
            -- postpone key package ref update until other checks/processing passed
            pure . updateKeyPackageRef $
              KeyPackageUpdate
                { kpupPrevious = senderRef,
                  kpupNext = updatedRef
                }
          (_, Nothing) -> pure $ pure () -- ignore commits without update path
          _ -> throw (mlsProtocolError "Unexpected sender")

    -- check all pending proposals are referenced in the commit
    allPendingProposals <- getAllPendingProposals groupId epoch
    let referencedProposals = Set.fromList $ mapMaybe (\x -> preview Proposal._Ref x) (cProposals commit)
    unless (all (`Set.member` referencedProposals) allPendingProposals) $
      throwS @'MLSCommitMissingReferences

    -- process and execute proposals
    action <- foldMap (applyProposalRef (tUnqualified lconv) groupId epoch) (cProposals commit)
    updates <- executeProposalAction qusr con lconv action

    -- update key package ref if necessary
    postponedKeyPackageRefUpdate
    -- increment epoch number
    setConversationEpoch (Data.convId (tUnqualified lconv)) (succ epoch)

    pure updates

applyProposalRef ::
  ( HasProposalEffects r,
    Members
      '[ ErrorS 'ConvNotFound,
         ErrorS 'MLSProposalNotFound,
         ErrorS 'MLSStaleMessage,
         ProposalStore
       ]
      r
  ) =>
  Data.Conversation ->
  GroupId ->
  Epoch ->
  ProposalOrRef ->
  Sem r ProposalAction
applyProposalRef conv groupId epoch (Ref ref) = do
  p <- getProposal groupId epoch ref >>= noteS @'MLSProposalNotFound
  checkEpoch epoch conv
  checkGroup groupId conv
  applyProposal (rmValue p)
applyProposalRef conv _groupId _epoch (Inline p) = do
  suite <-
    preview (to convProtocol . _ProtocolMLS . to cnvmlsCipherSuite) conv
      & noteS @'ConvNotFound
  checkProposalCipherSuite suite p
  applyProposal p

applyProposal :: HasProposalEffects r => Proposal -> Sem r ProposalAction
applyProposal (AddProposal kp) = do
  ref <-
    kpRef' kp
      & note (mlsProtocolError "Could not compute ref of a key package in an Add proposal")
  qclient <- cidQualifiedClient <$> derefKeyPackage ref
  pure (paAddClient (fmap (fmap (,ref)) qclient))
applyProposal (RemoveProposal ref) = do
  qclient <- cidQualifiedClient <$> derefKeyPackage ref
  pure (paRemoveClient (fmap (fmap (,ref)) qclient))
applyProposal _ = pure mempty

checkProposalCipherSuite ::
  Members
    '[ Error MLSProtocolError,
       ProposalStore
     ]
    r =>
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
  checkEpoch (msgEpoch msg) conv
  checkGroup (msgGroupId msg) conv
  suiteTag <-
    preview (to convProtocol . _ProtocolMLS . to cnvmlsCipherSuite) conv
      & noteS @'ConvNotFound

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
  storeProposal (msgGroupId msg) (msgEpoch msg) propRef prop

checkExternalProposalSignature ::
  Members
    '[ ErrorS 'MLSUnsupportedProposal
     ]
    r =>
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
  Members
    '[ BrigAccess,
       ErrorS 'MLSUnsupportedProposal,
       Input (Local ())
     ]
    r =>
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
              $ decodeMLS' @ClientIdentity (bcIdentity . kpCredential . rmValue $ keyPackage)
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
    Member (ErrorS 'ConvNotFound) r,
    Member (Error FederationError) r,
    Member (ErrorS 'MLSClientMismatch) r,
    Member (Error MLSProtocolError) r,
    Member (Error MLSProposalFailure) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'MLSUnsupportedProposal) r,
    Member (ErrorS 'MLSSelfRemovalNotAllowed) r,
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
  cs <- preview (to convProtocol . _ProtocolMLS . to cnvmlsCipherSuite) (tUnqualified lconv) & noteS @'ConvNotFound
  let ss = csSignatureScheme cs
      cm = convClientMap lconv
      newUserClients = Map.assocs (paAdd action)
      removeUserClients = Map.assocs (paRemove action)

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
      clientInfo <- getMLSClients lconv qtarget ss
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

  membersToRemove <- catMaybes <$> for removeUserClients (uncurry (checkRemoval lconv ss))

  -- add users to the conversation and send events
  addEvents <- foldMap addMembers . nonEmpty . map fst $ newUserClients
  -- add clients to the database
  for_ newUserClients $ \(qtarget, newClients) -> do
    addMLSClients (fmap convId lconv) qtarget newClients

  -- remove users from the conversation and send events
  removeEvents <- foldMap removeMembers (nonEmpty membersToRemove)

  pure (addEvents <> removeEvents)
  where
    -- This also filters out client removals for clients that don't exist anymore
    -- For these clients there is nothing left to do
    checkRemoval ::
      Local x ->
      SignatureSchemeTag ->
      Qualified UserId ->
      Set (ClientId, KeyPackageRef) ->
      Sem r (Maybe (Qualified UserId))
    checkRemoval loc ss qtarget (Set.map fst -> clients) = do
      allClients <- Set.map ciId <$> getMLSClients loc qtarget ss
      let allClientsDontExist = Set.null (clients `Set.intersection` allClients)
      if allClientsDontExist
        then pure Nothing
        else do
          -- We only support removal of client for user. This is likely to change in the future.
          -- See discussions here https://wearezeta.atlassian.net/wiki/spaces/CL/pages/612106259/Relax+constraint+between+users+and+clients+in+MLS+groups
          when (clients /= allClients) $ do
            -- FUTUREWORK: turn this error into a proper response
            throwS @'MLSClientMismatch
          when (qusr == qtarget) $
            throwS @'MLSSelfRemovalNotAllowed
          pure (Just qtarget)

    addMembers :: NonEmpty (Qualified UserId) -> Sem r [LocalConversationUpdate]
    addMembers users =
      -- FUTUREWORK: update key package ref mapping to reflect conversation membership
      handleNoChanges
        . handleMLSProposalFailures @ProposalErrors
        . fmap pure
        . updateLocalConversationUnchecked
          @'ConversationJoinTag
          lconv
          qusr
          con
        $ ConversationJoin users roleNameWireMember

    removeMembers :: NonEmpty (Qualified UserId) -> Sem r [LocalConversationUpdate]
    removeMembers =
      handleNoChanges
        . handleMLSProposalFailures @ProposalErrors
        . fmap pure
        . updateLocalConversationUnchecked
          @'ConversationRemoveMembersTag
          lconv
          qusr
          con

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
  traverse_ handleError <=< runFederatedConcurrentlyEither (map remoteMemberQualify (Data.convRemoteMembers conv)) $
    \(tUnqualified -> rs) ->
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
    clients LocalMember {..} = qualifyAs loc (lmId, Set.map fst lmMLSClients)

    remoteMemberMLSClients :: RemoteMember -> [(UserId, ClientId)]
    remoteMemberMLSClients rm =
      map
        (tUnqualified (rmId rm),)
        (toList (Set.map fst (rmMLSClients rm)))

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
  Sem r (Set ClientInfo)
getMLSClients loc = foldQualified loc getLocalMLSClients getRemoteMLSClients

getRemoteMLSClients ::
  Member FederatorAccess r =>
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
  Members
    '[ ErrorS 'ConvNotFound,
       ErrorS 'MLSStaleMessage
     ]
    r =>
  Epoch ->
  Data.Conversation ->
  Sem r ()
checkEpoch epoch conv = do
  curEpoch <-
    preview (to convProtocol . _ProtocolMLS . to cnvmlsEpoch) conv
      & noteS @'ConvNotFound
  unless (epoch == curEpoch) $ throwS @'MLSStaleMessage

-- | Check if the group ID matches that of a conversation
checkGroup ::
  Member (ErrorS 'ConvNotFound) r =>
  GroupId ->
  Data.Conversation ->
  Sem r ()
checkGroup gId conv = do
  groupId <-
    preview (to convProtocol . _ProtocolMLS . to cnvmlsGroupId) conv
      & noteS @'ConvNotFound
  unless (gId == groupId) $ throwS @'ConvNotFound

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
