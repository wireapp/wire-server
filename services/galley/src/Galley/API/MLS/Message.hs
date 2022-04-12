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

module Galley.API.MLS.Message (postMLSMessage) where

import Control.Arrow
import Control.Comonad
import Control.Lens (preview, to)
import Data.Either.Combinators
import Data.Id
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Set as Set
import Data.Tagged
import Data.Time
import Galley.API.Action
import Galley.API.Error
import Galley.API.MLS.KeyPackage
import Galley.API.Push
import Galley.API.Util
import Galley.Data.Conversation.Types hiding (Conversation)
import qualified Galley.Data.Conversation.Types as Data
import Galley.Effects
import Galley.Effects.BrigAccess
import Galley.Effects.ConversationStore
import Galley.Effects.MemberStore
import Galley.Options
import Galley.Types
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal
import Polysemy.TinyLog
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.Error
import Wire.API.MLS.Commit
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation

postMLSMessage ::
  ( HasProposalEffects r,
    Members
      '[ Error FederationError,
         ErrorS 'ConvNotFound,
         ErrorS 'MLSParseError,
         ErrorS 'MLSUnsupportedMessage,
         ErrorS 'MLSStaleMessage,
         ErrorS 'MLSProposalNotFound,
         TinyLog
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
      processCommit lusr con (rmRaw smsg) (msgEpoch msg) (msgGroupId msg) c
    ApplicationMessage _ -> throwS @'MLSUnsupportedMessage
    ProposalMessage _ -> pure mempty -- FUTUREWORK: handle proposals
  SomeMessage SMLSCipherText _ ->
    -- FUTUREWORK: handle all types of encrypted messages
    contentType (rmValue smsg) >>= \case
      ApplicationMessageTag ->
        pure <$> sendAppMessage lusr con smsg
      ProposalMessageTag -> pure mempty
      CommitMessageTag -> pure mempty

type HasProposalEffects r =
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member (Error MLSProtocolError) r,
    Member (Error MLSProposalFailure) r,
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
    Member (Error FederationError) r
  ) =>
  Local UserId ->
  ConnId ->
  ByteString ->
  Epoch ->
  GroupId ->
  Commit ->
  Sem r [Event]
processCommit lusr con _raw epoch gid commit = do
  -- fetch conversation
  qcnv <- getConversationIdByGroupId gid >>= noteS @'ConvNotFound
  lcnv <- ensureLocal lusr qcnv -- FUTUREWORK: allow remote conversations
  conv <- getConversation (tUnqualified lcnv) >>= noteS @'ConvNotFound

  -- check epoch number
  curEpoch <-
    preview (to convProtocol . _ProtocolMLS . to cnvmlsEpoch) conv
      & noteS @'ConvNotFound
  when (epoch /= curEpoch) $ throwS @'MLSStaleMessage

  -- process and execute proposals
  action <- foldMap applyProposalRef (cProposals commit)
  events <- executeProposalAction lusr con conv action

  -- increment epoch number
  setConversationEpoch (tUnqualified lcnv) (succ epoch)

  pure events

applyProposalRef ::
  ( HasProposalEffects r,
    Member (ErrorS 'MLSProposalNotFound) r
  ) =>
  ProposalOrRef ->
  Sem r ProposalAction
applyProposalRef (Ref _) = throwS @'MLSProposalNotFound
applyProposalRef (Inline p) = applyProposal p

applyProposal :: HasProposalEffects r => Proposal -> Sem r ProposalAction
applyProposal (AddProposal kp) = do
  ref <-
    kpRef' kp
      & note (mlsProtocolError "Could not compute ref of a key package in an Add proposal")
  qclient <- cidQualifiedClient <$> derefKeyPackage ref
  pure (paClient qclient)
applyProposal _ = throwS @'MLSUnsupportedProposal

executeProposalAction ::
  forall r.
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (Error FederationError) r,
    Member (ErrorS 'MLSClientMismatch) r,
    Member (Error MLSProposalFailure) r,
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
  Data.Conversation ->
  ProposalAction ->
  Sem r [Event]
executeProposalAction lusr con conv action = do
  let cm = convClientMap lusr conv
      newUserClients = Map.assocs (paAdd action)
  -- check that all clients of each user are added to the conversation, and
  -- update the database accordingly
  traverse_ (uncurry (addUserClients cm)) newUserClients
  -- add users to the conversation and send events
  result <- foldMap addMembers . nonEmpty . map fst $ newUserClients
  -- add clients to the database
  for_ newUserClients $ \(qtarget, newClients) -> do
    ltarget <- ensureLocal lusr qtarget -- FUTUREWORK: support remote users
    addMLSClients (convId conv) (tUnqualified ltarget) newClients
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

    addMembers :: NonEmpty (Qualified UserId) -> Sem r [Event]
    addMembers users =
      -- FUTUREWORK: update key package ref mapping to reflect conversation membership
      handleNoChanges
        . handleMLSProposalFailures @ProposalErrors
        . fmap pure
        . updateLocalConversationWithLocalUser @'ConversationJoinTag (qualifyAs lusr (convId conv)) lusr (Just con)
        $ ConversationJoin users roleNameWireMember

handleNoChanges :: Monoid a => Sem (Error NoChanges ': r) a -> Sem r a
handleNoChanges = fmap fold . runError

convClientMap :: Local x -> Data.Conversation -> ClientMap
convClientMap loc =
  mconcat
    [ foldMap localMember . convLocalMembers,
      mempty -- FUTUREWORK: add mls clients of remote members
    ]
  where
    localMember lm = Map.singleton (qUntagged (qualifyAs loc (lmId lm))) (lmMLSClients lm)

-- | Propagate an application message. Do not export this function due to its
-- imprecise interface (the 'SomeMessage' argument can also contain a commit or
-- a proposal).
sendAppMessage ::
  Members
    ( Append
        '[ BrigAccess,
           ConversationStore,
           Error FederationError,
           ErrorS 'ConvNotFound,
           ErrorS 'MLSParseError,
           ErrorS 'MLSKeyPackageRefNotFound,
           GundeckAccess,
           Input UTCTime,
           MemberStore
         ]
        (MessagePushEffects 'NormalMessage)
    )
    r =>
  Local UserId ->
  ConnId ->
  RawMLS SomeMessage ->
  Sem r Event
sendAppMessage lusr conn rMsg@(rmValue -> SomeMessage SMLSCipherText msg) = do
  when (isNothing . appMsgPayload $ msg) $ error "Expected an application message payload"

  -- FUTUREWORK: check the epoch
  convId <- getConversationIdByGroupId (msgGroupId msg) >>= noteS @'ConvNotFound
  lconvId <- ensureLocal lusr convId
  lmems <- getLocalMembers (qUnqualified convId)
  let lRecipients = clients lusr <$> lmems
      -- FUTUREWORK: support remote recipients
      lmMap = Map.fromList $ fmap (lmId &&& id) lmems
  sendLocalCipherApplication lusr lconvId conn rMsg lmMap (tUnqualified <$> lRecipients)
  where
    appMsgPayload m =
      case toMLSEnum' @ContentType . msgContentType . msgPayload $ m of
        Right ApplicationMessageTag -> Just . msgCipherText . msgPayload $ m
        _ -> Nothing
    clients :: Local x -> LocalMember -> Local (UserId, Set ClientId)
    clients loc LocalMember {..} = qualifyAs loc (lmId, lmMLSClients)
sendAppMessage _lusr _conn _msg =
  error "Plain text application messages not supported"

-- | Send an encrypted application message to local recipients
sendLocalCipherApplication ::
  Members
    ( Append
        '[Input UTCTime]
        (MessagePushEffects 'NormalMessage)
    )
    r =>
  Local UserId ->
  Local ConvId ->
  ConnId ->
  RawMLS SomeMessage ->
  LocalMemberMap 'NormalMessage ->
  [(UserId, Set ClientId)] ->
  Sem r Event
sendLocalCipherApplication lusr lconv conn msg lmMap lclients = do
  now <- input @UTCTime
  let p = rmRaw msg
      e = Event (qUntagged lconv) (qUntagged lusr) now $ EdMLSMessage p
  runMessagePush lconv (Just . qUntagged $ lconv) $
    foldMap (uncurry $ mkPush e) (cToList =<< lclients)
  pure e
  where
    mkPush :: Event -> UserId -> ClientId -> MessagePush 'NormalMessage
    mkPush ev u c = newMessagePush lconv lmMap (Just conn) defMessageMetadata (u, c) ev
    cToList :: (UserId, Set ClientId) -> [(UserId, ClientId)]
    cToList (u, s) = (u,) <$> Set.toList s

-- | Send an encrypted application message to remote recipients
_sendRemoteCipherApplication ::
  RawMLS SomeMessage ->
  Remote [(UserId, Set ClientId)] ->
  Sem r ()
_sendRemoteCipherApplication _ _ = pure ()

-- | Get the content type of a message. It throws an 'MLSParseError' if there
-- was an error in parsing the content type.
contentType :: Member (ErrorS 'MLSParseError) r => SomeMessage -> Sem r ContentType
contentType (SomeMessage SMLSPlainText m) =
  pure . mpTag . msgTBS . msgPayload $ m
contentType (SomeMessage SMLSCipherText m) =
  fromEither
    . mapLeft (const (Tagged @'MLSParseError ())) -- TODO(md): see if we can be
    -- more precise with error
    -- types
    . toMLSEnum' @ContentType
    . msgContentType
    . msgPayload
    $ m

--------------------------------------------------------------------------------
-- Error handling of proposal execution

-- The following errors are caught by 'executeProposalAction' and wrapped in a
-- 'MLSProposalFailure'. This way errors caused by the execution of proposals are
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
