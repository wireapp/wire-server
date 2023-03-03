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

module Galley.API.Message
  ( UserType (..),
    sendLocalMessages,
    postQualifiedOtrMessage,
    postBroadcast,
    postRemoteOtrMessage,
    legacyClientMismatchStrategy,
    Unqualify (..),
    userToProtectee,
    MessageMetadata (..),

    -- * Only exported for tests
    checkMessageClients,
    QualifiedMismatch (..),
    mkQualifiedUserClients,
    clientMismatchStrategyApply,
    collectFailedToSend,
  )
where

import Control.Lens
import Control.Monad.Extra (eitherM)
import Data.Aeson (encode)
import Data.Bifunctor
import Data.ByteString.Conversion (toByteString')
import Data.Domain (Domain)
import Data.Id
import Data.Json.Util
import qualified Data.Map as Map
import Data.Map.Lens (toMapOf)
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Data.Set.Lens
import Data.Singletons
import Data.Time.Clock (UTCTime)
import Galley.API.LegalHold.Conflicts
import Galley.API.Push
import Galley.API.Util
import Galley.Data.Conversation
import Galley.Data.Services
import Galley.Effects
import Galley.Effects.BrigAccess
import Galley.Effects.ClientStore
import Galley.Effects.ConversationStore
import Galley.Effects.FederatorAccess
import Galley.Effects.TeamStore
import Galley.Options
import qualified Galley.Types.Clients as Clients
import Galley.Types.Conversations.Members
import Imports hiding (forkIO)
import Polysemy hiding (send)
import Polysemy.Error
import Polysemy.Input
import qualified Polysemy.TinyLog as P
import qualified System.Logger.Class as Log
import Wire.API.Conversation.Protocol
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error
import Wire.API.Message
import Wire.API.Routes.Public.Galley.Messaging
import Wire.API.Team.LegalHold
import Wire.API.Team.Member
import Wire.API.User.Client
import Wire.API.UserMap (UserMap (..))

data UserType = User | Bot

userToProtectee :: UserType -> UserId -> LegalholdProtectee
userToProtectee User user = ProtectedUser user
userToProtectee Bot _ = UnprotectedBot

qualifiedUserToProtectee ::
  Domain ->
  UserType ->
  Qualified UserId ->
  LegalholdProtectee
qualifiedUserToProtectee localDomain ty user
  | qDomain user == localDomain = userToProtectee ty (qUnqualified user)
  | otherwise = LegalholdPlusFederationNotImplemented

data QualifiedMismatch = QualifiedMismatch
  { qmMissing :: QualifiedUserClients,
    qmRedundant :: QualifiedUserClients,
    qmDeleted :: QualifiedUserClients
  }
  deriving (Show, Eq)

type QualifiedRecipientSet = Set (Domain, UserId, ClientId)

type RecipientSet = Set (UserId, ClientId)

mkQualifiedMismatch ::
  QualifiedRecipientSet -> QualifiedRecipientSet -> QualifiedRecipientSet -> QualifiedMismatch
mkQualifiedMismatch missing redundant deleted =
  QualifiedMismatch
    (mkQualifiedUserClients missing)
    (mkQualifiedUserClients redundant)
    (mkQualifiedUserClients deleted)

mkQualifiedUserClients :: QualifiedRecipientSet -> QualifiedUserClients
mkQualifiedUserClients =
  QualifiedUserClients
    . Set.foldr (\(d, u, c) -> Map.insertWith (Map.unionWith Set.union) d (Map.singleton u (Set.singleton c))) Map.empty

mkQualifiedUserClientsByDomain :: Map Domain RecipientSet -> QualifiedUserClients
mkQualifiedUserClientsByDomain =
  QualifiedUserClients
    . Map.filter (not . Map.null)
    . fmap byUser
  where
    byUser :: RecipientSet -> Map UserId (Set ClientId)
    byUser = foldr (\(u, c) -> Map.insertWith (<>) u (Set.singleton c)) mempty

mkMessageSendingStatus :: UTCTimeMillis -> QualifiedMismatch -> QualifiedUserClients -> MessageSendingStatus
mkMessageSendingStatus time mismatch failures =
  MessageSendingStatus
    { mssTime = time,
      mssMissingClients = qmMissing mismatch,
      mssRedundantClients = qmRedundant mismatch,
      mssDeletedClients = qmDeleted mismatch,
      mssFailedToSend = failures
    }

clientMismatchStrategyApply :: ClientMismatchStrategy -> QualifiedRecipientSet -> QualifiedRecipientSet
clientMismatchStrategyApply MismatchReportAll = id
clientMismatchStrategyApply MismatchIgnoreAll = const mempty
clientMismatchStrategyApply (MismatchReportOnly users) =
  Set.filter (\(d, u, _) -> Set.member (Qualified u d) users)
clientMismatchStrategyApply (MismatchIgnoreOnly users) =
  Set.filter (\(d, u, _) -> not (Set.member (Qualified u d) users))

-- A Venn diagram of words in this function:
--
--                +-----------------------------------+
--                |                                   |
--    +---------->|                 +-----------------+--------------------+
--    |           |                 |                 |                    | <------+
--    |           |                 |                 |   Deleted Clients  |        |
--    |           |                 |                 |                    |        |
-- Expected       |                 |                 |                    |   Recipients
-- Clients        |  Missing        | Valid           |       Extra        |
--                |  Clients        | Clients         +-------Clients------+----------+
--                |                 |                 |                    |          |
--                |                 |                 |                    |          |
--                |                 |                 |       Redundant Clients     <------- Sender Client
--                |                 |                 |                    |          |
--                |                 |                 |                    |          |
--                |                 |                 |                    |          |
--                |                 +--------------------------------------+----------+
--                |                                   |
--                +-----------------------------------+
checkMessageClients ::
  -- | Sender
  (Domain, UserId, ClientId) ->
  -- | Participants of the conversation
  --
  -- When the set of clients for a given user is empty, that means the user is
  -- present in the conversation, but has no clients at all, and this is a
  -- valid state.
  Map (Domain, UserId) (Set ClientId) ->
  -- | Provided recipients and ciphertexts
  Map (Domain, UserId, ClientId) ByteString ->
  -- | Subset of missing clients to report
  ClientMismatchStrategy ->
  (Bool, Map (Domain, UserId, ClientId) ByteString, QualifiedMismatch)
checkMessageClients sender participantMap recipientMap mismatchStrat =
  let participants = setOf ((itraversed <. folded) . withIndex . to (\((d, u), c) -> (d, u, c))) participantMap
      expected = Set.delete sender participants
      expectedUsers :: Set (Domain, UserId) = Map.keysSet participantMap

      recipients = Map.keysSet recipientMap
      -- Whoever is expected but not in recipients is missing.
      missing = Set.difference expected recipients
      -- Whoever is in recipient but not expected is extra.
      extra = Set.difference recipients expected
      -- The clients which belong to users who are expected are considered deleted.
      deleted =
        Set.delete sender -- the sender is never deleted
          . Set.filter (\(d, u, _) -> Set.member (d, u) expectedUsers)
          $ extra
      -- The clients which are extra but not deleted, must belong to users which
      -- are not in the convesation and hence considered redundant.
      redundant = Set.difference extra deleted
      -- The clients which are both recipients and expected are considered valid.
      valid = Set.intersection recipients expected
      validMap = Map.restrictKeys recipientMap valid
      -- Resolve whether the message is valid using client mismatch strategy
      reportedMissing = clientMismatchStrategyApply mismatchStrat missing
   in ( Set.null reportedMissing,
        validMap,
        mkQualifiedMismatch reportedMissing redundant deleted
      )

getRemoteClients ::
  (Member FederatorAccess r) =>
  [RemoteMember] ->
  Sem r [Either (Remote [UserId], FederationError) (Map (Domain, UserId) (Set ClientId))]
getRemoteClients remoteMembers =
  -- concatenating maps is correct here, because their sets of keys are disjoint
  -- Use runFederatedConcurrentlyEither so we can catch federation errors and report to clients
  -- which domains and users aren't contactable at the moment.
  tUnqualified <$$$> runFederatedConcurrentlyEither (map rmId remoteMembers) getRemoteClientsFromDomain
  where
    getRemoteClientsFromDomain :: Remote [UserId] -> FederatorClient 'Brig (Map (Domain, UserId) (Set ClientId))
    getRemoteClientsFromDomain (tUntagged -> Qualified uids domain) =
      Map.mapKeys (domain,) . fmap (Set.map pubClientId) . userMap
        <$> fedClient @'Brig @"get-user-clients" (GetUserClients uids)

-- FUTUREWORK: sender should be Local UserId
postRemoteOtrMessage ::
  (Member FederatorAccess r) =>
  Qualified UserId ->
  Remote ConvId ->
  ByteString ->
  Sem r (PostOtrResponse MessageSendingStatus)
postRemoteOtrMessage sender conv rawMsg = do
  let msr =
        ProteusMessageSendRequest
          { pmsrConvId = tUnqualified conv,
            pmsrSender = qUnqualified sender,
            pmsrRawMessage = Base64ByteString rawMsg
          }
      rpc = fedClient @'Galley @"send-message" msr
  msResponse <$> runFederated conv rpc

postBroadcast ::
  ( Member BrigAccess r,
    Member ClientStore r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'NonBindingTeam) r,
    Member (ErrorS 'BroadcastLimitExceeded) r,
    Member GundeckAccess r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  QualifiedNewOtrMessage ->
  Sem r (PostOtrResponse MessageSendingStatus)
postBroadcast lusr con msg = runError $ do
  let senderClient = qualifiedNewOtrSender msg
      senderDomain = tDomain lusr
      senderUser = tUnqualified lusr
      rcps =
        Map.findWithDefault mempty senderDomain
          . qualifiedUserClientMap
          . qualifiedOtrRecipientsMap
          . qualifiedNewOtrRecipients
          $ msg
  now <- input

  tid <- lookupBindingTeam senderUser
  limit <- fromIntegral . fromRange <$> fanoutLimit
  -- If we are going to fan this out to more than limit, we want to fail early
  unless (Map.size rcps <= limit) $
    throwS @'BroadcastLimitExceeded
  -- In large teams, we may still use the broadcast endpoint but only if `report_missing`
  -- is used and length `report_missing` < limit since we cannot fetch larger teams than
  -- that.
  tMembers <-
    fmap (view userId) <$> case qualifiedNewOtrClientMismatchStrategy msg of
      -- Note: remote ids are not in a local team
      MismatchReportOnly qus ->
        maybeFetchLimitedTeamMemberList
          limit
          tid
          (fst (partitionQualified lusr qus))
          rcps
      _ -> maybeFetchAllMembersInTeam tid
  contacts <- getContactList senderUser
  let users = toList $ Set.union (Set.fromList tMembers) (Set.fromList contacts)

  isInternal <- useIntraClientListing
  localClients <-
    if isInternal
      then Clients.fromUserClients <$> lookupClients users
      else getClients users
  let qualifiedLocalClients =
        Map.mapKeys (tDomain lusr,)
          . makeUserMap (Set.fromList users)
          . Clients.toMap
          $ localClients

  let (sendMessage, validMessages, mismatch) =
        checkMessageClients
          (senderDomain, senderUser, senderClient)
          qualifiedLocalClients
          (flattenMap $ qualifiedNewOtrRecipients msg)
          (qualifiedNewOtrClientMismatchStrategy msg)
      otrResult = mkMessageSendingStatus (toUTCTimeMillis now) mismatch mempty
  unless sendMessage $ do
    let lhProtectee = qualifiedUserToProtectee (tDomain lusr) User (tUntagged lusr)
        missingClients = qmMissing mismatch

    mapError @LegalholdConflicts @(MessageNotSent MessageSendingStatus)
      (const MessageNotSentLegalhold)
      $ runLocalInput lusr
      $ guardQualifiedLegalholdPolicyConflicts lhProtectee missingClients
    throw $ MessageNotSentClientMissing otrResult

  failedToSend <-
    sendBroadcastMessages
      lusr
      now
      (tUntagged lusr)
      senderClient
      con
      (qualifiedNewOtrMetadata msg)
      validMessages
  pure otrResult {mssFailedToSend = failedToSend}
  where
    maybeFetchLimitedTeamMemberList ::
      ( Member (ErrorS 'BroadcastLimitExceeded) r,
        Member TeamStore r
      ) =>
      Int ->
      TeamId ->
      [UserId] ->
      Map UserId (Map ClientId ByteString) ->
      Sem r [TeamMember]
    maybeFetchLimitedTeamMemberList limit tid localUserIdsInFilter rcps = do
      let localUserIdsInRcps = Map.keys rcps
      let localUserIdsToLookup = Set.toList $ Set.union (Set.fromList localUserIdsInFilter) (Set.fromList localUserIdsInRcps)
      unless (length localUserIdsToLookup <= limit) $
        throwS @'BroadcastLimitExceeded
      selectTeamMembers tid localUserIdsToLookup
    maybeFetchAllMembersInTeam ::
      ( Member (ErrorS 'BroadcastLimitExceeded) r,
        Member TeamStore r
      ) =>
      TeamId ->
      Sem r [TeamMember]
    maybeFetchAllMembersInTeam tid = do
      mems <- getTeamMembersForFanout tid
      when (mems ^. teamMemberListType == ListTruncated) $
        throwS @'BroadcastLimitExceeded
      pure (mems ^. teamMembers)

postQualifiedOtrMessage ::
  ( Member BrigAccess r,
    Member ClientStore r,
    Member ConversationStore r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member ExternalAccess r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  UserType ->
  Qualified UserId ->
  Maybe ConnId ->
  Local ConvId ->
  QualifiedNewOtrMessage ->
  Sem r (PostOtrResponse MessageSendingStatus)
postQualifiedOtrMessage senderType sender mconn lcnv msg =
  runError @(MessageNotSent MessageSendingStatus)
    . mapToRuntimeError @'ConvNotFound @(MessageNotSent MessageSendingStatus) MessageNotSentConversationNotFound
    . mapToRuntimeError @'InvalidOperation @(MessageNotSent MessageSendingStatus) MessageNotSentConversationNotFound
    $ do
      let localDomain = tDomain lcnv
      now <- input
      let nowMillis = toUTCTimeMillis now
      let senderDomain = qDomain sender
          senderUser = qUnqualified sender
      let senderClient = qualifiedNewOtrSender msg

      conv <- getConversation (tUnqualified lcnv) >>= noteS @'ConvNotFound
      unless (protocolTag (convProtocol conv) == ProtocolProteusTag) $
        throwS @'InvalidOperation

      let localMemberIds = lmId <$> convLocalMembers conv
          botMap :: BotMap
          botMap = Map.fromList $ do
            mem <- convLocalMembers conv
            b <- maybeToList $ newBotMember mem
            pure (lmId mem, b)
          members :: Set (Qualified UserId)
          members =
            Set.fromList $
              map (tUntagged . qualifyAs lcnv) localMemberIds
                <> map (tUntagged . rmId) (convRemoteMembers conv)
      isInternal <- view (optSettings . setIntraListing) <$> input

      -- check if the sender is part of the conversation
      unless (Set.member sender members) $
        throwS @'ConvNotFound

      -- get local clients
      localClients <-
        if isInternal
          then Clients.fromUserClients <$> lookupClients localMemberIds
          else getClients localMemberIds
      let qualifiedLocalClients =
            Map.mapKeys (localDomain,)
              . makeUserMap (Set.fromList (map lmId (convLocalMembers conv)))
              . Clients.toMap
              $ localClients

      -- get remote clients
      qualifiedRemoteClients <- getRemoteClients (convRemoteMembers conv)
      let qualifiedClients = qualifiedLocalClients <> qualifiedRemoteClients'
          -- concatenating maps is correct here, because their sets of keys are disjoint
          qualifiedRemoteClients' = mconcat $ rights qualifiedRemoteClients
          -- Collect the list of users and their domains that we weren't able to fetch clients for.
          failedToSendFetchingClients :: QualifiedUserClients
          failedToSendFetchingClients =
            QualifiedUserClients . mconcat $ extractUserMap . fst <$> lefts qualifiedRemoteClients
            where
              extractUserMap :: Remote [UserId] -> Map Domain (Map UserId (Set ClientId))
              extractUserMap l = Map.singleton domain $ Map.fromList $ (,mempty) <$> users
                where
                  domain = qDomain $ tUntagged l
                  users = tUnqualified l
      -- check if the sender client exists (as one of the clients in the conversation)
      unless
        ( Set.member
            senderClient
            (Map.findWithDefault mempty (senderDomain, senderUser) qualifiedClients)
        )
        $ throw (MessageNotSentUnknownClient :: MessageNotSent MessageSendingStatus)

      let (sendMessage, validMessages, mismatch) =
            checkMessageClients
              (senderDomain, senderUser, senderClient)
              qualifiedClients
              (flattenMap $ qualifiedNewOtrRecipients msg)
              (qualifiedNewOtrClientMismatchStrategy msg)
          otrResult = mkMessageSendingStatus nowMillis mismatch mempty
      unless sendMessage $ do
        let lhProtectee = qualifiedUserToProtectee localDomain senderType sender
            missingClients = qmMissing mismatch
            legalholdErr = pure MessageNotSentLegalhold
            clientMissingErr = pure $ MessageNotSentClientMissing otrResult
        e <-
          runLocalInput lcnv
            . eitherM (const legalholdErr) (const clientMissingErr)
            . runError @LegalholdConflicts
            $ guardQualifiedLegalholdPolicyConflicts lhProtectee missingClients
        throw e
      failedToSend <-
        sendMessages @'NormalMessage
          now
          sender
          senderClient
          mconn
          lcnv
          botMap
          (qualifiedNewOtrMetadata msg)
          validMessages

      let -- List of the clients that are initially flagged as redundant.
          redundant' = toDomUserClient $ qualifiedUserClients $ mssRedundantClients otrResult
          -- List of users that we couldn't fetch clients for. Used to get their "redundant"
          -- clients for reporting as failedToSend.
          failed' = toDomUserClient $ qualifiedUserClients failedToSendFetchingClients
          -- failedToSendFetchingClients doesn't contain client IDs, so those need to be excluded
          -- from the filter search. We have to focus on only the domain and user. These clients
          -- should be listed in the failedToSend field however, as tracking these clients is an
          -- important part of the proteus protocol.
          predicate (d, (u, _)) = any (\(d', (u', _)) -> d == d' && u == u') failed'
          -- Failed users/clients aren't redundant
          (failed, redundant) = partition predicate redundant'
      pure
        otrResult
          { mssFailedToSend =
              QualifiedUserClients $
                collectFailedToSend
                  [ qualifiedUserClients failedToSend,
                    qualifiedUserClients failedToSendFetchingClients,
                    fromDomUserClient failed
                  ],
            mssRedundantClients = QualifiedUserClients $ fromDomUserClient redundant
          }
  where
    -- Get the triples for domains, users, and clients so we can easily filter
    -- out the values from redundant clients that should be in failed to send.
    toDomUserClient :: Map Domain (Map UserId (Set ClientId)) -> [(Domain, (UserId, Set ClientId))]
    toDomUserClient m = do
      (d, m') <- Map.assocs m
      (d,) <$> Map.assocs m'
    -- Rebuild the map, concatenating results along the way.
    fromDomUserClient :: [(Domain, (UserId, Set ClientId))] -> Map Domain (Map UserId (Set ClientId))
    fromDomUserClient = foldr buildUserClientMap mempty
      where
        buildUserClientMap :: (Domain, (UserId, Set ClientId)) -> Map Domain (Map UserId (Set ClientId)) -> Map Domain (Map UserId (Set ClientId))
        buildUserClientMap (d, (u, c)) m = Map.alter (pure . Map.alter (pure . Set.union c . fromMaybe mempty) u . fromMaybe mempty) d m

collectFailedToSend ::
  Foldable f =>
  f (Map Domain (Map UserId (Set ClientId))) ->
  Map Domain (Map UserId (Set ClientId))
collectFailedToSend = foldr (Map.unionWith (Map.unionWith Set.union)) mempty

makeUserMap :: Set UserId -> Map UserId (Set ClientId) -> Map UserId (Set ClientId)
makeUserMap keys = (<> Map.fromSet (const mempty) keys)

-- | Send both local and remote messages, return the set of clients for which
-- sending has failed.
sendMessages ::
  forall t r.
  ( t ~ 'NormalMessage,
    ( Member GundeckAccess r,
      Member ExternalAccess r,
      Member FederatorAccess r,
      Member P.TinyLog r
    )
  ) =>
  UTCTime ->
  Qualified UserId ->
  ClientId ->
  Maybe ConnId ->
  Local ConvId ->
  BotMap ->
  MessageMetadata ->
  Map (Domain, UserId, ClientId) ByteString ->
  Sem r QualifiedUserClients
sendMessages now sender senderClient mconn lcnv botMap metadata messages = do
  let messageMap = byDomain $ fmap toBase64Text messages
  let send dom =
        foldQualified
          lcnv
          (\l -> sendLocalMessages @t l now sender senderClient mconn (Just (tUntagged lcnv)) botMap metadata)
          (\r -> sendRemoteMessages r now sender senderClient lcnv metadata)
          (Qualified () dom)
  mkQualifiedUserClientsByDomain <$> Map.traverseWithKey send messageMap

sendBroadcastMessages ::
  Member GundeckAccess r =>
  Local x ->
  UTCTime ->
  Qualified UserId ->
  ClientId ->
  Maybe ConnId ->
  MessageMetadata ->
  Map (Domain, UserId, ClientId) ByteString ->
  Sem r QualifiedUserClients
sendBroadcastMessages loc now sender senderClient mconn metadata messages = do
  let messageMap = byDomain $ fmap toBase64Text messages
      localMessages = Map.findWithDefault mempty (tDomain loc) messageMap
  failed <- sendLocalMessages @'Broadcast loc now sender senderClient mconn Nothing mempty metadata localMessages
  pure . mkQualifiedUserClientsByDomain $ Map.singleton (tDomain loc) failed

byDomain :: Map (Domain, UserId, ClientId) a -> Map Domain (Map (UserId, ClientId) a)
byDomain =
  Map.foldrWithKey
    (\(d, u, c) t -> Map.insertWith (<>) d (Map.singleton (u, c) t))
    mempty

sendLocalMessages ::
  forall t r x.
  ( SingI t,
    Members (MessagePushEffects t) r,
    Monoid (MessagePush t)
  ) =>
  Local x ->
  UTCTime ->
  Qualified UserId ->
  ClientId ->
  Maybe ConnId ->
  Maybe (Qualified ConvId) ->
  BotMap ->
  MessageMetadata ->
  Map (UserId, ClientId) Text ->
  Sem r (Set (UserId, ClientId))
sendLocalMessages loc now sender senderClient mconn qcnv botMap metadata localMessages = do
  let events =
        localMessages
          & reindexed (first (qualifyAs loc)) itraversed
            %@~ newMessageEvent
              qcnv
              sender
              senderClient
              (mmData metadata)
              now
      pushes =
        events
          & itraversed
            %@~ newMessagePush loc botMap mconn metadata
  runMessagePush @t loc qcnv (pushes ^. traversed)
  pure mempty

-- | Send remote messages to the backend given by the domain argument, and
-- return the set of clients for which sending has failed. In case there was no
-- failure, the empty set is returned.
sendRemoteMessages ::
  forall r x.
  ( Member FederatorAccess r,
    Member P.TinyLog r
  ) =>
  Remote x ->
  UTCTime ->
  Qualified UserId ->
  ClientId ->
  Local ConvId ->
  MessageMetadata ->
  Map (UserId, ClientId) Text ->
  Sem r (Set (UserId, ClientId))
sendRemoteMessages domain now sender senderClient lcnv metadata messages = (handle =<<) $ do
  let rcpts =
        foldr
          (\((u, c), t) -> Map.insertWith (<>) u (Map.singleton c t))
          mempty
          (Map.assocs messages)
      rm =
        RemoteMessage
          { rmTime = now,
            rmData = mmData metadata,
            rmSender = sender,
            rmSenderClient = senderClient,
            rmConversation = tUnqualified lcnv,
            rmPriority = mmNativePriority metadata,
            rmPush = mmNativePush metadata,
            rmTransient = mmTransient metadata,
            rmRecipients = UserClientMap rcpts
          }
  let rpc = fedClient @'Galley @"on-message-sent" rm
  runFederatedEither domain rpc
  where
    handle :: Either FederationError a -> Sem r (Set (UserId, ClientId))
    handle (Right _) = pure mempty
    handle (Left e) = do
      P.warn $
        Log.field "conversation" (toByteString' (tUnqualified lcnv))
          Log.~~ Log.field "domain" (toByteString' (tDomain domain))
          Log.~~ Log.field "exception" (encode (federationErrorToWai e))
          Log.~~ Log.msg ("Remote message sending failed" :: Text)
      pure (Map.keysSet messages)

flattenMap :: QualifiedOtrRecipients -> Map (Domain, UserId, ClientId) ByteString
flattenMap (QualifiedOtrRecipients (QualifiedUserClientMap m)) =
  toMapOf (reindexed (\(d, (u, c)) -> (d, u, c)) (itraversed <.> itraversed <.> itraversed)) m

newMessageEvent ::
  Maybe (Qualified ConvId) ->
  Qualified UserId ->
  ClientId ->
  Maybe Text ->
  UTCTime ->
  (Local UserId, ClientId) ->
  Text ->
  Event
newMessageEvent mconvId sender senderClient dat time (receiver, receiverClient) cipherText =
  let convId = fromMaybe (tUntagged (fmap selfConv receiver)) mconvId
   in Event convId Nothing sender time . EdOtrMessage $
        OtrMessage
          { otrSender = senderClient,
            otrRecipient = receiverClient,
            otrCiphertext = cipherText,
            otrData = dat
          }

-- unqualified

legacyClientMismatchStrategy :: Domain -> Maybe [UserId] -> Maybe IgnoreMissing -> Maybe ReportMissing -> ClientMismatchStrategy
legacyClientMismatchStrategy localDomain (Just uids) _ _ =
  MismatchReportOnly (Set.fromList (map (`Qualified` localDomain) uids))
legacyClientMismatchStrategy _ Nothing (Just IgnoreMissingAll) _ = MismatchIgnoreAll
legacyClientMismatchStrategy localDomain Nothing (Just (IgnoreMissingList uids)) _ =
  MismatchIgnoreOnly (Set.map (`Qualified` localDomain) uids)
legacyClientMismatchStrategy _ Nothing Nothing (Just ReportMissingAll) = MismatchReportAll
legacyClientMismatchStrategy localDomain Nothing Nothing (Just (ReportMissingList uids)) =
  MismatchReportOnly (Set.map (`Qualified` localDomain) uids)
legacyClientMismatchStrategy _ Nothing Nothing Nothing = MismatchReportAll

class Unqualify a b where
  unqualify :: Domain -> a -> b

instance Unqualify a a where
  unqualify _ = id

instance Unqualify MessageSendingStatus ClientMismatch where
  unqualify domain status =
    ClientMismatch
      { cmismatchTime = mssTime status,
        missingClients = unqualify domain (mssMissingClients status),
        redundantClients = unqualify domain (mssRedundantClients status),
        deletedClients = unqualify domain (mssDeletedClients status)
      }

instance Unqualify QualifiedUserClients UserClients where
  unqualify domain =
    UserClients
      . Map.findWithDefault mempty domain
      . qualifiedUserClients

instance Unqualify a b => Unqualify (PostOtrResponse a) (PostOtrResponse b) where
  unqualify domain (Left a) = Left (unqualify domain <$> a)
  unqualify domain (Right a) = Right (unqualify domain a)
