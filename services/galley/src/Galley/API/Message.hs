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
import Data.Aeson (encode)
import Data.Bifunctor
import Data.ByteString.Conversion (toByteString')
import Data.Domain (Domain)
import Data.Id
import Data.Json.Util
import Data.Map qualified as Map
import Data.Map.Lens (toMapOf)
import Data.Qualified
import Data.Range
import Data.Set qualified as Set
import Data.Set.Lens
import Data.Time.Clock (UTCTime)
import Galley.API.LegalHold.Conflicts
import Galley.API.Push
import Galley.API.Util
import Galley.Data.Conversation
import Galley.Data.Services
import Galley.Effects
import Galley.Effects.BackendNotificationQueueAccess
import Galley.Effects.BrigAccess
import Galley.Effects.ClientStore
import Galley.Effects.ConversationStore
import Galley.Effects.FederatorAccess
import Galley.Effects.TeamStore
import Galley.Options
import Galley.Types.Clients qualified as Clients
import Galley.Types.Conversations.Members
import Imports hiding (forkIO)
import Network.AMQP qualified as Q
import Polysemy hiding (send)
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog qualified as P
import System.Logger.Class qualified as Log
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
import Wire.NotificationSubsystem (NotificationSubsystem)

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

mkMessageSendingStatus :: UTCTimeMillis -> QualifiedMismatch -> MessageSendingStatus
mkMessageSendingStatus time mismatch =
  MessageSendingStatus
    { mssTime = time,
      mssMissingClients = qmMissing mismatch,
      mssRedundantClients = qmRedundant mismatch,
      mssDeletedClients = qmDeleted mismatch,
      mssFailedToSend = mempty,
      mssFailedToConfirmClients = mempty
    }

clientMismatchStrategyApply :: ClientMismatchStrategy -> QualifiedRecipientSet -> QualifiedRecipientSet
clientMismatchStrategyApply MismatchReportAll = Imports.id
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
--    |           |                 |                 |                    | <----------+
--    |           |                 |                 |   Deleted Clients  |            |
--    |           |                 |                 |   (clients of users from conv   |
-- Expected       |                 |                 |   that have been deleted)       |   Recipients
-- Clients        |  Missing        | Valid           |       Extra        |                (from the request)
-- (actually in   |  Clients        | Clients         +-------Clients------+----------+
-- conversation)  |                 | (these will     |                    |          |
--                |                 | actually receive|                    |          |
--                |                 | the msg)        |       Redundant Clients     <------- Sender Client
--                |                 |                 |       (clients that are not   |
--                |                 |                 |       part of conv for        |
--                |                 |                 |       whatever reason + sender)
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
  let participants = setOf ((itraversed <. folded) . withIndex . Control.Lens.to (\((d, u), c) -> (d, u, c))) participantMap
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
      -- are not in the conversation and hence considered redundant.
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

postRemoteOtrMessage ::
  (Member FederatorAccess r) =>
  Local UserId ->
  Remote ConvId ->
  ByteString ->
  Sem r (PostOtrResponse MessageSendingStatus)
postRemoteOtrMessage sender conv rawMsg = do
  let msr =
        ProteusMessageSendRequest
          { convId = tUnqualified conv,
            sender = qUnqualified (tUntagged sender),
            rawMessage = Base64ByteString rawMsg
          }
      rpc = fedClient @'Galley @"send-message" msr
  (.response) <$> runFederated conv rpc

postBroadcast ::
  ( Member BrigAccess r,
    Member ClientStore r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'NonBindingTeam) r,
    Member (ErrorS 'BroadcastLimitExceeded) r,
    Member ExternalAccess r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member TeamStore r,
    Member P.TinyLog r,
    Member NotificationSubsystem r
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
    fmap (view Wire.API.Team.Member.userId) <$> case qualifiedNewOtrClientMismatchStrategy msg of
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

  localClients <- getBrigClients users
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
      otrResult = mkMessageSendingStatus (toUTCTimeMillis now) mismatch

  guardQualifiedLegalholdPolicyConflictsWrapper User (tUntagged lusr) localClients [] lusr

  unless sendMessage $ do
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
    Member BackendNotificationQueueAccess r,
    Member ExternalAccess r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member TeamStore r,
    Member P.TinyLog r,
    Member NotificationSubsystem r
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
      unless (protocolTag (convProtocol conv) `elem` [ProtocolProteusTag, ProtocolMixedTag]) $
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
      isInternal <- view (settings . intraListing) <$> input

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
      qualifiedRemoteClients :: [Either (Remote [UserId], FederationError) (Map (Domain, UserId) (Set ClientId))] <-
        getRemoteClients (convRemoteMembers conv)
      let -- concatenating maps is correct here, because their sets of keys are disjoint
          qualifiedRemoteClients' = mconcat $ rights qualifiedRemoteClients
          -- Try to get the client IDs for the users that we failed to fetch clients for from the recipient list.
          -- Partition the list of users into those that we were able to find clients for and those that we weren't.
          (unconfirmedKnownClients, unconfirmedUnknownClients) =
            partition
              (not . all Set.null)
              (matchUnconfirmedClientsWithRecipients (fst <$> lefts qualifiedRemoteClients))
          qualifiedClients =
            qualifiedLocalClients
              <> qualifiedRemoteClients'
              <> Map.fromList unconfirmedKnownClients
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
          otrResult = mkMessageSendingStatus nowMillis mismatch

      -- throw error if there is a legalhold policy conflict
      guardQualifiedLegalholdPolicyConflictsWrapper senderType sender localClients qualifiedRemoteClients lcnv

      -- throw error if clients are missing
      unless sendMessage $ do
        throw $ MessageNotSentClientMissing otrResult

      failedToSend <-
        sendMessages
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
          failed' = toDomUserClient $ toDomMap unconfirmedUnknownClients
          -- failedToConfirmRemoteClients doesn't contain client IDs, so those need to be excluded
          -- from the filter search. We have to focus on only the domain and user. These clients
          -- should be listed in the failedToSend field however, as tracking these clients is an
          -- important part of the proteus protocol.
          predicate (d, (u, _)) = any (\(d', (u', _)) -> d == d' && u == u') failed'
          -- Failed users/clients aren't redundant
          (failed, redundant) = partition predicate redundant'
          collectedFailedToSend = collectFailedToSend [qualifiedUserClients failedToSend, toDomMap unconfirmedUnknownClients, fromDomUserClient failed]
      pure
        otrResult
          { mssFailedToSend = QualifiedUserClients collectedFailedToSend,
            mssRedundantClients = QualifiedUserClients $ fromDomUserClient redundant,
            mssFailedToConfirmClients = QualifiedUserClients $ collectFailedToSend $ [toDomMap unconfirmedKnownClients, collectedFailedToSend]
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

    toDomMap :: [((Domain, UserId), Set ClientId)] -> Map Domain (Map UserId (Set ClientId))
    toDomMap = fromDomUserClient . fmap (\((d, u), s) -> (d, (u, s)))

    matchUnconfirmedClientsWithRecipients :: [Remote [UserId]] -> [((Domain, UserId), Set ClientId)]
    matchUnconfirmedClientsWithRecipients remotes = do
      remoteUsers@(qDomain . tUntagged -> domain) <- remotes
      user <- tUnqualified remoteUsers
      pure ((domain, user), tryFindClientIds domain user)

    tryFindClientIds :: Domain -> UserId -> Set ClientId
    tryFindClientIds domain uid = do
      Set.fromList $
        Map.keys $
          Map.findWithDefault mempty uid $
            Map.findWithDefault mempty domain $
              qualifiedUserClientMap $
                qualifiedOtrRecipientsMap $
                  qualifiedNewOtrRecipients msg

guardQualifiedLegalholdPolicyConflictsWrapper ::
  ( Member BrigAccess r,
    Member (Error (MessageNotSent MessageSendingStatus)) r,
    Member (Input Opts) r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  UserType ->
  Qualified UserId ->
  Clients.Clients ->
  [Either (Remote [UserId], FederationError) (Map (Domain, UserId) (Set ClientId))] ->
  Local any ->
  Sem r ()
guardQualifiedLegalholdPolicyConflictsWrapper senderType sender localClients qualifiedRemoteClients lany = do
  wrapper $ guardQualifiedLegalholdPolicyConflicts lhProtectee allReceivingClients
  where
    localDomain = tDomain lany
    lhProtectee = qualifiedUserToProtectee localDomain senderType sender

    allReceivingClients = mkQualifiedUserClients $ parseLocal localClients <> parseRemote qualifiedRemoteClients
      where
        parseLocal :: Clients.Clients -> QualifiedRecipientSet
        parseLocal =
          Set.fromList
            . mconcat
            . fmap (\(uid, cids) -> (localDomain,uid,) <$> cids)
            . Clients.toList

        parseRemote :: [Either (Remote [UserId], FederationError) (Map (Domain, UserId) (Set ClientId))] -> QualifiedRecipientSet
        parseRemote =
          Set.fromList
            . mconcat
            . fmap (\((dom, uid), Set.toList -> cids) -> (dom,uid,) <$> cids)
            . mconcat
            . fmap Map.toList
            . rights

    wrapper =
      runLocalInput lany
        . mapError @LegalholdConflicts @(MessageNotSent MessageSendingStatus) (const MessageNotSentLegalhold)
        . mapError @LegalholdConflictsOldClients @(MessageNotSent MessageSendingStatus) (const MessageNotSentLegalholdOldClients)

-- FUTUREWORK: This is just a workaround and would not be needed if we had a proper monoid/semigroup instance for Map where the values have a monoid instance.
collectFailedToSend ::
  (Foldable f) =>
  f (Map Domain (Map UserId (Set ClientId))) ->
  Map Domain (Map UserId (Set ClientId))
collectFailedToSend = foldr (Map.unionWith (Map.unionWith Set.union)) mempty

makeUserMap :: Set UserId -> Map UserId (Set ClientId) -> Map UserId (Set ClientId)
makeUserMap keys = (<> Map.fromSet (const mempty) keys)

-- | Send both local and remote messages, return the set of clients for which
-- sending has failed.
sendMessages ::
  forall r.
  ( Member ExternalAccess r,
    Member BackendNotificationQueueAccess r,
    Member P.TinyLog r,
    Member NotificationSubsystem r
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
          (\l -> sendLocalMessages l now sender senderClient mconn (Just (tUntagged lcnv)) botMap metadata)
          (\r -> sendRemoteMessages r now sender senderClient lcnv metadata)
          (Qualified () dom)
  mkQualifiedUserClientsByDomain <$> Map.traverseWithKey send messageMap

sendBroadcastMessages ::
  ( Member ExternalAccess r,
    Member P.TinyLog r,
    Member NotificationSubsystem r
  ) =>
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
  failed <- sendLocalMessages loc now sender senderClient mconn Nothing mempty metadata localMessages
  pure . mkQualifiedUserClientsByDomain $ Map.singleton (tDomain loc) failed

byDomain :: Map (Domain, UserId, ClientId) a -> Map Domain (Map (UserId, ClientId) a)
byDomain =
  Map.foldrWithKey
    (\(d, u, c) t -> Map.insertWith (<>) d (Map.singleton (u, c) t))
    mempty

sendLocalMessages ::
  forall r x.
  ( Member ExternalAccess r,
    Member P.TinyLog r,
    Member NotificationSubsystem r
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
            %@~ (\(u, c) -> newMessagePush botMap mconn metadata [(u, c)])
  for_ pushes $ runMessagePush loc qcnv
  pure mempty

-- | Send remote messages to the backend given by the domain argument, and
-- return the set of clients for which sending has failed. In case there was no
-- failure, the empty set is returned.
sendRemoteMessages ::
  forall r x.
  ( Member BackendNotificationQueueAccess r,
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
sendRemoteMessages domain now sender senderClient lcnv metadata messages =
  -- FUTUREWORK: a FederationError here just means that queueing did not work.
  -- It should not result in clients ending up in failedToSend.
  (handle <=< runError) $ do
    let rcpts =
          foldr
            (\((u, c), t) -> Map.insertWith (<>) u (Map.singleton c t))
            mempty
            (Map.assocs messages)
        rm =
          RemoteMessage
            { time = now,
              _data = mmData metadata,
              sender = sender,
              senderClient = senderClient,
              conversation = tUnqualified lcnv,
              priority = mmNativePriority metadata,
              push = mmNativePush metadata,
              transient = mmTransient metadata,
              recipients = UserClientMap rcpts
            }
    enqueueNotification Q.Persistent domain (fedQueueClient @'OnMessageSentTag rm)
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
  unqualify _ = Imports.id

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

instance (Unqualify a b) => Unqualify (PostOtrResponse a) (PostOtrResponse b) where
  unqualify domain (Left a) = Left (unqualify domain <$> a)
  unqualify domain (Right a) = Right (unqualify domain a)
