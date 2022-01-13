module Galley.API.Message where

import Control.Lens
import Control.Monad.Except (throwError)
import Control.Monad.Extra (eitherM)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Data.ByteString.Conversion (toByteString')
import Data.Domain (Domain)
import Data.Id (ClientId, ConnId, ConvId, UserId)
import Data.Json.Util
  ( Base64ByteString (..),
    UTCTimeMillis,
    toBase64Text,
    toUTCTimeMillis,
  )
import Data.List1 (singleton)
import qualified Data.Map as Map
import Data.Map.Lens (toMapOf)
import Data.Qualified
import qualified Data.Set as Set
import Data.Set.Lens
import Data.Time.Clock (UTCTime)
import Galley.API.LegalHold.Conflicts
import Galley.API.Util
import Galley.Data.Services as Data
import Galley.Effects
import Galley.Effects.BrigAccess
import Galley.Effects.ClientStore
import Galley.Effects.ConversationStore
import Galley.Effects.ExternalAccess
import Galley.Effects.FederatorAccess
import Galley.Effects.GundeckAccess hiding (Push)
import Galley.Effects.MemberStore
import Galley.Intra.Push
import Galley.Options
import qualified Galley.Types.Clients as Clients
import Galley.Types.Conversations.Members
import Gundeck.Types.Push.V2 (RecipientClients (..))
import Imports hiding (forkIO)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import qualified Polysemy.TinyLog as P
import qualified System.Logger.Class as Log
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.Message
import Wire.API.Team.LegalHold
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
  Member FederatorAccess r =>
  [RemoteMember] ->
  Sem r (Map (Domain, UserId) (Set ClientId))
getRemoteClients remoteMembers =
  -- concatenating maps is correct here, because their sets of keys are disjoint
  mconcat . map tUnqualified
    <$> runFederatedConcurrently (map rmId remoteMembers) getRemoteClientsFromDomain
  where
    getRemoteClientsFromDomain (qUntagged -> Qualified uids domain) =
      Map.mapKeys (domain,) . fmap (Set.map pubClientId) . userMap
        <$> fedClient @'Brig @VL @"get-user-clients" (GetUserClients uids)

-- FUTUREWORK: sender should be Local UserId
postRemoteOtrMessage ::
  Members '[FederatorAccess] r =>
  Qualified UserId ->
  Remote ConvId ->
  LByteString ->
  Sem r (PostOtrResponse MessageSendingStatus)
postRemoteOtrMessage sender conv rawMsg = do
  let msr =
        MessageSendRequest
          { msrConvId = tUnqualified conv,
            msrSender = qUnqualified sender,
            msrRawMessage = Base64ByteString rawMsg
          }
      rpc = fedClient @'Galley @VL @"send-message" msr
  msResponse <$> runFederated conv rpc

postQualifiedOtrMessage ::
  Members
    '[ BrigAccess,
       ClientStore,
       ConversationStore,
       FederatorAccess,
       GundeckAccess,
       ExternalAccess,
       Input (Local ()), -- FUTUREWORK: remove this
       Input Opts,
       Input UTCTime,
       MemberStore,
       TeamStore,
       P.TinyLog
     ]
    r =>
  UserType ->
  Qualified UserId ->
  Maybe ConnId ->
  Local ConvId ->
  QualifiedNewOtrMessage ->
  Sem r (PostOtrResponse MessageSendingStatus)
postQualifiedOtrMessage senderType sender mconn lcnv msg = runExceptT $ do
  alive <- lift $ isConversationAlive (tUnqualified lcnv)
  let localDomain = tDomain lcnv
  now <- lift $ input
  let nowMillis = toUTCTimeMillis now
  let senderDomain = qDomain sender
      senderUser = qUnqualified sender
  let senderClient = qualifiedNewOtrSender msg
  unless alive $ do
    lift $ deleteConversation (tUnqualified lcnv)
    throwError MessageNotSentConversationNotFound

  -- conversation members
  localMembers <- lift $ getLocalMembers (tUnqualified lcnv)
  remoteMembers <- lift $ getRemoteMembers (tUnqualified lcnv)

  let localMemberIds = lmId <$> localMembers
      localMemberMap :: Map UserId LocalMember
      localMemberMap = Map.fromList (map (\mem -> (lmId mem, mem)) localMembers)
      members :: Set (Qualified UserId)
      members =
        Set.map (`Qualified` localDomain) (Map.keysSet localMemberMap)
          <> Set.fromList (map (qUntagged . rmId) remoteMembers)
  isInternal <- lift $ view (optSettings . setIntraListing) <$> input

  -- check if the sender is part of the conversation
  unless (Set.member sender members) $
    throwError MessageNotSentConversationNotFound

  -- get local clients
  localClients <-
    lift $
      if isInternal
        then Clients.fromUserClients <$> lookupClients localMemberIds
        else getClients localMemberIds
  let qualifiedLocalClients =
        Map.mapKeys (localDomain,)
          . makeUserMap (Set.fromList (map lmId localMembers))
          . Clients.toMap
          $ localClients

  -- get remote clients
  qualifiedRemoteClients <- lift $ getRemoteClients remoteMembers
  let qualifiedClients = qualifiedLocalClients <> qualifiedRemoteClients

  -- check if the sender client exists (as one of the clients in the conversation)
  unless
    ( Set.member
        senderClient
        (Map.findWithDefault mempty (senderDomain, senderUser) qualifiedClients)
    )
    $ throwError MessageNotSentUnknownClient

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
      lift
        . runLocalInput lcnv
        . eitherM (const legalholdErr) (const clientMissingErr)
        . runError @LegalholdConflicts
        $ guardQualifiedLegalholdPolicyConflicts lhProtectee missingClients
    throwError e

  failedToSend <-
    lift $
      sendMessages
        now
        sender
        senderClient
        mconn
        lcnv
        localMemberMap
        (qualifiedNewOtrMetadata msg)
        validMessages
  pure otrResult {mssFailedToSend = failedToSend}
  where
    makeUserMap :: Set UserId -> Map UserId (Set ClientId) -> Map UserId (Set ClientId)
    makeUserMap keys = (<> Map.fromSet (const mempty) keys)

-- | Send both local and remote messages, return the set of clients for which
-- sending has failed.
sendMessages ::
  Members '[GundeckAccess, ExternalAccess, FederatorAccess, Input (Local ()), P.TinyLog] r =>
  -- FUTUREWORK: remove Input (Local ()) effect
  UTCTime ->
  Qualified UserId ->
  ClientId ->
  Maybe ConnId ->
  Local ConvId ->
  Map UserId LocalMember ->
  MessageMetadata ->
  Map (Domain, UserId, ClientId) ByteString ->
  Sem r QualifiedUserClients
sendMessages now sender senderClient mconn lcnv localMemberMap metadata messages = do
  let messageMap = byDomain $ fmap toBase64Text messages
  let send dom =
        foldQualified
          lcnv
          (\_ -> sendLocalMessages now sender senderClient mconn (qUntagged lcnv) localMemberMap metadata)
          (\r -> sendRemoteMessages r now sender senderClient lcnv metadata)
          (Qualified () dom)

  mkQualifiedUserClientsByDomain <$> Map.traverseWithKey send messageMap
  where
    byDomain :: Map (Domain, UserId, ClientId) a -> Map Domain (Map (UserId, ClientId) a)
    byDomain =
      Map.foldrWithKey
        (\(d, u, c) t -> Map.insertWith (<>) d (Map.singleton (u, c) t))
        mempty

sendLocalMessages ::
  Members '[GundeckAccess, ExternalAccess, Input (Local ()), P.TinyLog] r =>
  UTCTime ->
  Qualified UserId ->
  ClientId ->
  Maybe ConnId ->
  Qualified ConvId ->
  Map UserId LocalMember ->
  MessageMetadata ->
  Map (UserId, ClientId) Text ->
  Sem r (Set (UserId, ClientId))
sendLocalMessages now sender senderClient mconn qcnv localMemberMap metadata localMessages = do
  loc <- qualifyLocal ()
  let events =
        localMessages & reindexed snd itraversed
          %@~ newMessageEvent
            qcnv
            sender
            senderClient
            (mmData metadata)
            now
      pushes =
        events & itraversed
          %@~ newMessagePush loc localMemberMap mconn metadata
  runMessagePush qcnv (pushes ^. traversed)
  pure mempty

sendRemoteMessages ::
  forall r x.
  Members '[FederatorAccess, P.TinyLog] r =>
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
  let rpc = fedClient @'Galley @VL @"on-message-sent" rm
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

flatten :: Map Domain (Map UserId (Set ClientId)) -> Set (Domain, UserId, ClientId)
flatten =
  setOf $
    (itraversed <.> itraversed <. folded)
      . withIndex
      . to (\((d, u), c) -> (d, u, c))

flattenMap :: QualifiedOtrRecipients -> Map (Domain, UserId, ClientId) ByteString
flattenMap (QualifiedOtrRecipients (QualifiedUserClientMap m)) =
  toMapOf (reindexed (\(d, (u, c)) -> (d, u, c)) (itraversed <.> itraversed <.> itraversed)) m

data MessagePush = MessagePush
  { userPushes :: [Push],
    botPushes :: [(BotMember, Event)]
  }

instance Semigroup MessagePush where
  MessagePush us1 bs1 <> MessagePush us2 bs2 = MessagePush (us1 <> us2) (bs1 <> bs2)

instance Monoid MessagePush where
  mempty = MessagePush mempty mempty

newUserPush :: Push -> MessagePush
newUserPush p = MessagePush {userPushes = pure p, botPushes = mempty}

newBotPush :: BotMember -> Event -> MessagePush
newBotPush b e = MessagePush {userPushes = mempty, botPushes = pure (b, e)}

runMessagePush ::
  forall r.
  Members '[GundeckAccess, ExternalAccess, Input (Local ()), P.TinyLog] r =>
  Qualified ConvId ->
  MessagePush ->
  Sem r ()
runMessagePush qcnv mp = do
  push (userPushes mp)
  pushToBots (botPushes mp)
  where
    pushToBots :: [(BotMember, Event)] -> Sem r ()
    pushToBots pushes = do
      localDomain <- tDomain <$> qualifyLocal ()
      if localDomain /= qDomain qcnv
        then unless (null pushes) $ do
          P.warn $ Log.msg ("Ignoring messages for local bots in a remote conversation" :: ByteString) . Log.field "conversation" (show qcnv)
        else deliverAndDeleteAsync (qUnqualified qcnv) pushes

newMessageEvent :: Qualified ConvId -> Qualified UserId -> ClientId -> Maybe Text -> UTCTime -> ClientId -> Text -> Event
newMessageEvent convId sender senderClient dat time receiverClient cipherText =
  Event OtrMessageAdd convId sender time . EdOtrMessage $
    OtrMessage
      { otrSender = senderClient,
        otrRecipient = receiverClient,
        otrCiphertext = cipherText,
        otrData = dat
      }

newMessagePush ::
  Ord k =>
  Local x ->
  Map k LocalMember ->
  Maybe ConnId ->
  MessageMetadata ->
  (k, ClientId) ->
  Event ->
  MessagePush
newMessagePush loc members mconn mm (k, client) e = fromMaybe mempty $ do
  member <- Map.lookup k members
  newBotMessagePush member <|> newUserMessagePush member
  where
    newBotMessagePush :: LocalMember -> Maybe MessagePush
    newBotMessagePush member = newBotPush <$> newBotMember member <*> pure e
    newUserMessagePush :: LocalMember -> Maybe MessagePush
    newUserMessagePush member =
      fmap newUserPush $
        newConversationEventPush e (qualifyAs loc [lmId member])
          <&> set pushConn mconn
            . set pushNativePriority (mmNativePriority mm)
            . set pushRoute (bool RouteDirect RouteAny (mmNativePush mm))
            . set pushTransient (mmTransient mm)
            . set (pushRecipients . traverse . recipientClients) (RecipientClientsSome (singleton client))

data MessageMetadata = MessageMetadata
  { mmNativePush :: Bool,
    mmTransient :: Bool,
    mmNativePriority :: Maybe Priority,
    mmData :: Maybe Text
  }
  deriving (Eq, Ord, Show)

qualifiedNewOtrMetadata :: QualifiedNewOtrMessage -> MessageMetadata
qualifiedNewOtrMetadata msg =
  MessageMetadata
    { mmNativePush = qualifiedNewOtrNativePush msg,
      mmTransient = qualifiedNewOtrTransient msg,
      mmNativePriority = qualifiedNewOtrNativePriority msg,
      mmData = Just . toBase64Text $ qualifiedNewOtrData msg
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
