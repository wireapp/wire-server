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
import Data.Time.Clock (UTCTime, getCurrentTime)
import Galley.API.LegalHold.Conflicts (guardQualifiedLegalholdPolicyConflicts)
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import Galley.Data.Services as Data
import Galley.Effects
import qualified Galley.External as External
import qualified Galley.Intra.Client as Intra
import Galley.Intra.Push
import Galley.Options (optSettings, setIntraListing)
import qualified Galley.Types.Clients as Clients
import Galley.Types.Conversations.Members
import Gundeck.Types.Push.V2 (RecipientClients (..))
import Imports hiding (forkIO)
import qualified System.Logger.Class as Log
import Wire.API.Event.Conversation
import qualified Wire.API.Federation.API.Brig as FederatedBrig
import qualified Wire.API.Federation.API.Galley as FederatedGalley
import Wire.API.Federation.Client (FederationError, executeFederated)
import Wire.API.Federation.Error (federationErrorToWai)
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
  Galley r (Map (Domain, UserId) (Set ClientId))
getRemoteClients remoteMembers =
  -- concatenating maps is correct here, because their sets of keys are disjoint
  mconcat . map tUnqualified
    <$> runFederatedConcurrently (map rmId remoteMembers) getRemoteClientsFromDomain
  where
    getRemoteClientsFromDomain (qUntagged -> Qualified uids domain) =
      Map.mapKeys (domain,) . fmap (Set.map pubClientId) . userMap
        <$> FederatedBrig.getUserClients FederatedBrig.clientRoutes (FederatedBrig.GetUserClients uids)

postRemoteOtrMessage ::
  Member FederatorAccess r =>
  Qualified UserId ->
  Qualified ConvId ->
  LByteString ->
  Galley r (PostOtrResponse MessageSendingStatus)
postRemoteOtrMessage sender conv rawMsg = do
  let msr =
        FederatedGalley.MessageSendRequest
          { FederatedGalley.msrConvId = qUnqualified conv,
            FederatedGalley.msrSender = qUnqualified sender,
            FederatedGalley.msrRawMessage = Base64ByteString rawMsg
          }
      rpc = FederatedGalley.sendMessage FederatedGalley.clientRoutes (qDomain sender) msr
  FederatedGalley.msResponse <$> runFederatedGalley (qDomain conv) rpc

postQualifiedOtrMessage ::
  Members '[BotAccess, FederatorAccess, GundeckAccess, ExternalAccess] r =>
  UserType ->
  Qualified UserId ->
  Maybe ConnId ->
  ConvId ->
  QualifiedNewOtrMessage ->
  Galley r (PostOtrResponse MessageSendingStatus)
postQualifiedOtrMessage senderType sender mconn convId msg = runExceptT $ do
  alive <- lift $ Data.isConvAlive convId
  localDomain <- viewFederationDomain
  now <- liftIO getCurrentTime
  let nowMillis = toUTCTimeMillis now
  let senderDomain = qDomain sender
      senderUser = qUnqualified sender
  let senderClient = qualifiedNewOtrSender msg
  unless alive $ do
    lift $ Data.deleteConversation convId
    throwError MessageNotSentConversationNotFound

  -- conversation members
  localMembers <- lift $ Data.members convId
  remoteMembers <- lift $ Data.lookupRemoteMembers convId

  let localMemberIds = lmId <$> localMembers
      localMemberMap :: Map UserId LocalMember
      localMemberMap = Map.fromList (map (\mem -> (lmId mem, mem)) localMembers)
      members :: Set (Qualified UserId)
      members =
        Set.map (`Qualified` localDomain) (Map.keysSet localMemberMap)
          <> Set.fromList (map (qUntagged . rmId) remoteMembers)
  isInternal <- view $ options . optSettings . setIntraListing

  -- check if the sender is part of the conversation
  unless (Set.member sender members) $
    throwError MessageNotSentConversationNotFound

  -- get local clients
  localClients <-
    lift $
      if isInternal
        then Clients.fromUserClients <$> Intra.lookupClients localMemberIds
        else Data.lookupClients localMemberIds
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
    guardQualifiedLegalholdPolicyConflicts lhProtectee missingClients
      & eitherM (const legalholdErr) (const clientMissingErr)
      & lift
      >>= throwError

  failedToSend <-
    lift $
      sendMessages
        now
        sender
        senderClient
        mconn
        convId
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
  Members '[BotAccess, GundeckAccess, ExternalAccess] r =>
  UTCTime ->
  Qualified UserId ->
  ClientId ->
  Maybe ConnId ->
  ConvId ->
  Map UserId LocalMember ->
  MessageMetadata ->
  Map (Domain, UserId, ClientId) ByteString ->
  Galley r QualifiedUserClients
sendMessages now sender senderClient mconn conv localMemberMap metadata messages = do
  localDomain <- viewFederationDomain
  let messageMap = byDomain $ fmap toBase64Text messages
  let send dom
        | localDomain == dom =
          sendLocalMessages now sender senderClient mconn (Qualified conv localDomain) localMemberMap metadata
        | otherwise =
          sendRemoteMessages dom now sender senderClient conv metadata
  mkQualifiedUserClientsByDomain <$> Map.traverseWithKey send messageMap
  where
    byDomain :: Map (Domain, UserId, ClientId) a -> Map Domain (Map (UserId, ClientId) a)
    byDomain =
      Map.foldrWithKey
        (\(d, u, c) t -> Map.insertWith (<>) d (Map.singleton (u, c) t))
        mempty

sendLocalMessages ::
  Members '[BotAccess, GundeckAccess, ExternalAccess] r =>
  UTCTime ->
  Qualified UserId ->
  ClientId ->
  Maybe ConnId ->
  Qualified ConvId ->
  Map UserId LocalMember ->
  MessageMetadata ->
  Map (UserId, ClientId) Text ->
  Galley r (Set (UserId, ClientId))
sendLocalMessages now sender senderClient mconn conv localMemberMap metadata localMessages = do
  localDomain <- viewFederationDomain
  let events =
        localMessages & reindexed snd itraversed
          %@~ newMessageEvent
            conv
            sender
            senderClient
            (mmData metadata)
            now
      pushes =
        events & itraversed
          %@~ newMessagePush localDomain localMemberMap mconn metadata
  runMessagePush conv (pushes ^. traversed)
  pure mempty

sendRemoteMessages ::
  Domain ->
  UTCTime ->
  Qualified UserId ->
  ClientId ->
  ConvId ->
  MessageMetadata ->
  Map (UserId, ClientId) Text ->
  Galley r (Set (UserId, ClientId))
sendRemoteMessages domain now sender senderClient conv metadata messages = handle <=< runExceptT $ do
  let rcpts =
        foldr
          (\((u, c), t) -> Map.insertWith (<>) u (Map.singleton c t))
          mempty
          (Map.assocs messages)
      rm =
        FederatedGalley.RemoteMessage
          { FederatedGalley.rmTime = now,
            FederatedGalley.rmData = mmData metadata,
            FederatedGalley.rmSender = sender,
            FederatedGalley.rmSenderClient = senderClient,
            FederatedGalley.rmConversation = conv,
            FederatedGalley.rmPriority = mmNativePriority metadata,
            FederatedGalley.rmPush = mmNativePush metadata,
            FederatedGalley.rmTransient = mmTransient metadata,
            FederatedGalley.rmRecipients = UserClientMap rcpts
          }
  -- Semantically, the origin domain should be the converation domain. Here one
  -- backend has only one domain so we just pick it from the environment.
  originDomain <- viewFederationDomain
  let rpc = FederatedGalley.onMessageSent FederatedGalley.clientRoutes originDomain rm
  executeFederated domain rpc
  where
    handle :: Either FederationError a -> Galley r (Set (UserId, ClientId))
    handle (Right _) = pure mempty
    handle (Left e) = do
      Log.warn $
        Log.field "conversation" (toByteString' conv)
          Log.~~ Log.field "domain" (toByteString' domain)
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
  Members '[BotAccess, GundeckAccess, ExternalAccess] r =>
  Qualified ConvId ->
  MessagePush ->
  Galley r ()
runMessagePush cnv mp = do
  pushSome (userPushes mp)
  pushToBots (botPushes mp)
  where
    pushToBots :: [(BotMember, Event)] -> Galley r ()
    pushToBots pushes = do
      localDomain <- viewFederationDomain
      if localDomain /= qDomain cnv
        then unless (null pushes) $ do
          Log.warn $ Log.msg ("Ignoring messages for local bots in a remote conversation" :: ByteString) . Log.field "conversation" (show cnv)
        else External.deliverAndDeleteAsync (qUnqualified cnv) pushes

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
  Domain ->
  Map k LocalMember ->
  Maybe ConnId ->
  MessageMetadata ->
  (k, ClientId) ->
  Event ->
  MessagePush
newMessagePush localDomain members mconn mm (k, client) e = fromMaybe mempty $ do
  member <- Map.lookup k members
  newBotMessagePush member <|> newUserMessagePush member
  where
    newBotMessagePush :: LocalMember -> Maybe MessagePush
    newBotMessagePush member = newBotPush <$> newBotMember member <*> pure e
    newUserMessagePush :: LocalMember -> Maybe MessagePush
    newUserMessagePush member =
      fmap newUserPush $
        newConversationEventPush localDomain e [lmId member]
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
