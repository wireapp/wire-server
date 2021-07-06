module Galley.API.Message where

import Control.Lens
import Control.Monad.Catch (throwM)
import Control.Monad.Except (throwError)
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
import Data.Proxy
import Data.Qualified (Qualified (..), partitionRemote)
import Data.SOP (I (..), htrans, unI)
import qualified Data.Set as Set
import Data.Set.Lens
import Data.Tagged (unTagged)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Galley.API.Error (missingLegalholdConsent)
import Galley.API.LegalHold.Conflicts (guardQualifiedLegalholdPolicyConflicts)
import Galley.API.Util
  ( runFederatedBrig,
    runFederatedGalley,
    viewFederationDomain,
  )
import Galley.App
import qualified Galley.Data as Data
import Galley.Data.Services as Data
import qualified Galley.External as External
import qualified Galley.Intra.Client as Intra
import Galley.Intra.Push
import Galley.Intra.User
import Galley.Options (optSettings, setIntraListing)
import qualified Galley.Types.Clients as Clients
import Galley.Types.Conversations.Members
import Gundeck.Types.Push.V2 (RecipientClients (..))
import Imports
import qualified Servant
import Servant.API (Union, WithStatus (..))
import qualified System.Logger.Class as Log
import UnliftIO.Async
import Wire.API.ErrorDescription as ErrorDescription
import Wire.API.Event.Conversation
import qualified Wire.API.Federation.API.Brig as FederatedBrig
import qualified Wire.API.Federation.API.Galley as FederatedGalley
import Wire.API.Federation.Client (FederationError, executeFederated)
import Wire.API.Federation.Error (federationErrorToWai)
import Wire.API.Message
import qualified Wire.API.Message as Public
import Wire.API.Routes.Public.Galley as Public
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

getRemoteClients :: [RemoteMember] -> Galley (Map (Domain, UserId) (Set ClientId))
getRemoteClients remoteMembers = do
  fmap mconcat -- concatenating maps is correct here, because their sets of keys are disjoint
    . pooledMapConcurrentlyN 8 (uncurry getRemoteClientsFromDomain)
    . partitionRemote
    . map rmId
    $ remoteMembers
  where
    getRemoteClientsFromDomain :: Domain -> [UserId] -> Galley (Map (Domain, UserId) (Set ClientId))
    getRemoteClientsFromDomain domain uids = do
      let rpc = FederatedBrig.getUserClients FederatedBrig.clientRoutes (FederatedBrig.GetUserClients uids)
      Map.mapKeys (domain,) . fmap (Set.map pubClientId) . userMap <$> runFederatedBrig domain rpc

postRemoteOtrMessage ::
  Qualified UserId ->
  Qualified ConvId ->
  LByteString ->
  Galley (Union Public.PostOtrResponses)
postRemoteOtrMessage sender conv rawMsg = do
  let msr =
        FederatedGalley.MessageSendRequest
          { FederatedGalley.msrConvId = qUnqualified conv,
            FederatedGalley.msrSender = sender,
            FederatedGalley.msrRawMessage = Base64ByteString rawMsg
          }
      rpc = FederatedGalley.sendMessage FederatedGalley.clientRoutes msr
  mkPostOtrResponsesUnion . FederatedGalley.msResponse =<< runFederatedGalley (qDomain conv) rpc

mkPostOtrResponsesUnion :: Either FederatedGalley.MessageNotSent MessageSendingStatus -> Galley (Union Public.PostOtrResponses)
mkPostOtrResponsesUnion (Right mss) = Servant.respond (WithStatus @201 mss)
mkPostOtrResponsesUnion (Left reason) = case reason of
  FederatedGalley.MessageNotSentClientMissing mss -> Servant.respond (WithStatus @412 mss)
  FederatedGalley.MessageNotSentUnknownClient -> Servant.respond ErrorDescription.unknownClient
  FederatedGalley.MessageNotSentConversationNotFound -> Servant.respond ErrorDescription.convNotFound
  FederatedGalley.MessageNotSentLegalhold -> throwM missingLegalholdConsent

postQualifiedOtrMessage :: UserType -> Qualified UserId -> Maybe ConnId -> ConvId -> Public.QualifiedNewOtrMessage -> Galley (Either FederatedGalley.MessageNotSent Public.MessageSendingStatus)
postQualifiedOtrMessage senderType sender mconn convId msg = runExceptT $ do
  alive <- Data.isConvAlive convId
  localDomain <- viewFederationDomain
  now <- liftIO getCurrentTime
  let nowMillis = toUTCTimeMillis now
  let senderDomain = qDomain sender
      senderUser = qUnqualified sender
  let senderClient = qualifiedNewOtrSender msg
  unless alive $ do
    lift $ Data.deleteConversation convId
    throwError FederatedGalley.MessageNotSentConversationNotFound

  -- conversation members
  localMembers <- lift $ Data.members convId
  remoteMembers <- Data.lookupRemoteMembers convId

  let localMemberIds = memId <$> localMembers
      localMemberMap :: Map UserId LocalMember
      localMemberMap = Map.fromList (map (\mem -> (memId mem, mem)) localMembers)
      members :: Set (Qualified UserId)
      members =
        Set.map (`Qualified` localDomain) (Map.keysSet localMemberMap)
          <> Set.fromList (map (unTagged . rmId) remoteMembers)
  isInternal <- view $ options . optSettings . setIntraListing

  -- check if the sender is part of the conversation
  unless (Set.member sender members) $
    throwError FederatedGalley.MessageNotSentConversationNotFound

  -- get local clients
  localClients <-
    lift $
      if isInternal
        then Clients.fromUserClients <$> Intra.lookupClients localMemberIds
        else Data.lookupClients localMemberIds
  let qualifiedLocalClients =
        Map.mapKeys (localDomain,)
          . makeUserMap (Set.fromList (map memId localMembers))
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
    $ throwError FederatedGalley.MessageNotSentUnknownClient

  let (sendMessage, validMessages, mismatch) =
        checkMessageClients
          (senderDomain, senderUser, qualifiedNewOtrSender msg)
          qualifiedClients
          (flattenMap $ qualifiedNewOtrRecipients msg)
          (qualifiedNewOtrClientMismatchStrategy msg)
      otrResult = mkMessageSendingStatus nowMillis mismatch mempty
  unless sendMessage $ do
    guardResult <-
      lift $
        guardQualifiedLegalholdPolicyConflicts
          (qualifiedUserToProtectee localDomain senderType sender)
          (qmMissing mismatch)
    case guardResult of
      Left _ -> throwError FederatedGalley.MessageNotSentLegalhold
      Right () -> throwError $ FederatedGalley.MessageNotSentClientMissing otrResult

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
  UTCTime ->
  Qualified UserId ->
  ClientId ->
  Maybe ConnId ->
  ConvId ->
  Map UserId LocalMember ->
  MessageMetadata ->
  Map (Domain, UserId, ClientId) ByteString ->
  Galley QualifiedUserClients
sendMessages now sender senderClient mconn conv localMemberMap metadata messages = do
  localDomain <- viewFederationDomain
  let messageMap = byDomain messages
  let send dom
        | localDomain == dom =
          sendLocalMessages now sender senderClient mconn conv localMemberMap metadata
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
  UTCTime ->
  Qualified UserId ->
  ClientId ->
  Maybe ConnId ->
  ConvId ->
  Map UserId LocalMember ->
  MessageMetadata ->
  Map (UserId, ClientId) ByteString ->
  Galley (Set (UserId, ClientId))
sendLocalMessages now sender senderClient mconn conv localMemberMap metadata localMessages = do
  localDomain <- viewFederationDomain
  let events =
        localMessages & reindexed snd itraversed
          %@~ newMessageEvent
            (Qualified conv localDomain)
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
  Map (UserId, ClientId) ByteString ->
  Galley (Set (UserId, ClientId))
sendRemoteMessages domain now sender senderClient conv metadata messages = (>>= handle) . runExceptT $ do
  let rcpts =
        foldr
          (\((u, c), t) -> Map.insertWith (<>) u (Map.singleton c (toBase64Text t)))
          mempty
          (Map.assocs messages)
      rm =
        FederatedGalley.RemoteMessage
          { FederatedGalley.rmTime = now,
            FederatedGalley.rmData = Just (toBase64Text (mmData metadata)),
            FederatedGalley.rmSender = sender,
            FederatedGalley.rmSenderClient = senderClient,
            FederatedGalley.rmConversation = conv,
            FederatedGalley.rmPriority = mmNativePriority metadata,
            FederatedGalley.rmPush = mmNativePush metadata,
            FederatedGalley.rmTransient = mmTransient metadata,
            FederatedGalley.rmRecipients = UserClientMap rcpts
          }
  let rpc = FederatedGalley.receiveMessage FederatedGalley.clientRoutes rm
  executeFederated domain rpc
  where
    handle :: Either FederationError a -> Galley (Set (UserId, ClientId))
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

runMessagePush :: ConvId -> MessagePush -> Galley ()
runMessagePush cnv mp = do
  pushSome (userPushes mp)
  void . forkIO $ do
    gone <- External.deliver (botPushes mp)
    mapM_ (deleteBot cnv . botMemId) gone

newMessageEvent :: Qualified ConvId -> Qualified UserId -> ClientId -> ByteString -> UTCTime -> ClientId -> ByteString -> Event
newMessageEvent convId sender senderClient dat time recieverClient cipherText =
  Event OtrMessageAdd convId sender time . EdOtrMessage $
    OtrMessage
      { otrSender = senderClient,
        otrRecipient = recieverClient,
        otrCiphertext = toBase64Text cipherText,
        otrData = Just $ toBase64Text dat
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
        newConversationEventPush localDomain e [memId member]
          <&> set pushConn mconn
            . set pushNativePriority (mmNativePriority mm)
            . set pushRoute (bool RouteDirect RouteAny (mmNativePush mm))
            . set pushTransient (mmTransient mm)
            . set (pushRecipients . traverse . recipientClients) (RecipientClientsSome (singleton client))

data MessageMetadata = MessageMetadata
  { mmNativePush :: Bool,
    mmTransient :: Bool,
    mmNativePriority :: Maybe Priority,
    mmData :: ByteString
  }
  deriving (Eq, Ord, Show)

qualifiedNewOtrMetadata :: Public.QualifiedNewOtrMessage -> MessageMetadata
qualifiedNewOtrMetadata msg =
  MessageMetadata
    { mmNativePush = Public.qualifiedNewOtrNativePush msg,
      mmTransient = Public.qualifiedNewOtrTransient msg,
      mmNativePriority = Public.qualifiedNewOtrNativePriority msg,
      mmData = Public.qualifiedNewOtrData msg
    }

-- unqualified

legacyClientMismatchStrategy :: Domain -> Maybe [UserId] -> Maybe Public.IgnoreMissing -> Maybe Public.ReportMissing -> ClientMismatchStrategy
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

instance
  Unqualify a b =>
  Unqualify (WithStatus c a) (WithStatus c b)
  where
  unqualify domain (WithStatus x) = WithStatus (unqualify domain x)

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

instance Unqualify (Union PostOtrResponses) (Union PostOtrResponsesUnqualified) where
  unqualify domain = htrans (Proxy @Unqualify) $ I . unqualify domain . unI
