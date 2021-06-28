module Galley.API.Message where

import Control.Lens
import Data.Domain (Domain)
import Data.Id (ClientId, ConnId, ConvId, UserId)
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import Data.List1 (singleton)
import qualified Data.Map as Map
import Data.Qualified (Qualified (..))
import qualified Data.Set as Set
import Data.Set.Lens
import Data.Time.Clock (UTCTime, getCurrentTime)
import Galley.API.LegalHold (guardQualifiedLegalholdPolicyConflicts)
import Galley.API.Util (runUnionT, throwUnion, toBase64Text, viewFederationDomain)
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
import Servant.API (Union, WithStatus (..))
import Wire.API.ErrorDescription as ErrorDescription
import Wire.API.Event.Conversation
import Wire.API.Message
import qualified Wire.API.Message as Public
import Wire.API.Routes.Public.Galley as Public
import Wire.API.Team.LegalHold
import Wire.API.Team.Member (ListType (..))
import Wire.API.User.Client

data UserType = User | Bot

userToProtectee :: UserType -> UserId -> LegalholdProtectee
userToProtectee User user = ProtectedUser user
userToProtectee Bot _ = UnprotectedBot

data QualifiedMismatch = QualifiedMismatch
  { qmMissing :: QualifiedUserClients,
    qmRedundant :: QualifiedUserClients,
    qmDeleted :: QualifiedUserClients
  }

mkQualifiedMismatch ::
  QualifiedRecipientSet -> QualifiedRecipientSet -> QualifiedRecipientSet -> QualifiedMismatch
mkQualifiedMismatch missing redundant deleted =
  QualifiedMismatch
    (QualifiedUserClients missing)
    (QualifiedUserClients redundant)
    (QualifiedUserClients deleted)

mkMessageSendingStatus :: UTCTimeMillis -> QualifiedMismatch -> QualifiedUserClients -> MessageSendingStatus
mkMessageSendingStatus time mismatch failures =
  MessageSendingStatus
    { mssTime = time,
      mssMissingClients = qmMissing mismatch,
      mssRedundantClients = qmRedundant mismatch,
      mssDeletedClients = qmDeleted mismatch,
      mssFailedToSend = failures
    }

type QualifiedRecipient = (Domain, (UserId, ClientId))

qualifiedRecipientToUser :: QualifiedRecipient -> Qualified UserId
qualifiedRecipientToUser (dom, (uid, _)) = Qualified uid dom

type RecipientMap a = Map UserId (Map ClientId a)

type RecipientSet = Map UserId (Set ClientId)

type QualifiedRecipientMap a = Map Domain (RecipientMap a)

type QualifiedRecipientSet = Map Domain RecipientSet

isUserPresent :: QualifiedRecipientSet -> Domain -> UserId -> Bool
isUserPresent qrs d u =
  let memberUsers = Map.keysSet $ Map.findWithDefault mempty d qrs
   in Set.member u memberUsers

qualifiedRecipientMapToSet :: QualifiedRecipientMap a -> QualifiedRecipientSet
qualifiedRecipientMapToSet = Map.map (Map.map Map.keysSet)

qualifiedRecipientSetSingleton :: Domain -> UserId -> ClientId -> QualifiedRecipientSet
qualifiedRecipientSetSingleton dom uid cid =
  Map.singleton dom
    . Map.singleton uid
    . Set.singleton
    $ cid

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
--                |  Clients        | Clients    +------------Clients------+
--                |                 |            |    |                    |
--                |                 |            |    |                    |
--                |                 |            | Redundant Clients       |
--                |                 |            |    |                    |
--                |                 |            |    |                    |
--                |                 |            |    |                    |
--                |                 +------------+-------------------------+
--                |                                   |
--                +-----------------------------------+
checkMessageClients ::
  -- | Sender domain
  Domain ->
  -- | Sender User
  UserId ->
  QualifiedUserClients ->
  QualifiedNewOtrMessage ->
  (Maybe (QualifiedRecipientMap ByteString), QualifiedMismatch)
checkMessageClients senderDomain senderUser (QualifiedUserClients expected) msg =
  let senderClient = qualifiedNewOtrSender msg
      -- Recipients provided in the message.
      recipientMap =
        qualifiedUserClientMap
          . qualifiedOtrRecipientsMap
          . qualifiedNewOtrRecipients
          $ msg
      recipients = qualifiedRecipientMapToSet recipientMap
      -- Whoever is expected but not in recipients is missing.
      missing = qualifiedDiff expected recipients
      -- Whoever is in recipient but not expected is extra.
      extra = qualifiedDiff recipients expected
      -- The clients which belong to users who are expected are considered
      -- deleted.
      --
      -- FUTUREWORK: Optimize this by partitioning extra, this way redundants
      -- wouldn't need a qualifiedDiff.
      deleted = nestedKeyFilter (isUserPresent expected) extra
      -- If sender includes a message for themself, it is considered redundant
      redundantSender
        | isUserPresent expected senderDomain senderUser =
          qualifiedRecipientSetSingleton senderDomain senderUser senderClient
        | otherwise = mempty
      -- The clients which are extra but not deleted, must belong to users which
      -- are not in the convesation and hence considered redundant.
      redundant = qualifiedDiff extra deleted <> redundantSender
      -- The clients which are recipients but not extra or the sender client are
      -- considered valid.
      valid = qualifiedDiff recipients (extra <> redundantSender)
      validMap = selectClients valid recipientMap
      -- Resolve whether the message is valid using client mismatch strategy
      usersWithMissingClients = extractUsers missing
      isValidMessage = case qualifiedNewOtrClientMismatchStrategy msg of
        MismatchIgnoreAll -> True
        MismatchReportAll -> Map.null missing
        MismatchIgnoreOnly ignoredUsers ->
          let nonIgnoredUsersMissingClients = usersWithMissingClients `Set.difference` ignoredUsers
           in Set.null nonIgnoredUsersMissingClients
        MismatchReportOnly strictUsers ->
          let strictUsersMisingClients = strictUsers `Set.difference` usersWithMissingClients
           in Set.null strictUsersMisingClients
   in ( guard isValidMessage $> validMap,
        mkQualifiedMismatch missing redundant deleted
      )
  where
    nestedKeyFilter :: (a -> b -> Bool) -> Map a (Map b c) -> Map a (Map b c)
    nestedKeyFilter predicate =
      Map.filterWithKey
        ( \a ->
            not . Map.null
              . Map.filterWithKey (\b _ -> predicate a b)
        )

    extractUsers :: QualifiedRecipientSet -> Set (Qualified UserId)
    extractUsers =
      setOf $
        reindexed (uncurry (flip Qualified)) (itraversed <.> itraversed)
          . asIndex

    -- TODO: I am pretty sure this was different
    selectClients :: QualifiedRecipientSet -> QualifiedRecipientMap a -> QualifiedRecipientMap a
    selectClients = Map.intersectionWith (Map.intersectionWith (flip Map.restrictKeys))

qualifiedDiff :: QualifiedRecipientSet -> QualifiedRecipientSet -> QualifiedRecipientSet
qualifiedDiff =
  Map.differenceWith
    . guardedOp (not . Map.null)
    . Map.differenceWith
    . guardedOp (not . Set.null)
    $ Set.difference
  where
    guarded :: (a -> Bool) -> a -> Maybe a
    guarded p x = guard (p x) $> x

    guardedOp :: (c -> Bool) -> (a -> b -> c) -> (a -> b -> Maybe c)
    guardedOp p f a b = guarded p (f a b)

postQualifiedOtrMessage :: UserType -> UserId -> Maybe ConnId -> ConvId -> Public.QualifiedNewOtrMessage -> Galley (Union Public.PostOtrResponses)
postQualifiedOtrMessage senderType sender mconn convId msg = runUnionT $ do
  alive <- Data.isConvAlive convId
  localDomain <- viewFederationDomain
  now <- liftIO $ getCurrentTime
  let nowMillis = toUTCTimeMillis now
  -- TODO: Test this, fix this, make it a parameters
  senderDomain <- viewFederationDomain
  unless alive $ do
    Data.deleteConversation convId
    throwUnion ErrorDescription.convNotFound

  localMembers <- Data.members convId
  let localMemberIds = memId <$> localMembers
      localMemberMap :: Map UserId LocalMember
      localMemberMap = Map.fromList (map (\mem -> (memId mem, mem)) localMembers)
  isInternal <- view $ options . optSettings . setIntraListing
  localClients <-
    lift $
      if isInternal
        then Clients.fromUserClients <$> Intra.lookupClients localMemberIds
        else Data.lookupClients localMemberIds
  let qualifiedLocalClients = Map.singleton localDomain $ Clients.toMap localClients
  let (mValidMessages, mismatch) =
        checkMessageClients
          senderDomain
          sender
          (QualifiedUserClients qualifiedLocalClients)
          msg
      otrResult = mkMessageSendingStatus nowMillis mismatch mempty

  validMessages <- case mValidMessages of
    Nothing -> do
      lift $ guardQualifiedLegalholdPolicyConflicts (userToProtectee senderType sender) (qmMissing mismatch)
      throwUnion $ WithStatus @412 otrResult
    Just v -> pure v
  let qualifiedConv = Qualified convId localDomain
      qualifiedSender = Qualified sender senderDomain
      localValidMessages = Map.findWithDefault mempty localDomain validMessages
      metadata = qualifiedNewOtrMetadata msg
      events =
        localValidMessages & traverse . itraversed
          %@~ newMessageEvent
            qualifiedConv
            qualifiedSender
            (Public.qualifiedNewOtrSender msg)
            (Public.qualifiedNewOtrData msg)
            now
      pushes =
        events & itraversed <.> itraversed
          %@~ newMessagePush localMemberMap mconn metadata
  lift $ runMessagePush convId (pushes ^. traversed . traversed)
  throwUnion $ WithStatus @201 otrResult

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
  Map k LocalMember ->
  Maybe ConnId ->
  MessageMetadata ->
  (k, ClientId) ->
  Event ->
  MessagePush
newMessagePush members mconn mm (k, client) e = fromMaybe mempty $ do
  member <- Map.lookup k members
  newBotMessagePush member <|> newUserMessagePush member
  where
    newBotMessagePush :: LocalMember -> Maybe MessagePush
    newBotMessagePush member = newBotPush <$> newBotMember member <*> pure e
    newUserMessagePush :: LocalMember -> Maybe MessagePush
    newUserMessagePush member =
      fmap newUserPush $
        newPush
          ListComplete
          (qUnqualified (evtFrom e))
          (ConvEvent e)
          [ recipient member & recipientClients .~ RecipientClientsSome (singleton client)
          ]
          <&> set pushConn mconn
            . set pushNativePriority (mmNativePriority mm)
            . set pushRoute (bool RouteDirect RouteAny (mmNativePush mm))
            . set pushTransient (mmTransient mm)

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
