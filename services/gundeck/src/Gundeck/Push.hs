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

module Gundeck.Push
  ( push,
    addToken,
    listTokens,
    deleteToken,
    -- (for testing)
    pushAll,
    splitPush,
    MonadPushAll (..),
    MonadNativeTargets (..),
    MonadMapAsync (..),
  )
where

import Bilge qualified
import Bilge.RPC qualified as Bilge
import Control.Error
import Control.Lens (to, view, (.~), (^.))
import Control.Monad.Catch
import Control.Monad.Except (throwError)
import Data.Aeson qualified as Aeson
import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.List.Extra qualified as List
import Data.List1 (List1 (..), list1, toNonEmpty)
import Data.Map qualified as Map
import Data.Misc
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.These
import Data.Timeout
import Data.UUID qualified as UUID
import Gundeck.Aws (endpointUsers)
import Gundeck.Aws qualified as Aws
import Gundeck.Aws.Arn
import Gundeck.Env
import Gundeck.Monad
import Gundeck.Notification.Data qualified as Data
import Gundeck.Options
import Gundeck.Presence.Data qualified as Presence
import Gundeck.Push.Data qualified as Data
import Gundeck.Push.Native qualified as Native
import Gundeck.Push.Native.Types
import Gundeck.Push.Websocket qualified as Web
import Gundeck.ThreadBudget
import Gundeck.Util
import Imports
import Network.AMQP (Message (..))
import Network.AMQP qualified as Q
import Network.HTTP.Types
import Network.Wai.Utilities
import System.Logger.Class (msg, val, (+++), (.=), (~~))
import System.Logger.Class qualified as Log
import UnliftIO (pooledMapConcurrentlyN)
import Util.Options
import Wire.API.Internal.Notification
import Wire.API.Notification
import Wire.API.Presence (Presence (..))
import Wire.API.Push.Token qualified as Public
import Wire.API.Push.V2
import Wire.API.User (UserSet (..))
import Wire.API.User.Client (Client (..), UserClientsFull (..), supportsConsumableNotifications)

push :: [Push] -> Gundeck ()
push ps = do
  pushAll ps `catch` \(ex :: SomeException) -> do
    Log.err $ msg (val "Push failed") . Log.field "error" (displayException ex)
    throwM (mkError status500 "server-error" "Server Error")

-- | Abstract over all effects in 'pushAll' (for unit testing).
class (MonadThrow m) => MonadPushAll m where
  mpaNotificationTTL :: m NotificationTTL
  mpaCellsEventQueue :: m (Maybe Text)
  mpaMkNotificationId :: m NotificationId
  mpaListAllPresences :: [UserId] -> m [[Presence]]
  mpaBulkPush :: [(Notification, [Presence])] -> m [(NotificationId, [Presence])]
  mpaStreamAdd :: NotificationId -> List1 NotificationTarget -> List1 Aeson.Object -> NotificationTTL -> m ()
  mpaPushNative :: Notification -> Priority -> [Address] -> m ()
  mpaForkIO :: m () -> m ()
  mpaRunWithBudget :: Int -> a -> m a -> m a
  mpaGetClients :: Set UserId -> m UserClientsFull
  mpaPublishToRabbitMq :: Text -> Text -> Q.Message -> m ()

instance MonadPushAll Gundeck where
  mpaNotificationTTL = view (options . settings . notificationTTL)
  mpaCellsEventQueue = view (options . settings . cellsEventQueue)
  mpaMkNotificationId = mkNotificationId
  mpaListAllPresences = runWithDefaultRedis . Presence.listAll
  mpaBulkPush = Web.bulkPush
  mpaStreamAdd = Data.add
  mpaPushNative = pushNative
  mpaForkIO = void . forkIO
  mpaRunWithBudget = runWithBudget''
  mpaGetClients = getClients
  mpaPublishToRabbitMq = publishToRabbitMq

publishToRabbitMq :: Text -> Text -> Q.Message -> Gundeck ()
publishToRabbitMq exchangeName routingKey qMsg = do
  chan <- getRabbitMqChan
  void $ liftIO $ Q.publishMsg chan exchangeName routingKey qMsg

-- | Another layer of wrap around 'runWithBudget'.
runWithBudget'' :: Int -> a -> Gundeck a -> Gundeck a
runWithBudget'' budget fallback action = do
  view threadBudgetState >>= \case
    Nothing -> action
    Just tbs -> runWithBudget' tbs budget fallback action

-- | Abstract over all effects in 'nativeTargets' (for unit testing).
class (Monad m) => MonadNativeTargets m where
  mntgtLogErr :: SomeException -> m ()
  mntgtLookupAddresses :: UserId -> m [Address]

instance MonadNativeTargets Gundeck where
  mntgtLogErr e = Log.err (msg (val "Failed to get native push address: " +++ show e))
  mntgtLookupAddresses rcp = Data.lookup rcp Data.One

class (Monad m) => MonadMapAsync m where
  mntgtMapAsync :: (a -> m b) -> [a] -> m [Either SomeException b]
  mntgtPerPushConcurrency :: m (Maybe Int)

instance MonadMapAsync Gundeck where
  mntgtPerPushConcurrency = view (options . settings . perNativePushConcurrency)
  mntgtMapAsync f l = do
    perPushConcurrency <- mntgtPerPushConcurrency
    case perPushConcurrency of
      Nothing -> mapAsync f l
      Just chunkSize -> concat <$> mapM (mapAsync f) (List.chunksOf chunkSize l)

splitPushes :: (MonadPushAll m) => [Push] -> m ([Push], [Push], UserClientsFull)
splitPushes ps = do
  allUserClients <- mpaGetClients (Set.unions $ map (\p -> Set.map (._recipientId) $ p._pushRecipients) ps)
  let (rabbitmqPushes, legacyPushes) = partitionHereThere $ map (splitPush allUserClients) ps
  pure (rabbitmqPushes, legacyPushes, allUserClients)

-- | Split a push into rabbitmq and legacy push. This code exists to help with
-- migration. Once it is completed and old APIs are not supported anymore we can
-- assume everything is meant for RabbitMQ and stop splitting.
splitPush ::
  UserClientsFull ->
  Push ->
  -- | These rabbitmqPush cassandraPush
  These Push Push
splitPush clientsFull p = do
  let (rabbitmqRecipients, legacyRecipients) =
        partitionHereThere $
          map splitRecipient (toList p._pushRecipients)
  case (null rabbitmqRecipients, null legacyRecipients) of
    (True, _) -> (That p)
    (_, True) -> (This p)
    (False, False) ->
      These
        p {_pushRecipients = Set.fromList rabbitmqRecipients}
        p {_pushRecipients = Set.fromList legacyRecipients}
  where
    splitRecipient :: Recipient -> These Recipient Recipient
    splitRecipient rcpt = do
      let allClients = Map.findWithDefault mempty rcpt._recipientId $ clientsFull.userClientsFull
          relevantClients = case rcpt._recipientClients of
            RecipientClientsSome cs ->
              Set.filter (\c -> c.clientId `elem` toList cs) allClients
            RecipientClientsAll -> allClients
            RecipientClientsTemporaryOnly -> mempty
          (rabbitmqClients, legacyClients) = Set.partition supportsConsumableNotifications relevantClients
          rabbitmqClientIds = (.clientId) <$> Set.toList rabbitmqClients
          legacyClientIds = (.clientId) <$> Set.toList legacyClients
      case (rabbitmqClientIds, legacyClientIds) of
        ([], _) ->
          -- Checking for rabbitmqClientIds first ensures that we fall back to
          -- old behaviour even if legacyClientIds is empty too. This way we
          -- won't break things before clients are ready for it.
          --
          -- We return all clients for RabbitMQ even if there are no real
          -- clients so a temporary client can still read the notifications on
          -- RabbitMQ.
          That rcpt
        (_, []) ->
          This rcpt
        (r : rs, l : ls) ->
          let rabbitMqRecipients = case rcpt._recipientClients of
                RecipientClientsAll -> RecipientClientsAll
                RecipientClientsSome _ -> RecipientClientsSome $ list1 r rs
                RecipientClientsTemporaryOnly -> RecipientClientsTemporaryOnly
           in These
                rcpt {_recipientClients = rabbitMqRecipients}
                rcpt {_recipientClients = RecipientClientsSome $ list1 l ls}

getClients :: Set UserId -> Gundeck UserClientsFull
getClients uids = do
  fmap mconcat
    . pooledMapConcurrentlyN 4 getBatch
    . List.chunksOf 100
    $ Set.toList uids
  where
    getBatch :: [UserId] -> Gundeck UserClientsFull
    getBatch uidsChunk = do
      r <- do
        Endpoint h p <- view $ options . brig
        Bilge.rpc "brig" $
          Bilge.method POST
            . Bilge.host (toByteString' h)
            . Bilge.port p
            . Bilge.path "/i/clients/full"
            . Bilge.json (UserSet $ Set.fromList uidsChunk)
            . Bilge.expect2xx
      Bilge.responseJsonError r

pushAll :: (MonadPushAll m, MonadNativeTargets m, MonadMapAsync m, Log.MonadLogger m) => [Push] -> m ()
pushAll pushes = do
  Log.debug $ msg (val "pushing") . Log.field "pushes" (Aeson.encode pushes)
  (rabbitmqPushes, legacyPushes, allUserClients) <- splitPushes pushes

  legacyNotifs <- mapM mkNewNotification legacyPushes
  pushAllLegacy legacyNotifs allUserClients

  rabbitmqNotifs <- mapM mkNewNotification rabbitmqPushes
  pushAllViaRabbitMq rabbitmqNotifs allUserClients

  -- Note that Cells needs all notifications because it doesn't matter whether
  -- some recipients have rabbitmq clients or not.
  --
  -- We cannot simply merge 'legacyNotifs' and 'rabbitmqNotifs' because
  -- notifications could be duplicated in case cells notifications have users
  -- who have legacy and rabbitmq clients.
  allNotifs <- mapM mkNewNotification pushes
  pushAllToCells allNotifs

-- | Construct and send a single bulk push request to the client.  Write the 'Notification's from
-- the request to C*.  Trigger native pushes for all delivery failures notifications.
pushAllLegacy :: (MonadPushAll m, MonadNativeTargets m, MonadMapAsync m) => [NewNotification] -> UserClientsFull -> m ()
pushAllLegacy newNotifications userClientsFull = do
  -- persist push request
  let cassandraTargets :: [CassandraTargets]
      cassandraTargets = map mkCassandraTargets newNotifications
  forM_ cassandraTargets $ \CassandraTargets {..} ->
    unless (ntfTransient ctNotification) $
      case ctNotificationTargets of
        [] -> pure ()
        t : ts ->
          mpaStreamAdd (ntfId ctNotification) (list1 t ts) (ntfPayload ctNotification)
            =<< mpaNotificationTTL
  mpaForkIO $ do
    -- websockets
    wsTargets <- mapM mkWSTargets newNotifications
    resp <- compilePushResps wsTargets <$> mpaBulkPush (compilePushReq <$> wsTargets)
    -- native push
    forM_ resp $ \((notif :: Notification, psh :: Push), alreadySent :: [Presence]) -> do
      let alreadySentClients = Set.fromList $ mapMaybe (\p -> (p.userId,) <$> p.clientId) alreadySent
          rabbitmqClients = Map.map (Set.filter supportsConsumableNotifications) userClientsFull.userClientsFull
          rabbitmqClientIds = Map.foldMapWithKey (\uid clients -> Set.map (\c -> (uid, c.clientId)) clients) rabbitmqClients
      pushNativeWithBudget notif psh (Set.toList $ Set.union alreadySentClients rabbitmqClientIds)

pushNativeWithBudget :: (MonadMapAsync m, MonadPushAll m, MonadNativeTargets m) => Notification -> Push -> [(UserId, ClientId)] -> m ()
pushNativeWithBudget notif psh dontPush = do
  perPushConcurrency <- mntgtPerPushConcurrency
  let rcps' = nativeTargetsRecipients psh
      cost = maybe (length rcps') (min (length rcps')) perPushConcurrency
  -- this is a rough budget cost, since there may be more than one device in a
  -- 'Presence', so one budget token may trigger at most 8 push notifications
  -- to be sent out.
  -- If perPushConcurrency is defined, we take the min with 'perNativePushConcurrency', as native push requests
  -- to cassandra and SNS are limited to 'perNativePushConcurrency' in parallel.
  unless (psh ^. pushTransient) $
    mpaRunWithBudget cost () $
      mpaPushNative notif (psh ^. pushNativePriority) =<< nativeTargets psh rcps' dontPush

pushAllViaRabbitMq :: (MonadPushAll m, MonadMapAsync m, MonadNativeTargets m) => [NewNotification] -> UserClientsFull -> m ()
pushAllViaRabbitMq newNotifs userClientsFull = do
  for_ newNotifs $ pushViaRabbitMq
  mpaForkIO $ do
    for_ newNotifs $ \newNotif -> do
      let cassandraClients = Map.map (Set.filter $ not . supportsConsumableNotifications) userClientsFull.userClientsFull
          cassandraClientIds = Map.foldMapWithKey (\uid clients -> Set.map (\c -> (uid, c.clientId)) clients) cassandraClients
      pushNativeWithBudget newNotif.nnNotification newNotif.nnPush (Set.toList $ cassandraClientIds)

pushViaRabbitMq :: (MonadPushAll m) => NewNotification -> m ()
pushViaRabbitMq newNotif = do
  qMsg <- mkMessage newNotif.nnNotification
  let routingKeys =
        Set.unions $
          flip Set.map (Set.fromList . toList $ newNotif.nnRecipients) \r ->
            case r._recipientClients of
              RecipientClientsAll ->
                Set.singleton $ userRoutingKey r._recipientId
              RecipientClientsSome (toList -> cs) ->
                Set.fromList $ map (clientRoutingKey r._recipientId) cs
              RecipientClientsTemporaryOnly ->
                Set.singleton $ temporaryRoutingKey r._recipientId
  for_ routingKeys $ \routingKey ->
    mpaPublishToRabbitMq userNotificationExchangeName routingKey qMsg

pushAllToCells :: (MonadPushAll m, Log.MonadLogger m) => [NewNotification] -> m ()
pushAllToCells newNotifs = do
  mQueue <- mpaCellsEventQueue

  let cellsNotifs = filter (.nnPush._pushIsCellsEvent) newNotifs
  unless (null cellsNotifs) $ case mQueue of
    Nothing -> do
      Log.err $ msg ("Cells events have been generated, but no Cells queue is configured in gundeck" :: ByteString)
    Just queue ->
      traverse_ (pushToCells queue) cellsNotifs

pushToCells :: (MonadPushAll m) => Text -> NewNotification -> m ()
pushToCells queue newNotif = do
  qMsg <- mkMessage newNotif.nnNotification
  mpaPublishToRabbitMq "" queue qMsg

mkMessage :: (MonadPushAll m) => Notification -> m Message
mkMessage notif = do
  NotificationTTL ttl <- mpaNotificationTTL
  pure $
    Q.newMsg
      { msgBody =
          Aeson.encode
            . queuedNotification notif.ntfId
            $ toNonEmpty notif.ntfPayload,
        msgContentType = Just "application/json",
        msgDeliveryMode =
          -- Non-persistent messages never hit the disk and so do not
          -- survive RabbitMQ node restarts, this is great for transient
          -- notifications.
          Just
            ( if notif.ntfTransient
                then Q.NonPersistent
                else Q.Persistent
            ),
        msgExpiration =
          Just
            ( if notif.ntfTransient
                then
                  ( -- Means that if there is no active consumer, this
                    -- message will never be delivered to anyone. It can
                    -- still take some time before RabbitMQ forgets about
                    -- this message because the expiration is only
                    -- considered for messages which are at the head of a
                    -- queue. See docs: https://www.rabbitmq.com/docs/ttl
                    "0"
                  )
                else showT $ fromIntegral ttl # Second #> MilliSecond
            )
      }

-- | A new notification to be stored in C* and pushed over websockets
data NewNotification = NewNotification
  { nnPush :: Push,
    nnNotification :: Notification,
    nnRecipients :: [Recipient]
  }

mkNewNotification :: forall m. (MonadPushAll m) => Push -> m NewNotification
mkNewNotification psh = do
  let rcps = toList $ psh ^. pushRecipients
  notifId <- mpaMkNotificationId
  let notif = Notification notifId (psh ^. pushTransient) (psh ^. pushPayload)
  pure $ NewNotification psh notif rcps

-- | Information to be stored in cassandra for every 'Notification'.
data CassandraTargets = CassandraTargets
  { ctNotification :: Notification,
    ctNotificationTargets :: [NotificationTarget]
  }

mkCassandraTargets :: NewNotification -> CassandraTargets
mkCassandraTargets NewNotification {..} =
  CassandraTargets nnNotification (mapMaybe mkTarget $ toList nnRecipients)
  where
    mkTarget :: Recipient -> Maybe NotificationTarget
    mkTarget r = do
      clients <-
        case r ^. recipientClients of
          RecipientClientsAll -> Just []
          -- clients are stored in cassandra as a list with a notification.  empty list
          -- is interpreted as "all clients" by 'Gundeck.Notification.Data.toNotif'.
          RecipientClientsSome cs -> Just $ toList cs
          RecipientClientsTemporaryOnly -> Nothing
      pure $ target (r ^. recipientId) & targetClients .~ clients

-- | Information needed to push notifications over websockets and/or native
-- pushes.
data WSTargets = WSTargets
  { wstPush :: Push,
    wstNotification :: Notification,
    wstPresences :: [(Recipient, [Presence])]
  }

mkWSTargets :: (MonadPushAll m) => NewNotification -> m WSTargets
mkWSTargets NewNotification {..} = do
  withPresences <- addPresences nnRecipients
  pure $ WSTargets nnPush nnNotification withPresences
  where
    addPresences :: forall m. (MonadPushAll m) => [Recipient] -> m [(Recipient, [Presence])]
    addPresences rcps = do
      presences <- mpaListAllPresences $ fmap (view recipientId) rcps
      pure $ zip rcps presences

-- REFACTOR: @[Presence]@ here should be @newtype WebSockedDelivered = WebSockedDelivered [Presence]@
compilePushReq :: WSTargets -> (Notification, [Presence])
compilePushReq WSTargets {..} =
  (wstNotification, mconcat . fmap compileTargets $ wstPresences)
  where
    compileTargets :: (Recipient, [Presence]) -> [Presence]
    compileTargets (rcp, pre) = filter (shouldActuallyPush wstPush rcp) pre

compilePushResps ::
  [WSTargets] ->
  [(NotificationId, [Presence])] ->
  [((Notification, Push), [Presence])]
compilePushResps notifIdMap (Map.fromList -> deliveries) =
  notifIdMap
    <&> (\WSTargets {..} -> ((wstNotification, wstPush), fromMaybe [] (Map.lookup (ntfId wstNotification) deliveries)))

-- | Is 'PushTarget' the origin of the 'Push', or is missing in a non-empty whitelist?  (Whitelists
-- reside both in 'Push' itself and in each 'Recipient').
shouldActuallyPush :: Push -> Recipient -> Presence -> Bool
shouldActuallyPush psh rcp pres = not isOrigin && okByPushAllowlist && okByRecipientAllowlist
  where
    isOrigin =
      psh ^. pushOrigin == Just (userId pres)
        && psh ^. pushOriginConnection == Just (connId pres)

    okByPushAllowlist :: Bool
    okByPushAllowlist = not allowlistExists || isAllowlisted
      where
        allowlist = psh ^. pushConnections
        allowlistExists = not $ Set.null allowlist
        isAllowlisted = connId pres `Set.member` allowlist

    okByRecipientAllowlist :: Bool
    okByRecipientAllowlist =
      case (rcp ^. recipientClients, pres.clientId) of
        (RecipientClientsSome cs, Just c) -> c `elem` cs
        _ -> True

-- | Failures to push natively can be ignored.  Logging already happens in
-- 'Gundeck.Push.Native.push1', and we cannot recover from any of the error cases.
pushNative :: Notification -> Priority -> [Address] -> Gundeck ()
pushNative _ _ [] = pure ()
pushNative notif prio rcps = do
  Native.push (Native.NativePush (ntfId notif) prio Nothing) rcps

-- | Compute list of 'Recipient's from a 'Push' that may be interested in a native push.  More
-- filtering in 'nativeTargets'.
nativeTargetsRecipients :: Push -> [Recipient]
nativeTargetsRecipients psh = filter routeNative (toList (psh ^. pushRecipients))
  where
    routeNative u =
      u ^. recipientRoute /= RouteDirect
        && (Just (u ^. recipientId) /= psh ^. pushOrigin || psh ^. pushNativeIncludeOrigin)

-- | FUTUREWORK: 'nativeTargets' calls cassandra once for each 'Recipient' of the 'Push'.  Instead,
-- it should be called once with @[Push]@ for every call to 'pushAll', and that call should
-- only call cassandra once in total, yielding all addresses of all recipients of all pushes.
nativeTargets ::
  forall m.
  (MonadNativeTargets m, MonadMapAsync m) =>
  Push ->
  [Recipient] ->
  [(UserId, ClientId)] ->
  m [Address]
nativeTargets psh rcps' dontPush =
  mntgtMapAsync addresses rcps' >>= fmap concat . mapM check
  where
    addresses :: Recipient -> m [Address]
    addresses u = do
      addrs <- mntgtLookupAddresses (u ^. recipientId)
      pure $ filter (eligible u) addrs
    eligible :: Recipient -> Address -> Bool
    eligible u a
      -- Never include the origin client.
      | Just (a ^. addrUser) == psh ^. pushOrigin && Just (a ^. addrConn) == psh ^. pushOriginConnection = False
      -- Is the specific client an intended recipient?
      | not (eligibleClient a (u ^. recipientClients)) = False
      -- Is the client not whitelisted?
      | not (whitelistedOrNoWhitelist a) = False
      -- Include client if not found in already served presences.
      | otherwise = not $ List.elem (a ^. addrUser, a ^. addrClient) dontPush --  (List.find (isOnline a) alreadySent)
      -- isOnline a x =
      --   a ^. addrUser == Presence.userId x
      --     && (a ^. addrConn == Presence.connId x || equalClient a x)
      -- equalClient a x = Just (a ^. addrClient) == Presence.clientId x
    eligibleClient _ RecipientClientsAll = True
    eligibleClient a (RecipientClientsSome cs) = (a ^. addrClient) `elem` cs
    eligibleClient _ RecipientClientsTemporaryOnly = False
    whitelistedOrNoWhitelist a =
      null (psh ^. pushConnections)
        || a ^. addrConn `elem` psh ^. pushConnections

    check :: Either SomeException [a] -> m [a]
    check (Left e) = mntgtLogErr e >> pure []
    check (Right r) = pure r

type AddTokenResponse = Either Public.AddTokenError Public.AddTokenSuccess

addToken :: UserId -> ConnId -> PushToken -> Gundeck AddTokenResponse
addToken uid cid newtok = mpaRunWithBudget 1 (Left Public.AddTokenErrorNoBudget) $ runExceptT $ do
  when (newtok ^. tokenTransport `elem` [APNSVoIP, APNSVoIPSandbox]) $
    throwError Public.AddTokenErrorApnsVoipNotSupported

  (cur, old) <- lift $ foldl' (matching newtok) (Nothing, []) <$> Data.lookup uid Data.LocalQuorum
  lift $
    Log.info $
      "user"
        .= UUID.toASCIIBytes (toUUID uid)
        ~~ "token"
        .= Text.take 16 (tokenText (newtok ^. token))
        ~~ msg (val "Registering push token")
  addr <- continue newtok cur
  lift $ Native.deleteTokens old (Just addr)
  pure $ Public.AddTokenSuccess newtok
  where
    matching ::
      PushToken ->
      (Maybe Address, [Address]) ->
      Address ->
      (Maybe Address, [Address])
    matching t (x, old) a
      | a ^. addrTransport == t ^. tokenTransport
          && a ^. addrApp == t ^. tokenApp
          && a ^. addrClient == t ^. tokenClient =
          if a ^. addrToken == t ^. token
            then (Just a, old)
            else (x, a : old)
      | otherwise = (x, old)

    continue ::
      PushToken ->
      Maybe Address ->
      ExceptT Public.AddTokenError Gundeck Address
    continue t Nothing = create (0 :: Int) t
    continue t (Just a) = update (0 :: Int) t (a ^. addrEndpoint)

    create ::
      Int ->
      PushToken ->
      ExceptT Public.AddTokenError Gundeck Address
    create n t = do
      let trp = t ^. tokenTransport
      let app = t ^. tokenApp
      let tok = t ^. token
      env <- view (options . aws . arnEnv)
      aws' <- view awsEnv
      ept <- Aws.execute aws' (Aws.createEndpoint uid trp env app tok)
      case ept of
        Left (Aws.EndpointInUse arn) -> do
          lift $ Log.info $ "arn" .= toText arn ~~ msg (val "ARN in use")
          update (n + 1) t arn
        Left (Aws.AppNotFound app') -> do
          lift $ Log.info $ msg ("Push token of unknown application: '" <> appNameText app' <> "'")
          throwError Public.AddTokenErrorNotFound
        Left (Aws.InvalidToken _) -> do
          lift $
            Log.info $
              "token"
                .= tokenText tok
                ~~ msg (val "Invalid push token.")
          throwError Public.AddTokenErrorInvalid
        Left (Aws.TokenTooLong l) -> do
          lift $ Log.info $ msg ("Push token is too long: token length = " ++ show l)
          throwError Public.AddTokenErrorTooLong
        Right arn -> do
          Data.insert uid trp app tok arn cid (t ^. tokenClient)
          pure $ mkAddr t arn

    update ::
      Int ->
      PushToken ->
      SnsArn EndpointTopic ->
      ExceptT Public.AddTokenError Gundeck Address
    update n t arn = do
      when (n >= 3) $ do
        lift $ Log.err $ msg (val "AWS SNS inconsistency w.r.t. " +++ toText arn)
        throwM (mkError status500 "server-error" "Server Error")
      aws' <- view awsEnv
      ept <- Aws.execute aws' (Aws.lookupEndpoint arn)
      case ept of
        Nothing -> create (n + 1) t
        Just ep ->
          do
            lift $ updateEndpoint uid t arn ep
            Data.insert
              uid
              (t ^. tokenTransport)
              (t ^. tokenApp)
              (t ^. token)
              arn
              cid
              (t ^. tokenClient)
            pure $ mkAddr t arn
            `catch` \case
              -- Note: If the endpoint was recently deleted (not necessarily
              -- concurrently), we may get an EndpointNotFound error despite
              -- the previous lookup, i.e. endpoint lookups may exhibit eventually
              -- consistent semantics with regards to endpoint deletion (or
              -- possibly updates in general). We make another attempt to (re-)create
              -- the endpoint in these cases instead of failing immediately.
              Aws.EndpointNotFound {} -> create (n + 1) t
              Aws.InvalidCustomData {} -> throwError Public.AddTokenErrorMetadataTooLong
              ex -> throwM ex

    mkAddr ::
      PushToken ->
      EndpointArn ->
      Address
    mkAddr t arn =
      Address
        uid
        arn
        cid
        (pushToken (t ^. tokenTransport) (t ^. tokenApp) (t ^. token) (t ^. tokenClient))

-- | Update an SNS endpoint with the given user and token.
updateEndpoint :: UserId -> PushToken -> EndpointArn -> Aws.SNSEndpoint -> Gundeck ()
updateEndpoint uid t arn e = do
  env <- view awsEnv
  requestId <- view reqId

  unless (equalTransport && equalApp) $ do
    Log.err $ logMessage requestId "PushToken does not fit to user_push data: Transport or app mismatch"
    throwM $ mkError status500 "server-error" "Server Error"

  Log.info $ logMessage requestId "Upserting push token."
  let users = Set.insert uid (e ^. endpointUsers)
  Aws.execute env $ Aws.updateEndpoint users (t ^. token) arn
  where
    equalTransport = t ^. tokenTransport == arn ^. snsTopic . endpointTransport
    equalApp = t ^. tokenApp == arn ^. snsTopic . endpointAppName
    logMessage requestId m =
      "user"
        .= UUID.toASCIIBytes (toUUID uid)
        ~~ "token"
        .= Text.take 16 (t ^. token . to tokenText)
        ~~ "tokenTransport"
        .= show (t ^. tokenTransport)
        ~~ "tokenApp"
        .= (t ^. tokenApp . to appNameText)
        ~~ "arn"
        .= toText arn
        ~~ "endpointTransport"
        .= show (arn ^. snsTopic . endpointTransport)
        ~~ "endpointAppName"
        .= (arn ^. snsTopic . endpointAppName . to appNameText)
        ~~ "request"
        .= unRequestId requestId
        ~~ msg (val m)

deleteToken :: UserId -> Token -> Gundeck (Maybe ())
deleteToken uid tok = do
  Data.lookup uid Data.LocalQuorum
    >>= ( \case
            [] -> pure Nothing
            xs -> Native.deleteTokens xs Nothing $> Just ()
        )
      . filter (\x -> x ^. addrToken == tok)

listTokens :: UserId -> Gundeck PushTokenList
listTokens uid = PushTokenList . map (^. addrPushToken) <$> Data.lookup uid Data.LocalQuorum
