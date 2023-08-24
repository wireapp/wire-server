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
    pushAny,
    MonadPushAll (..),
    MonadNativeTargets (..),
    MonadMapAsync (..),
    MonadPushAny (..),
  )
where

import Control.Arrow ((&&&))
import Control.Error
import Control.Exception (ErrorCall (ErrorCall))
import Control.Lens (view, (.~), (^.))
import Control.Monad.Catch
import Data.Aeson as Aeson (Object)
import Data.Id
import Data.List.Extra qualified as List
import Data.List1 (List1, list1)
import Data.Map qualified as Map
import Data.Range
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
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
import Gundeck.Types
import Gundeck.Types.Presence qualified as Presence
import Gundeck.Util
import Imports hiding (cs)
import Network.HTTP.Types
import Network.Wai.Utilities
import System.Logger.Class (msg, val, (+++), (.=), (~~))
import System.Logger.Class qualified as Log
import Wire.API.Internal.Notification
import Wire.API.Push.Token qualified as Public

push :: [Push] -> Gundeck ()
push ps = do
  bulk :: Bool <- view (options . settings . bulkPush)
  rs <-
    if bulk
      then (Right <$> pushAll ps) `catch` (pure . Left . Seq.singleton)
      else pushAny ps
  case rs of
    Right () -> pure ()
    Left exs -> do
      forM_ exs $ Log.err . msg . (val "Push failed: " +++) . show
      throwM (mkError status500 "server-error" "Server Error")

-- | Abstract over all effects in 'pushAll' (for unit testing).
class MonadThrow m => MonadPushAll m where
  mpaNotificationTTL :: m NotificationTTL
  mpaMkNotificationId :: m NotificationId
  mpaListAllPresences :: [UserId] -> m [[Presence]]
  mpaBulkPush :: [(Notification, [Presence])] -> m [(NotificationId, [Presence])]
  mpaStreamAdd :: NotificationId -> List1 NotificationTarget -> List1 Aeson.Object -> NotificationTTL -> m ()
  mpaPushNative :: Notification -> Priority -> [Address] -> m ()
  mpaForkIO :: m () -> m ()
  mpaRunWithBudget :: Int -> a -> m a -> m a

instance MonadPushAll Gundeck where
  mpaNotificationTTL = view (options . settings . notificationTTL)
  mpaMkNotificationId = mkNotificationId
  mpaListAllPresences = runWithDefaultRedis . Presence.listAll
  mpaBulkPush = Web.bulkPush
  mpaStreamAdd = Data.add
  mpaPushNative = pushNative
  mpaForkIO = void . forkIO
  mpaRunWithBudget = runWithBudget''

-- | Another layer of wrap around 'runWithBudget'.
runWithBudget'' :: Int -> a -> Gundeck a -> Gundeck a
runWithBudget'' budget fallback action = do
  metrics <- view monitor
  view threadBudgetState >>= \case
    Nothing -> action
    Just tbs -> runWithBudget' metrics tbs budget fallback action

-- | Abstract over all effects in 'nativeTargets' (for unit testing).
class Monad m => MonadNativeTargets m where
  mntgtLogErr :: SomeException -> m ()
  mntgtLookupAddresses :: UserId -> m [Address]

instance MonadNativeTargets Gundeck where
  mntgtLogErr e = Log.err (msg (val "Failed to get native push address: " +++ show e))
  mntgtLookupAddresses rcp = Data.lookup rcp Data.One

class Monad m => MonadMapAsync m where
  mntgtMapAsync :: (a -> m b) -> [a] -> m [Either SomeException b]
  mntgtPerPushConcurrency :: m (Maybe Int)

instance MonadMapAsync Gundeck where
  mntgtPerPushConcurrency = view (options . settings . perNativePushConcurrency)
  mntgtMapAsync f l = do
    perPushConcurrency <- mntgtPerPushConcurrency
    case perPushConcurrency of
      Nothing -> mapAsync f l
      Just chunkSize -> concat <$> mapM (mapAsync f) (List.chunksOf chunkSize l)

-- | Abstract over all effects in 'pushAny' (for unit testing).
class (MonadPushAll m, MonadNativeTargets m, MonadMapAsync m) => MonadPushAny m where
  mpyPush ::
    Notification ->
    List1 NotificationTarget ->
    Maybe UserId ->
    Maybe ConnId ->
    Set ConnId ->
    m [Presence]

instance MonadPushAny Gundeck where
  mpyPush = Web.push

-- | Send individual HTTP requests to cannon for every device and notification.
--
-- REFACTOR: This should go away in the future, once 'pushAll' has been proven to always do the same
-- thing.  also check what types this removal would make unnecessary.
pushAny ::
  forall m.
  (MonadPushAny m) =>
  [Push] ->
  m (Either (Seq.Seq SomeException) ())
pushAny ps = collectErrors <$> mntgtMapAsync pushAny' ps
  where
    collectErrors :: [Either SomeException ()] -> Either (Seq.Seq SomeException) ()
    collectErrors = runAllE . foldMap (AllE . fmapL Seq.singleton)

pushAny' ::
  forall m.
  (MonadPushAny m) =>
  Push ->
  m ()
pushAny' p = do
  i <- mpaMkNotificationId
  let pload = p ^. pushPayload
  let notif = Notification i (p ^. pushTransient) pload
  let rcps = fromRange (p ^. pushRecipients)
  let uniq = uncurry list1 $ head &&& tail $ toList rcps
  let tgts = mkTarget <$> uniq
  unless (p ^. pushTransient) $
    mpaStreamAdd i tgts pload =<< mpaNotificationTTL
  mpaForkIO $ do
    alreadySent <- mpyPush notif tgts (p ^. pushOrigin) (p ^. pushOriginConnection) (p ^. pushConnections)
    unless (p ^. pushTransient) $
      mpaPushNative notif (p ^. pushNativePriority) =<< nativeTargets p (nativeTargetsRecipients p) alreadySent
  where
    mkTarget :: Recipient -> NotificationTarget
    mkTarget r =
      target (r ^. recipientId)
        & targetClients .~ case r ^. recipientClients of
          RecipientClientsAll -> []
          RecipientClientsSome cs -> toList cs

-- | Construct and send a single bulk push request to the client.  Write the 'Notification's from
-- the request to C*.  Trigger native pushes for all delivery failures notifications.
pushAll :: (MonadPushAll m, MonadNativeTargets m, MonadMapAsync m) => [Push] -> m ()
pushAll pushes = do
  newNotifications <- mapM mkNewNotification pushes
  -- persist push request
  let cassandraTargets :: [CassandraTargets]
      cassandraTargets = map mkCassandraTargets newNotifications
  forM_ cassandraTargets $ \CassandraTargets {..} ->
    unless (ntfTransient ctNotification) $
      mpaStreamAdd (ntfId ctNotification) ctNotificationTargets (ntfPayload ctNotification)
        =<< mpaNotificationTTL
  mpaForkIO $ do
    -- websockets
    wsTargets <- mapM mkWSTargets newNotifications
    resp <- compilePushResps wsTargets <$> mpaBulkPush (compilePushReq <$> wsTargets)
    -- native push
    perPushConcurrency <- mntgtPerPushConcurrency
    forM_ resp $ \((notif :: Notification, psh :: Push), alreadySent :: [Presence]) -> do
      let rcps' = nativeTargetsRecipients psh
          cost = maybe (length rcps') (min (length rcps')) perPushConcurrency
      -- this is a rough budget cost, since there may be more than one device in a
      -- 'Presence', so one budget token may trigger at most 8 push notifications
      -- to be sent out.
      -- If perPushConcurrency is defined, we take the min with 'perNativePushConcurrency', as native push requests
      -- to cassandra and SNS are limited to 'perNativePushConcurrency' in parallel.
      unless (psh ^. pushTransient) $
        mpaRunWithBudget cost () $
          mpaPushNative notif (psh ^. pushNativePriority) =<< nativeTargets psh rcps' alreadySent

-- | A new notification to be stored in C* and pushed over websockets
data NewNotification = NewNotification
  { nnPush :: Push,
    nnNotification :: Notification,
    nnRecipients :: List1 Recipient
  }

mkNewNotification :: forall m. MonadPushAll m => Push -> m NewNotification
mkNewNotification psh = NewNotification psh <$> mkNotif <*> rcps
  where
    mkNotif :: m Notification
    mkNotif = do
      notifId <- mpaMkNotificationId
      pure $ Notification notifId (psh ^. pushTransient) (psh ^. pushPayload)

    rcps :: m (List1 Recipient)
    rcps = assertList1 . toList . fromRange $ (psh ^. pushRecipients :: Range 1 1024 (Set Recipient))

    -- Shouldn't fail as we just extracted this from `Range 1 1024`
    assertList1 :: [a] -> m (List1 a)
    assertList1 (x : xs) = pure $ list1 x xs
    assertList1 _ = throwM $ ErrorCall "mkNotification: internal error." -- can @listAll@ return @[]@?

-- | Information to be stored in cassandra for every 'Notification'.
data CassandraTargets = CassandraTargets
  { ctNotification :: Notification,
    ctNotificationTargets :: List1 NotificationTarget
  }

mkCassandraTargets :: NewNotification -> CassandraTargets
mkCassandraTargets NewNotification {..} =
  CassandraTargets nnNotification (mkTarget <$> nnRecipients)
  where
    mkTarget :: Recipient -> NotificationTarget
    mkTarget r =
      target (r ^. recipientId)
        & targetClients .~ case r ^. recipientClients of
          RecipientClientsAll -> []
          -- clients are stored in cassandra as a list with a notification.  empty list
          -- is interpreted as "all clients" by 'Gundeck.Notification.Data.toNotif'.
          RecipientClientsSome cs -> toList cs

-- | Information needed to push notifications over websockets and/or native
-- pushes.
data WSTargets = WSTargets
  { wstPush :: Push,
    wstNotification :: Notification,
    wstPresences :: List1 (Recipient, [Presence])
  }

mkWSTargets :: MonadPushAll m => NewNotification -> m WSTargets
mkWSTargets NewNotification {..} = do
  withPresences <- addPresences nnRecipients
  pure $ WSTargets nnPush nnNotification withPresences
  where
    addPresences :: forall m. MonadPushAll m => List1 Recipient -> m (List1 (Recipient, [Presence]))
    addPresences (toList -> rcps) = do
      presences <- mpaListAllPresences $ fmap (view recipientId) rcps
      zip1 rcps presences
      where
        zip1 :: [a] -> [b] -> m (List1 (a, b))
        zip1 (x : xs) (y : ys) = pure $ list1 (x, y) (zip xs ys)
        zip1 _ _ = throwM $ ErrorCall "mkNotificationAndTargets: internal error." -- can @listAll@ return @[]@?

-- REFACTOR: @[Presence]@ here should be @newtype WebSockedDelivered = WebSockedDelivered [Presence]@
compilePushReq :: WSTargets -> (Notification, [Presence])
compilePushReq WSTargets {..} =
  (wstNotification, mconcat . fmap compileTargets . toList $ wstPresences)
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
      case (rcp ^. recipientClients, clientId pres) of
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
nativeTargetsRecipients psh = filter routeNative (toList (fromRange (psh ^. pushRecipients)))
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
  [Presence] ->
  m [Address]
nativeTargets psh rcps' alreadySent =
  mntgtMapAsync addresses rcps' >>= fmap concat . mapM check
  where
    addresses :: Recipient -> m [Address]
    addresses u = do
      addrs <- mntgtLookupAddresses (u ^. recipientId)
      pure
        $ preference
          . filter (eligible u)
        $ addrs
    eligible :: Recipient -> Address -> Bool
    eligible u a
      -- Never include the origin client.
      | Just (a ^. addrUser) == psh ^. pushOrigin && Just (a ^. addrConn) == psh ^. pushOriginConnection = False
      -- Is the specific client an intended recipient?
      | not (eligibleClient a (u ^. recipientClients)) = False
      -- Is the client not whitelisted?
      | not (whitelistedOrNoWhitelist a) = False
      -- Include client if not found in already served presences.
      | otherwise = isNothing (List.find (isOnline a) alreadySent)
    isOnline a x =
      a ^. addrUser == Presence.userId x
        && (a ^. addrConn == Presence.connId x || equalClient a x)
    equalClient a x = Just (a ^. addrClient) == Presence.clientId x
    eligibleClient _ RecipientClientsAll = True
    eligibleClient a (RecipientClientsSome cs) = (a ^. addrClient) `elem` cs
    whitelistedOrNoWhitelist a =
      null (psh ^. pushConnections)
        || a ^. addrConn `elem` psh ^. pushConnections
    -- Apply transport preference in case of alternative transports for the
    -- same client (currently only APNS vs APNS VoIP). If no explicit
    -- preference is given, the default preference depends on the priority.
    preference as =
      let pref = psh ^. pushNativeAps >>= view apsPreference
       in filter (pick (fromMaybe defPreference pref)) as
      where
        pick pr a = case a ^. addrTransport of
          GCM -> True
          APNS -> pr == ApsStdPreference || notAny a APNSVoIP
          APNSSandbox -> pr == ApsStdPreference || notAny a APNSVoIPSandbox
          APNSVoIP -> pr == ApsVoIPPreference || notAny a APNS
          APNSVoIPSandbox -> pr == ApsVoIPPreference || notAny a APNSSandbox
        notAny a t =
          not
            ( any
                ( \a' ->
                    addrEqualClient a a'
                      && a ^. addrApp == a' ^. addrApp
                      && a' ^. addrTransport == t
                )
                as
            )
        defPreference = case psh ^. pushNativePriority of
          LowPriority -> ApsStdPreference
          HighPriority -> ApsVoIPPreference
    check :: Either SomeException [a] -> m [a]
    check (Left e) = mntgtLogErr e >> pure []
    check (Right r) = pure r

type AddTokenResponse = Either Public.AddTokenError Public.AddTokenSuccess

addToken :: UserId -> ConnId -> PushToken -> Gundeck AddTokenResponse
addToken uid cid newtok = mpaRunWithBudget 1 (Left Public.AddTokenErrorNoBudget) $ do
  (cur, old) <- foldl' (matching newtok) (Nothing, []) <$> Data.lookup uid Data.LocalQuorum
  Log.info $
    "user"
      .= UUID.toASCIIBytes (toUUID uid)
      ~~ "token"
        .= Text.take 16 (tokenText (newtok ^. token))
      ~~ msg (val "Registering push token")
  continue newtok cur
    >>= either
      pure
      ( \a -> do
          Native.deleteTokens old (Just a)
          pure (Right $ Public.AddTokenSuccess newtok)
      )
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
      Gundeck (Either AddTokenResponse Address)
    continue t Nothing = create (0 :: Int) t
    continue t (Just a) = update (0 :: Int) t (a ^. addrEndpoint)

    create ::
      Int ->
      PushToken ->
      Gundeck (Either AddTokenResponse Address)
    create n t = do
      let trp = t ^. tokenTransport
      let app = t ^. tokenApp
      let tok = t ^. token
      env <- view (options . aws . arnEnv)
      aws' <- view awsEnv
      ept <- Aws.execute aws' (Aws.createEndpoint uid trp env app tok)
      case ept of
        Left (Aws.EndpointInUse arn) -> do
          Log.info $ "arn" .= toText arn ~~ msg (val "ARN in use")
          update (n + 1) t arn
        Left (Aws.AppNotFound app') -> do
          Log.info $ msg ("Push token of unknown application: '" <> appNameText app' <> "'")
          pure (Left (Left Public.AddTokenErrorNotFound))
        Left (Aws.InvalidToken _) -> do
          Log.info $
            "token"
              .= tokenText tok
              ~~ msg (val "Invalid push token.")
          pure (Left (Left Public.AddTokenErrorInvalid))
        Left (Aws.TokenTooLong l) -> do
          Log.info $ msg ("Push token is too long: token length = " ++ show l)
          pure (Left (Left Public.AddTokenErrorTooLong))
        Right arn -> do
          Data.insert uid trp app tok arn cid (t ^. tokenClient)
          pure (Right (mkAddr t arn))

    update ::
      Int ->
      PushToken ->
      SnsArn EndpointTopic ->
      Gundeck (Either AddTokenResponse Address)
    update n t arn = do
      when (n >= 3) $ do
        Log.err $ msg (val "AWS SNS inconsistency w.r.t. " +++ toText arn)
        throwM (mkError status500 "server-error" "Server Error")
      aws' <- view awsEnv
      ept <- Aws.execute aws' (Aws.lookupEndpoint arn)
      case ept of
        Nothing -> create (n + 1) t
        Just ep ->
          do
            updateEndpoint uid t arn ep
            Data.insert
              uid
              (t ^. tokenTransport)
              (t ^. tokenApp)
              (t ^. token)
              arn
              cid
              (t ^. tokenClient)
            pure (Right (mkAddr t arn))
            `catch` \case
              -- Note: If the endpoint was recently deleted (not necessarily
              -- concurrently), we may get an EndpointNotFound error despite
              -- the previous lookup, i.e. endpoint lookups may exhibit eventually
              -- consistent semantics with regards to endpoint deletion (or
              -- possibly updates in general). We make another attempt to (re-)create
              -- the endpoint in these cases instead of failing immediately.
              Aws.EndpointNotFound {} -> create (n + 1) t
              Aws.InvalidCustomData {} -> pure (Left (Left Public.AddTokenErrorMetadataTooLong))
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
  unless (equalTransport && equalApp) $ do
    Log.err $ logMessage uid arn (t ^. token) "Transport or app mismatch"
    throwM $ mkError status500 "server-error" "Server Error"
  Log.info $ logMessage uid arn (t ^. token) "Upserting push token."
  let users = Set.insert uid (e ^. endpointUsers)
  Aws.execute env $ Aws.updateEndpoint users (t ^. token) arn
  where
    equalTransport = t ^. tokenTransport == arn ^. snsTopic . endpointTransport
    equalApp = t ^. tokenApp == arn ^. snsTopic . endpointAppName
    logMessage a r tk m =
      "user"
        .= UUID.toASCIIBytes (toUUID a)
        ~~ "token"
          .= Text.take 16 (tokenText tk)
        ~~ "arn"
          .= toText r
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
