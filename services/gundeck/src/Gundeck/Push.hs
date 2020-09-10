-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Gundeck.Push
  ( push,
    AddTokenResponse (..),
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
import Control.Lens (view, (%~), (.~), (^.), _2)
import Control.Monad.Catch
import Data.Aeson as Aeson (Object)
import Data.Id
import qualified Data.List.Extra as List
import Data.List1 (List1, list1)
import qualified Data.Map as Map
import Data.Range
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import Gundeck.Aws (endpointUsers)
import qualified Gundeck.Aws as Aws
import Gundeck.Aws.Arn
import Gundeck.Env
import Gundeck.Monad
import qualified Gundeck.Notification.Data as Stream
import Gundeck.Options
import qualified Gundeck.Presence.Data as Presence
import qualified Gundeck.Push.Data as Data
import qualified Gundeck.Push.Native as Native
import Gundeck.Push.Native.Types
import qualified Gundeck.Push.Websocket as Web
import Gundeck.ThreadBudget
import Gundeck.Types
import qualified Gundeck.Types.Presence as Presence
import Gundeck.Util
import Imports
import Network.HTTP.Types
import Network.Wai.Utilities
import System.Logger.Class (msg, val, (+++), (.=), (~~))
import qualified System.Logger.Class as Log
import UnliftIO.Concurrent (forkIO)
import qualified Wire.API.Push.Token as Public

push :: [Push] -> Gundeck ()
push ps = do
  bulk :: Bool <- view (options . optSettings . setBulkPush)
  rs <-
    if bulk
      then (Right <$> pushAll ps) `catch` (pure . Left . Seq.singleton)
      else pushAny ps
  case rs of
    Right () -> return ()
    Left exs -> do
      forM_ exs $ Log.err . msg . (val "Push failed: " +++) . show
      throwM (Error status500 "server-error" "Server Error")

-- | Abstract over all effects in 'pushAll' (for unit testing).
class MonadThrow m => MonadPushAll m where
  mpaNotificationTTL :: m NotificationTTL
  mpaMkNotificationId :: m NotificationId
  mpaListAllPresences :: [UserId] -> m [[Presence]]
  mpaBulkPush :: [(Notification, [Presence])] -> m [(NotificationId, [Presence])]
  mpaStreamAdd :: NotificationId -> List1 NotificationTarget -> List1 Aeson.Object -> NotificationTTL -> m ()
  mpaPushNative :: Notification -> Push -> [Address] -> m ()
  mpaForkIO :: m () -> m ()
  mpaRunWithBudget :: Int -> a -> m a -> m a

instance MonadPushAll Gundeck where
  mpaNotificationTTL = view (options . optSettings . setNotificationTTL)
  mpaMkNotificationId = mkNotificationId
  mpaListAllPresences = Presence.listAll
  mpaBulkPush = Web.bulkPush
  mpaStreamAdd = Stream.add
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
  mntgtPerPushConcurrency = view (options . optSettings . setPerNativePushConcurrency)
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
    UserId ->
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
      mpaPushNative notif p =<< nativeTargets p (nativeTargetsRecipients p) alreadySent
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
  targets :: [(Push, (Notification, List1 (Recipient, [Presence])))] <-
    zip pushes <$> (mkNotificationAndTargets `mapM` pushes)
  -- persist push request
  let cassandraTargets :: [(Push, (Notification, List1 NotificationTarget))]
      cassandraTargets = (_2 . _2 %~ (mkTarget . fst <$>)) <$> targets
        where
          mkTarget :: Recipient -> NotificationTarget
          mkTarget r =
            target (r ^. recipientId)
              & targetClients .~ case r ^. recipientClients of
                RecipientClientsAll -> []
                -- clients are stored in cassandra as a list with a notification.  empty list
                -- is interpreted as "all clients" by 'Gundeck.Notification.Data.toNotif'.
                RecipientClientsSome cs -> toList cs
  forM_ cassandraTargets $ \(psh, (notif, notifTrgt)) ->
    unless (psh ^. pushTransient) $
      mpaStreamAdd (ntfId notif) notifTrgt (psh ^. pushPayload)
        =<< mpaNotificationTTL
  mpaForkIO $ do
    -- websockets
    resp <- compilePushResps targets <$> mpaBulkPush (compilePushReq <$> targets)
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
          mpaPushNative notif psh =<< nativeTargets psh rcps' alreadySent

-- REFACTOR: @[Presence]@ here should be @newtype WebSockedDelivered = WebSockedDelivered [Presence]@
compilePushReq :: (Push, (Notification, List1 (Recipient, [Presence]))) -> (Notification, [Presence])
compilePushReq (psh, notifsAndTargets) =
  notifsAndTargets & _2 %~ (mconcat . fmap compileTargets . toList)
  where
    compileTargets :: (Recipient, [Presence]) -> [Presence]
    compileTargets (rcp, pre) = filter (shouldActuallyPush psh rcp) pre

compilePushResps ::
  [(Push, (Notification, any))] ->
  [(NotificationId, [Presence])] ->
  [((Notification, Push), [Presence])]
compilePushResps notifIdMap (Map.fromList -> deliveries) =
  notifIdMap
    <&> (\(psh, (notif, _)) -> ((notif, psh), fromMaybe [] (Map.lookup (ntfId notif) deliveries)))

-- | Look up 'Push' recipients in Redis, construct a notifcation, and return all the data needed for
-- performing the push action.
--
-- 'Recipient' can be turned into 'NotificationTarget' needed for storing in C*; the 'Presence's can
-- be turned into 'PushTarget' needed by Cannon and in 'nativeTargets' for filtering the devices
-- that need no native push.
mkNotificationAndTargets ::
  forall m.
  MonadPushAll m =>
  Push ->
  m (Notification, List1 (Recipient, [Presence]))
mkNotificationAndTargets psh = (,) <$> mkNotif <*> doCollect
  where
    mkNotif :: m Notification
    mkNotif = do
      notifId <- mpaMkNotificationId
      pure $ Notification notifId (psh ^. pushTransient) (psh ^. pushPayload)
    doCollect :: m (List1 (Recipient, [Presence]))
    doCollect = zip1 rcps =<< mpaListAllPresences (view recipientId <$> rcps)
      where
        rcps :: [Recipient]
        rcps = toList . fromRange $ (psh ^. pushRecipients :: Range 1 1024 (Set.Set Recipient))
        zip1 :: [a] -> [b] -> m (List1 (a, b))
        zip1 (x : xs) (y : ys) = pure $ list1 (x, y) (zip xs ys)
        zip1 _ _ = throwM $ ErrorCall "mkNotificationAndTargets: internal error." -- can @listAll@ return @[]@?

-- | Is 'PushTarget' the origin of the 'Push', or is missing in a non-empty whitelist?  (Whitelists
-- reside both in 'Push' itself and in each 'Recipient').
shouldActuallyPush :: Push -> Recipient -> Presence -> Bool
shouldActuallyPush psh rcp pres = not isOrigin && okByPushWhitelist && okByRecipientWhitelist
  where
    isOrigin =
      psh ^. pushOrigin == userId pres
        && psh ^. pushOriginConnection == Just (connId pres)
    okByPushWhitelist = if whitelistExists then isWhitelisted else True
      where
        whitelist = psh ^. pushConnections
        whitelistExists = not $ Set.null whitelist
        isWhitelisted = connId pres `Set.member` whitelist
    okByRecipientWhitelist :: Bool
    okByRecipientWhitelist =
      case (rcp ^. recipientClients, clientId pres) of
        (RecipientClientsSome cs, Just c) -> c `elem` cs
        _ -> True

-- | Failures to push natively can be ignored.  Logging already happens in
-- 'Gundeck.Push.Native.push1', and we cannot recover from any of the error cases.
pushNative :: Notification -> Push -> [Address] -> Gundeck ()
pushNative _ _ [] = return ()
pushNative notif p rcps = do
  let prio = p ^. pushNativePriority
  Native.push (Native.NativePush (ntfId notif) prio Nothing) rcps

-- | Compute list of 'Recipient's from a 'Push' that may be interested in a native push.  More
-- filtering in 'nativeTargets'.
nativeTargetsRecipients :: Push -> [Recipient]
nativeTargetsRecipients psh = filter routeNative (toList (fromRange (psh ^. pushRecipients)))
  where
    routeNative u =
      u ^. recipientRoute /= RouteDirect
        && (u ^. recipientId /= psh ^. pushOrigin || psh ^. pushNativeIncludeOrigin)

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
      return $
        preference
          . filter (eligible u)
          $ addrs
    eligible :: Recipient -> Address -> Bool
    eligible u a
      -- Never include the origin client.
      | a ^. addrUser == psh ^. pushOrigin && Just (a ^. addrConn) == psh ^. pushOriginConnection = False
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
    check (Left e) = mntgtLogErr e >> return []
    check (Right r) = return r

data AddTokenResponse
  = AddTokenSuccess Public.PushToken
  | AddTokenNoBudget
  | AddTokenNotFound
  | AddTokenInvalid
  | AddTokenTooLong
  | AddTokenMetadataTooLong

addToken :: UserId -> ConnId -> PushToken -> Gundeck AddTokenResponse
addToken uid cid newtok = mpaRunWithBudget 1 AddTokenNoBudget $ do
  (cur, old) <- foldl' (matching newtok) (Nothing, []) <$> Data.lookup uid Data.Quorum
  Log.info $
    "user" .= UUID.toASCIIBytes (toUUID uid)
      ~~ "token" .= Text.take 16 (tokenText (newtok ^. token))
      ~~ msg (val "Registering push token")
  continue newtok cur
    >>= either
      return
      ( \a -> do
          Native.deleteTokens old (Just a)
          return (AddTokenSuccess newtok)
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
      env <- view (options . optAws . awsArnEnv)
      aws <- view awsEnv
      ept <- Aws.execute aws (Aws.createEndpoint uid trp env app tok)
      case ept of
        Left (Aws.EndpointInUse arn) -> do
          Log.info $ "arn" .= toText arn ~~ msg (val "ARN in use")
          update (n + 1) t arn
        Left (Aws.AppNotFound app') -> do
          Log.info $ msg ("Push token of unknown application: '" <> appNameText app' <> "'")
          return (Left AddTokenNotFound)
        Left (Aws.InvalidToken _) -> do
          Log.info $
            "token" .= tokenText tok
              ~~ msg (val "Invalid push token.")
          return (Left AddTokenInvalid)
        Left (Aws.TokenTooLong l) -> do
          Log.info $ msg ("Push token is too long: token length = " ++ show l)
          return (Left AddTokenTooLong)
        Right arn -> do
          Data.insert uid trp app tok arn cid (t ^. tokenClient)
          return (Right (mkAddr t arn))

    update ::
      Int ->
      PushToken ->
      SnsArn EndpointTopic ->
      Gundeck (Either AddTokenResponse Address)
    update n t arn = do
      when (n >= 3) $ do
        Log.err $ msg (val "AWS SNS inconsistency w.r.t. " +++ toText arn)
        throwM (Error status500 "server-error" "Server Error")
      aws <- view awsEnv
      ept <- Aws.execute aws (Aws.lookupEndpoint arn)
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
            return (Right (mkAddr t arn))
            `catch` \case
              -- Note: If the endpoint was recently deleted (not necessarily
              -- concurrently), we may get an EndpointNotFound error despite
              -- the previous lookup, i.e. endpoint lookups may exhibit eventually
              -- consistent semantics with regards to endpoint deletion (or
              -- possibly updates in general). We make another attempt to (re-)create
              -- the endpoint in these cases instead of failing immediately.
              Aws.EndpointNotFound {} -> create (n + 1) t
              Aws.InvalidCustomData {} -> return (Left AddTokenMetadataTooLong)
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
    throwM $ Error status500 "server-error" "Server Error"
  Log.info $ logMessage uid arn (t ^. token) "Upserting push token."
  let users = Set.insert uid (e ^. endpointUsers)
  Aws.execute env $ Aws.updateEndpoint users (t ^. token) arn
  where
    equalTransport = t ^. tokenTransport == arn ^. snsTopic . endpointTransport
    equalApp = t ^. tokenApp == arn ^. snsTopic . endpointAppName
    logMessage a r tk m =
      "user" .= UUID.toASCIIBytes (toUUID a)
        ~~ "token" .= Text.take 16 (tokenText tk)
        ~~ "arn" .= toText r
        ~~ msg (val m)

deleteToken :: UserId -> Token -> Gundeck ()
deleteToken uid tok = do
  as <- filter (\x -> x ^. addrToken == tok) <$> Data.lookup uid Data.Quorum
  when (null as) $
    throwM (Error status404 "not-found" "Push token not found")
  Native.deleteTokens as Nothing

listTokens :: UserId -> Gundeck PushTokenList
listTokens uid = PushTokenList . map (^. addrPushToken) <$> Data.lookup uid Data.Quorum
