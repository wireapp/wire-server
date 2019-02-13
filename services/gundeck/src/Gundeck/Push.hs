
module Gundeck.Push
    ( push
    , addToken
    , listTokens
    , deleteToken

      -- (for testing)
    , pushAll, pushAny
    , MonadPushAll(..)
    , MonadNativeTargets(..)
    , MonadPushAny(..)
    ) where

import Imports
import Control.Arrow ((&&&))
import Control.Error
import Control.Exception (ErrorCall(ErrorCall))
import Control.Lens ((^.), (.~), (%~), _2, view)
import Control.Monad.Catch
import Data.Aeson as Aeson (Object)
import Data.Id
import Data.List1 (List1, list1)
import Data.Predicate ((:::)(..))
import Data.Range
import Gundeck.Aws (endpointUsers)
import Gundeck.Aws.Arn
import Gundeck.Env
import Gundeck.Monad
import Gundeck.Push.Native.Types
import Gundeck.Options
import Gundeck.Types
import Gundeck.Util
import Network.HTTP.Types
import Network.Wai (Request, Response)
import Network.Wai.Utilities
import System.Logger.Class (msg, (.=), (~~), val, (+++))
import UnliftIO.Concurrent (forkIO)

import qualified Data.List.Extra              as List
import qualified Data.Sequence                as Seq
import qualified Data.Set                     as Set
import qualified Data.Map                     as Map
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import qualified Data.UUID                    as UUID
import qualified Gundeck.Aws                  as Aws
import qualified Gundeck.Notification.Data    as Stream
import qualified Gundeck.Presence.Data        as Presence
import qualified Gundeck.Push.Data            as Data
import qualified Gundeck.Push.Native          as Native
import qualified Gundeck.Push.Websocket       as Web
import qualified Gundeck.Types.Presence       as Presence
import qualified System.Logger.Class          as Log

push :: Request ::: JSON -> Gundeck Response
push (req ::: _) = do
    ps   :: [Push] <- fromBody req (Error status400 "bad-request")
    bulk :: Bool   <- view (options . optSettings . setBulkPush)
    rs             <- if bulk
                      then (Right <$> pushAll ps) `catch` (pure . Left . Seq.singleton)
                      else pushAny ps
    case rs of
        Right () -> return empty
        Left exs -> do
            forM_ exs $ Log.err . msg . (val "Push failed: " +++) . show
            throwM (Error status500 "server-error" "Server Error")

-- | Abstract over all effects in 'pushAll' (for unit testing).
class MonadThrow m =>  MonadPushAll m where
  mpaNotificationTTL  :: m NotificationTTL
  mpaMkNotificationId :: m NotificationId
  mpaListAllPresences :: [UserId] -> m [[Presence]]
  mpaBulkPush         :: [(Notification, [Presence])] -> m [(NotificationId, [Presence])]
  mpaStreamAdd        :: NotificationId -> List1 NotificationTarget -> List1 Aeson.Object -> NotificationTTL -> m ()
  mpaPushNative       :: Notification -> Push -> [Address "no-keys"] -> m ()
  mpaForkIO           :: m () -> m ()

instance MonadPushAll Gundeck where
  mpaNotificationTTL  = view (options . optSettings . setNotificationTTL)
  mpaMkNotificationId = mkNotificationId
  mpaListAllPresences = Presence.listAll
  mpaBulkPush         = Web.bulkPush
  mpaStreamAdd        = Stream.add
  mpaPushNative       = pushNative
  mpaForkIO           = void . forkIO

-- | Abstract over all effects in 'nativeTargets' (for unit testing).
class Monad m => MonadNativeTargets m where
  mntgtLogErr        :: SomeException -> m ()
  mntgtLookupAddress :: UserId -> m [Address "no-keys"]  -- ^ REFACTOR: rename to 'mntgtLookupAddresses'!
  mntgtMapAsync      :: (a -> m b) -> [a] -> m [Either SomeException b]

instance MonadNativeTargets Gundeck where
  mntgtLogErr e = Log.err (msg (val "Failed to get native push address: " +++ show e))
  mntgtLookupAddress rcp = Data.lookup rcp Data.One
  mntgtMapAsync = mapAsync

-- | Abstract over all effects in 'pushAny' (for unit testing).
class (MonadPushAll m, MonadNativeTargets m) => MonadPushAny m where
  mpyPush :: Notification
          -> List1 NotificationTarget
          -> UserId
          -> Maybe ConnId
          -> Set ConnId
          -> m [Presence]

instance MonadPushAny Gundeck where
  mpyPush = Web.push

-- | Send individual HTTP requests to cannon for every device and notification.
--
-- REFACTOR: This should go away in the future, once 'pushAll' has been proven to always do the same
-- thing.  also check what types this removal would make unnecessary.
pushAny
  :: forall m. (MonadPushAny m)
  => [Push] -> m (Either (Seq.Seq SomeException) ())
pushAny ps = collectErrors <$> mntgtMapAsync pushAny' ps
  where
    collectErrors :: [Either SomeException ()] -> Either (Seq.Seq SomeException) ()
    collectErrors = runAllE . foldMap (AllE . fmapL Seq.singleton)

pushAny'
  :: forall m. (MonadPushAny m)
  => Push -> m ()
pushAny' p = do
    i <- mpaMkNotificationId
    let pload = p^.pushPayload
    let notif = Notification i (p^.pushTransient) pload
    let rcps  = fromRange (p^.pushRecipients)
    let uniq  = uncurry list1 $ head &&& tail $ toList rcps
    let tgts  = mkTarget <$> uniq
    unless (p^.pushTransient) $
        mpaStreamAdd i tgts pload =<< mpaNotificationTTL
    mpaForkIO $ do
        prs <- mpyPush notif tgts (p^.pushOrigin) (p^.pushOriginConnection) (p^.pushConnections)
        unless (p^.pushTransient) $
            mpaPushNative notif p =<< nativeTargets p prs
  where
    mkTarget :: Recipient -> NotificationTarget
    mkTarget r =
      target (r^.recipientId)
        & targetClients .~ case r^.recipientClients of
            RecipientClientsAll -> []
            RecipientClientsSome cs -> toList cs

-- | Construct and send a single bulk push request to the client.  Write the 'Notification's from
-- the request to C*.  Trigger native pushes for all delivery failures notifications.
pushAll :: (MonadPushAll m, MonadNativeTargets m) => [Push] -> m ()
pushAll pushes = do
    targets :: [(Push, (Notification, List1 (Recipient, [Presence])))]
            <- zip pushes <$> (mkNotificationAndTargets `mapM` pushes)

    -- persist push request
    let cassandraTargets :: [(Push, (Notification, List1 NotificationTarget))]
        cassandraTargets = (_2 . _2 %~ (mkTarget . fst <$>)) <$> targets
          where
            mkTarget :: Recipient -> NotificationTarget
            mkTarget r =
              target (r^.recipientId)
                & targetClients .~ case r^.recipientClients of
                    RecipientClientsAll -> []
                      -- clients are stored in cassandra as a list with a notification.  empty list
                      -- is interpreted as "all clients" by 'Gundeck.Notification.Data.toNotif'.
                    RecipientClientsSome cs -> toList cs

    forM_ cassandraTargets $ \(psh, (notif, notifTrgt)) -> unless (psh ^. pushTransient) $
        mpaStreamAdd (ntfId notif) notifTrgt (psh ^. pushPayload)
          =<< mpaNotificationTTL

    mpaForkIO $ do
        -- websockets
        resp <- compilePushResps targets <$> mpaBulkPush (compilePushReq <$> targets)

        -- native push
        forM_ resp $ \((notif, psh), alreadySent) -> unless (psh ^. pushTransient) $
            mpaPushNative notif psh =<< nativeTargets psh alreadySent


-- REFACTOR: @[Presence]@ here should be @newtype WebSockedDelivered = WebSockedDelivered [Presence]@
compilePushReq :: (Push, (Notification, List1 (Recipient, [Presence]))) -> (Notification, [Presence])
compilePushReq (psh, notifsAndTargets) =
    notifsAndTargets & _2 %~ (mconcat . fmap compileTargets . toList)
  where
    compileTargets :: (Recipient, [Presence]) -> [Presence]
    compileTargets (rcp, pre) = filter (shouldActuallyPush psh rcp) pre

compilePushResps
  :: [(Push, (Notification, any))]
  -> [(NotificationId, [Presence])]
  -> [((Notification, Push), [Presence])]
compilePushResps notifIdMap (Map.fromList -> deliveries) =
  notifIdMap <&>
    (\(psh, (notif, _)) -> ((notif, psh), fromMaybe [] (Map.lookup (ntfId notif) deliveries)))


-- | Look up 'Push' recipients in Redis, construct a notifcation, and return all the data needed for
-- performing the push action.
--
-- 'Recipient' can be turned into 'NotificationTarget' needed for storing in C*; the 'Presence's can
-- be turned into 'PushTarget' needed by Cannon and in 'nativeTargets' for filtering the devices
-- that need no native push.
mkNotificationAndTargets :: forall m. MonadPushAll m
                         => Push -> m (Notification, List1 (Recipient, [Presence]))
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
        zip1 (x:xs) (y:ys) = pure $ list1 (x, y) (zip xs ys)
        zip1 _ _ = throwM $ ErrorCall "mkNotificationAndTargets: internal error."  -- can @listAll@ return @[]@?


-- | Is 'PushTarget' the origin of the 'Push', or is missing in a non-empty whitelist?  (Whitelists
-- reside both in 'Push' itself and in each 'Recipient').
shouldActuallyPush :: Push -> Recipient -> Presence -> Bool
shouldActuallyPush psh rcp pres = not isOrigin && okByPushWhitelist && okByRecipientWhitelist
  where
    isOrigin = psh ^. pushOrigin == userId pres &&
               psh ^. pushOriginConnection == Just (connId pres)

    okByPushWhitelist = if whitelistExists then isWhitelisted else True
      where
        whitelist = psh ^. pushConnections
        whitelistExists = not $ Set.null whitelist
        isWhitelisted = connId pres `Set.member` whitelist

    okByRecipientWhitelist :: Bool
    okByRecipientWhitelist =
        case (rcp ^. recipientClients, clientId pres) of
            (RecipientClientsSome cs, Just c) -> c `elem` cs
            _                                 -> True


-- | Failures to push natively can be ignored.  Logging already happens in
-- 'Gundeck.Push.Native.push1', and we cannot recover from any of the error cases.
pushNative :: Notification -> Push -> [Address "no-keys"] -> Gundeck ()
pushNative _ _ [] = return ()
pushNative notif p rcps = do
    let prio = p^.pushNativePriority
    void $ Native.push (Native.Notice (ntfId notif) prio Nothing) rcps

nativeTargets :: forall m. MonadNativeTargets m => Push -> [Presence] -> m [Address "no-keys"]
nativeTargets p pres =
    let rcps' = filter routeNative (toList (fromRange (p^.pushRecipients)))
    in mntgtMapAsync addresses rcps' >>= fmap concat . mapM check
  where
    -- Interested in native pushes?
    routeNative u = u^.recipientRoute /= RouteDirect
                 && (u^.recipientId /= p^.pushOrigin || p^.pushNativeIncludeOrigin)

    addresses :: Recipient -> m [Address "no-keys"]
    addresses u = do
        addrs <- mntgtLookupAddress (u^.recipientId)
        return $ preference
               . filter (eligible u)
               $ addrs

    eligible u a
        -- Never include the origin client.
        | a^.addrUser == p^.pushOrigin && Just (a^.addrConn) == p^.pushOriginConnection = False
        -- Is the specific client an intended recipient?
        | not (eligibleClient a (u^.recipientClients)) = False
        -- Is the client not whitelisted?
        | not (whitelistedOrNoWhitelist a) = False
        -- Include client if not found in presences.
        | otherwise = isNothing (List.find (isOnline a) pres)

    isOnline a x =  a^.addrUser == Presence.userId x
                && (a^.addrConn == Presence.connId x || equalClient a x)

    equalClient a x = Just (a^.addrClient) == Presence.clientId x

    eligibleClient _ RecipientClientsAll = True
    eligibleClient a (RecipientClientsSome cs) = (a^.addrClient) `elem` cs

    whitelistedOrNoWhitelist a = null (p^.pushConnections)
                              || a^.addrConn `elem` p^.pushConnections

    -- Apply transport preference in case of alternative transports for the
    -- same client (currently only APNS vs APNS VoIP). If no explicit
    -- preference is given, the default preference depends on the priority.
    preference as = let pref = p^.pushNativeAps >>= view apsPreference in
        filter (pick (fromMaybe defPreference pref)) as
      where
        pick pr a = case a^.addrTransport of
            GCM             -> True
            APNS            -> pr == ApsStdPreference  || notAny a APNSVoIP
            APNSSandbox     -> pr == ApsStdPreference  || notAny a APNSVoIPSandbox
            APNSVoIP        -> pr == ApsVoIPPreference || notAny a APNS
            APNSVoIPSandbox -> pr == ApsVoIPPreference || notAny a APNSSandbox

        notAny a t = not (any (\a' ->
            addrEqualClient a a'
            && a^.addrApp == a'^.addrApp
            && a'^.addrTransport == t) as)

        defPreference = case p^.pushNativePriority of
            LowPriority  -> ApsStdPreference
            HighPriority -> ApsVoIPPreference

    check :: Either SomeException [a] -> m [a]
    check (Left  e) = mntgtLogErr e >> return []
    check (Right r) = return r

addToken :: UserId ::: ConnId ::: Request ::: JSON ::: JSON -> Gundeck Response
addToken (uid ::: cid ::: req ::: _) = do
    new <- fromBody req (Error status400 "bad-request")
    (cur, old) <- foldl' (matching new) (Nothing, []) <$> Data.lookup uid Data.Quorum
    Log.info $ "user"  .= UUID.toASCIIBytes (toUUID uid)
            ~~ "token" .= Text.take 16 (tokenText (new^.token))
            ~~ msg (val "Registering push token")
    continue new cur >>= either return (\a -> do
        Native.deleteTokens old (Just a)
        return (success new))
  where
    matching t (x, old) a
        | a^.addrTransport  == t^.tokenTransport &&
          a^.addrApp        == t^.tokenApp       &&
          a^.addrClient     == t^.tokenClient    =
            if a^.addrToken == t^.token
                then (Just a, old)
                else (x, a : old)
        | otherwise = (x, old)

    continue t Nothing  = create (0 :: Int) t
    continue t (Just a) = update (0 :: Int) t (a^.addrEndpoint)

    create :: Int -> PushToken -> Gundeck (Either Response (Address "no-keys"))
    create n t = do
        let trp = t^.tokenTransport
        let app = t^.tokenApp
        let tok = t^.token
        env <- view (options.optAws.awsArnEnv)
        aws <- view awsEnv
        ept <- Aws.execute aws (Aws.createEndpoint uid trp env app tok)
        case ept of
            Left (Aws.EndpointInUse arn) -> do
                Log.info $ "arn" .= toText arn ~~ msg (val "ARN in use")
                update (n + 1) t arn
            Left (Aws.AppNotFound app') -> do
                Log.info $ msg ("Push token of unknown application: '" <> appNameText app' <> "'")
                return (Left notFound)
            Left (Aws.InvalidToken _) -> do
                Log.info $ "token" .= tokenText tok
                        ~~ msg (val "Invalid push token.")
                return (Left invalidToken)
            Left (Aws.TokenTooLong l) -> do
                Log.info $ msg ("Push token is too long: token length = " ++ show l)
                return (Left tokenTooLong)
            Right arn -> do
                Data.insert uid trp app tok arn cid (t^.tokenClient)
                return (Right (mkAddr t arn))

    update :: Int -> PushToken -> SnsArn EndpointTopic -> Gundeck (Either Response (Address "no-keys"))
    update n t arn = do
        when (n >= 3) $ do
            Log.err $ msg (val "AWS SNS inconsistency w.r.t. " +++ toText arn)
            throwM (Error status500 "server-error" "Server Error")
        aws <- view awsEnv
        ept <- Aws.execute aws (Aws.lookupEndpoint arn)
        case ept of
            Nothing -> create (n + 1) t
            Just ep -> do
                updateEndpoint uid t arn ep
                Data.insert uid (t^.tokenTransport) (t^.tokenApp) (t^.token) arn cid
                                (t^.tokenClient)
                return (Right (mkAddr t arn))
              `catch` \case
                -- Note: If the endpoint was recently deleted (not necessarily
                -- concurrently), we may get an EndpointNotFound error despite
                -- the previous lookup, i.e. endpoint lookups may exhibit eventually
                -- consistent semantics with regards to endpoint deletion (or
                -- possibly updates in general). We make another attempt to (re-)create
                -- the endpoint in these cases instead of failing immediately.
                Aws.EndpointNotFound  {} -> create (n + 1) t
                Aws.InvalidCustomData {} -> return (Left metadataTooLong)
                ex                       -> throwM ex

    mkAddr t arn = Address uid (t^.tokenTransport) (t^.tokenApp) (t^.token)
                           arn cid (t^.tokenClient)

-- | Update an SNS endpoint with the given user and token.
updateEndpoint :: UserId -> PushToken -> EndpointArn -> Aws.SNSEndpoint -> Gundeck ()
updateEndpoint uid t arn e = do
    env  <- view awsEnv
    unless (equalTransport && equalApp) $ do
        Log.err $ logMessage uid arn (t^.token) "Transport or app mismatch"
        throwM $ Error status500 "server-error" "Server Error"
    Log.info $ logMessage uid arn (t^.token) "Upserting push token."
    let users = Set.insert uid (e^.endpointUsers)
    Aws.execute env $ Aws.updateEndpoint users (t^.token) arn
  where
    equalTransport = t^.tokenTransport == arn^.snsTopic.endpointTransport
    equalApp       = t^.tokenApp       == arn^.snsTopic.endpointAppName

    logMessage a r tk m =
          "user"  .= UUID.toASCIIBytes (toUUID a)
       ~~ "token" .= Text.take 16 (tokenText tk)
       ~~ "arn"   .= toText r
       ~~ msg (val m)

deleteToken :: UserId ::: Token ::: JSON -> Gundeck Response
deleteToken (uid ::: tok ::: _) = do
    as <- filter (\x -> x^.addrToken == tok) <$> Data.lookup uid Data.Quorum
    when (null as) $
        throwM (Error status404 "not-found" "Push token not found")
    Native.deleteTokens as Nothing
    return $ empty & setStatus status204

success :: PushToken -> Response
success t =
    let loc = Text.encodeUtf8 . tokenText $ t^.token in
    json t & setStatus status201 & addHeader hLocation loc

invalidToken :: Response
invalidToken = json (Error status400 "invalid-token" "Invalid push token")
             & setStatus status404

tokenTooLong :: Response
tokenTooLong = json (Error status400 "token-too-long" "Push token length must be < 8192 for GCM or 400 for APNS")
             & setStatus status413

metadataTooLong :: Response
metadataTooLong = json (Error status400 "metadata-too-long" "Tried to add token to endpoint resulting in metadata length > 2048")
             & setStatus status413

notFound :: Response
notFound = empty & setStatus status404

listTokens :: UserId ::: JSON -> Gundeck Response
listTokens (uid ::: _) =
    setStatus status200 . json . PushTokenList . map toToken <$> Data.lookup uid Data.Quorum
  where
    toToken :: Address s -> PushToken
    toToken a = pushToken (a^.addrTransport) (a^.addrApp) (a^.addrToken) (a^.addrClient)
