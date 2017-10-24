{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Gundeck.Push
    ( push
    , addToken
    , listTokens
    , deleteToken
    , cancelFallback
    ) where

import Control.Arrow ((&&&))
import Control.Concurrent.Async.Lifted.Safe (async, wait)
import Control.Concurrent.Lifted (fork)
import Control.Error
import Control.Lens ((^.), (&), (.~), view, set)
import Control.Monad (when, unless, void)
import Control.Monad.Catch
import Data.Foldable (toList, forM_, foldl')
import Data.Id
import Data.List (partition)
import Data.List1 (list1)
import Data.Monoid
import Data.Predicate ((:::)(..))
import Data.Range
import Gundeck.Aws (endpointUsers)
import Gundeck.Aws.Arn
import Gundeck.Env
import Gundeck.Monad
import Gundeck.Push.Native.Types
import Gundeck.Options hiding (aws)
import Gundeck.Types
import Gundeck.Util
import Network.HTTP.Types
import Network.Wai (Request, Response)
import Network.Wai.Utilities
import System.Logger.Class (msg, (.=), (~~), val, (+++))

import qualified Data.List.Extra              as List
import qualified Data.Sequence                as Seq
import qualified Data.Set                     as Set
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import qualified Data.UUID                    as UUID
import qualified Gundeck.Aws                  as Aws
import qualified Gundeck.Client               as Client
import qualified Gundeck.Notification.Data    as Stream
import qualified Gundeck.Options              as Opt
import qualified Gundeck.Push.Data            as Data
import qualified Gundeck.Push.Native          as Native
import qualified Gundeck.Push.Native.Fallback as Fallback
import qualified Gundeck.Push.Websocket       as Web
import qualified Gundeck.Types.Presence       as Presence
import qualified System.Logger.Class          as Log

push :: Request ::: JSON -> Gundeck Response
push (req ::: _) = do
    ps <- fromBody req (Error status400 "bad-request")
    rs <- mapAsync pushAny (ps :: [Push])
    case runAllE (foldMap (AllE . fmapL Seq.singleton) rs) of
        Right () -> return empty
        Left exs -> do
            forM_ exs $ Log.err . msg . (val "Push failed: " +++) . show
            throwM (Error status500 "server-error" "Server Error")
  where
    pushAny p = do
        i <- mkNotificationId
        let pload = p^.pushPayload
        let notif = Notification i (p^.pushTransient) pload
        let rcps  = fromRange (p^.pushRecipients)
        let uniq  = uncurry list1 $ head &&& tail $ toList rcps
        let tgts  = mkTarget <$> uniq
        unless (p^.pushTransient) $
            Stream.add i tgts pload =<< view (options.optSettings.notificationTTL)
        void . fork $ do
            prs <- Web.push notif tgts (p^.pushOrigin) (p^.pushOriginConnection) (p^.pushConnections)
            pushNative notif p =<< nativeTargets p prs

    mkTarget r = target (r^.recipientId) & targetClients .~ r^.recipientClients

    pushNative notif p rcps
        | ntfTransient notif = pushData p notif rcps
        | otherwise = case partition (preferNotice (p^.pushOrigin)) rcps of
            (xs, []) -> pushNotice p notif xs
            ([], ys) -> pushData p notif ys
            (xs, ys) -> do
                a <- async $ pushNotice p notif xs
                pushData p notif ys
                wait a

    -- If a fallback address is set, the push is not transient and
    -- the address does not belong to the origin user, we prefer
    -- type=notice notifications even for the first attempt, since the device
    -- has to make a request to cancel the fallback notification in any
    -- case, which it can combine with fetching the notification in a single
    -- request. We can thus save the effort of encrypting and decrypting native
    -- push payloads to such addresses.
    preferNotice orig a = isJust (a^.addrFallback)
                       && orig /= (a^.addrUser)

    pushNotice _     _   [] = return ()
    pushNotice p notif rcps = do
        let prio = p^.pushNativePriority
        r <- Native.push (Native.Notice (ntfId notif) prio Nothing) rcps
        pushFallback (p^.pushOrigin) notif r prio

    pushData _     _   [] = return ()
    pushData p notif rcps = do
        let aps = p^.pushNativeAps
        let prio = p^.pushNativePriority
        if p^.pushNativeEncrypt then do
            c <- view cipher
            d <- view digest
            t <- Client.lookupKeys rcps
            r <- Native.push (Native.Ciphertext notif c d prio aps) t
            pushFallback (p^.pushOrigin) notif r prio
        else do
            r <- Native.push (Native.Plaintext notif prio aps) rcps
            pushFallback (p^.pushOrigin) notif r prio

    -- Process fallback notifications, which can either be immediate (e.g.
    -- because the push payload was too large) or delayed if a fallback push
    -- address is set. Fallback notifications are always of type=notice and
    -- thus only non-transient notifications are eligible for a fallback.
    pushFallback orig notif r prio = case Fallback.prepare orig r of
        Nothing  -> return ()
        Just can ->
            if ntfTransient notif
                then Log.warn $ msg (val "Transient notification failed")
                else void $ Fallback.execute (ntfId notif) prio can

nativeTargets :: Push -> [Presence] -> Gundeck [Address "no-keys"]
nativeTargets p pres =
    let rcps' = filter routeNative (toList (fromRange (p^.pushRecipients)))
    in mapAsync addresses rcps' >>= fmap concat . mapM check
  where
    -- Interested in native pushes?
    routeNative u = u^.recipientRoute /= RouteDirect
                 && (u^.recipientId /= p^.pushOrigin || p^.pushNativeIncludeOrigin)

    addresses u = do
        addrs <- Data.lookup (u^.recipientId) Data.One
        return $ preference
               . map (checkFallback u)
               . filter (eligible u)
               $ addrs

    eligible u a
        -- Never include the origin client.
        | a^.addrUser == p^.pushOrigin && Just (a^.addrConn) == p^.pushOriginConnection = False
        -- Is the specific client an intended recipient?
        | not (eligibleClient a (u^.recipientClients)) = False
        -- Include client if not found in presences.
        | otherwise = isNothing (List.find (isOnline a) pres)

    checkFallback u a
        | u^.recipientFallback = a
        | otherwise            = set addrFallback Nothing a

    isOnline a x =  a^.addrUser == Presence.userId x
                && (a^.addrConn == Presence.connId x || equalClient a x)

    equalClient a x = Just (a^.addrClient) == Presence.clientId x

    eligibleClient _ [] = True
    eligibleClient a cs = (a^.addrClient) `elem` cs

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

    check (Left  e) = Log.err (msg (val "Failed to get native push address: " +++ show e))
                   >> return []
    check (Right r) = return r

addToken :: UserId ::: ConnId ::: Request ::: JSON ::: JSON -> Gundeck Response
addToken (uid ::: cid ::: req ::: _) = do
    new <- fromBody req (Error status400 "bad-request")
    unless (validFallback new) $
        throwM invalidFallback
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

    create n t = do
        let trp = t^.tokenTransport
        let app = t^.tokenApp
        let tok = t^.token
        env <- view (options.Opt.aws.awsArnEnv)
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
            Right arn -> do
                Data.insert uid trp app tok arn cid (t^.tokenClient) (t^.tokenFallback)
                return (Right (mkAddr t arn))

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
                                (t^.tokenClient) (t^.tokenFallback)
                return (Right (mkAddr t arn))
              `catch` \case
                -- Note: If the endpoint was recently deleted (not necessarily
                -- concurrently), we may get an EndpointNotFound error despite
                -- the previous lookup, i.e. endpoint lookups may exhibit eventually
                -- consistent semantics with regards to endpoint deletion (or
                -- possibly updates in general). We make another attempt to (re-)create
                -- the endpoint in these cases instead of failing immediately.
                Aws.EndpointNotFound {} -> create (n + 1) t
                ex                      -> throwM ex

    mkAddr t arn = Address uid (t^.tokenTransport) (t^.tokenApp) (t^.token)
                           arn cid (t^.tokenClient) Nothing (t^.tokenFallback)

    validFallback t = case (t^.tokenTransport, t^.tokenFallback) of
        (              _,          Nothing) -> True
        (       APNSVoIP,        Just APNS) -> True
        (APNSVoIPSandbox, Just APNSSandbox) -> True
        (              _,                _) -> False

    invalidFallback = Error status403 "invalid-fallback" "Invalid fallback transport."

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

notFound :: Response
notFound = empty & setStatus status404

listTokens :: UserId ::: JSON -> Gundeck Response
listTokens (uid ::: _) =
    setStatus status200 . json . map toToken <$> Data.lookup uid Data.Quorum
  where
    toToken a = pushToken (a^.addrTransport) (a^.addrApp) (a^.addrToken) (a^.addrClient)

cancelFallback :: UserId ::: NotificationId -> Gundeck Response
cancelFallback (u ::: n) = Fallback.cancel u n >> return empty

