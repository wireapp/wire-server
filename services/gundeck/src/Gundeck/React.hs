{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Curate native push tokens based on provider feedback reported
-- via SNS events.
module Gundeck.React (onEvent) where

import Control.Lens ((^.), view, (.~), (&))
import Control.Monad
import Data.ByteString.Conversion
import Data.Id (UserId, ClientId)
import Data.List1
import Data.Foldable (for_)
import Data.Text (Text)
import Gundeck.Aws (SNSEndpoint, endpointEnabled, endpointToken, endpointUsers)
import Gundeck.Aws.Arn
import Gundeck.Aws.Sns
import Gundeck.Env
import Gundeck.Instances ()
import Gundeck.Monad
import Gundeck.Options (notificationTTL, optSettings)
import Gundeck.Push.Native.Types
import Gundeck.Types
import Gundeck.Util
import System.Logger.Class (Msg, msg, (.=), (~~), val, (+++))

import qualified Data.List                 as List
import qualified Data.Set                  as Set
import qualified Data.Text                 as Text
import qualified Gundeck.Aws               as Aws
import qualified Gundeck.Notification.Data as Stream
import qualified Gundeck.Push.Data         as Push
import qualified Gundeck.Push.Websocket    as Web
import qualified System.Logger.Class       as Log

onEvent :: Event -> Gundeck ()
onEvent ev = case ev^.evType of
    EndpointUpdated                            -> onUpdated ev
    DeliveryFailure DeliveryInvalidToken       -> onPermFailure ev
    DeliveryFailure DeliveryEndpointDisabled   -> onFailure ev
    DeliveryFailure DeliveryFailedPerm         -> onPermFailure ev
    DeliveryFailure DeliveryTTLExpired         -> onTTLExpired ev
    DeliveryFailure (DeliveryUnknownFailure r) -> onUnknownFailure ev r
    _                                          -> onUnhandledEventType ev

onUpdated :: Event -> Gundeck ()
onUpdated ev = withEndpoint ev $ \e as ->
    if not (e^.endpointEnabled)
        then do
            forM_ as $ \a -> do
                logUserEvent (a^.addrUser) ev $ msg (val "Removing disabled token")
                deleteToken (a^.addrUser) ev (a^.addrToken) (a^.addrClient)
            deleteEndpoint ev
        else do
            -- Note: The assumption is always that if the tokens differ, then
            --       the token in the SNS endpoint has been updated due to provider
            --       feedback (e.g. GCM canonical IDs) and the new token must
            --       already have been registered by the client. Hence the
            --       existing tokens are considered superseded and removed.
            --       This is crucial for proper token curation.
            let (sup, cur) = List.partition (\a -> e^.endpointToken /= a^.addrToken) as
            forM_ sup $ \a -> do
                logUserEvent (a^.addrUser) ev $ msg (val "Removing superseded token")
                deleteToken (a^.addrUser) ev (a^.addrToken) (a^.addrClient)
            if | null sup  -> return ()
               | null cur  -> deleteEndpoint ev
               | otherwise -> updateEndpoint ev e (map (view addrUser) cur)

onFailure :: Event -> Gundeck ()
onFailure ev = withEndpoint ev $ \e as ->
    unless (e^.endpointEnabled) $ do
        forM_ as $ \a -> do
            logUserEvent (a^.addrUser) ev $ msg (val "Removing disabled token")
            deleteToken (a^.addrUser) ev (a^.addrToken) (a^.addrClient)
        deleteEndpoint ev

onPermFailure :: Event -> Gundeck ()
onPermFailure ev = withEndpoint ev $ \_ as -> do
    forM_ as $ \a -> do
        logUserEvent (a^.addrUser) ev $ msg (val "Removing invalid token")
        deleteToken (a^.addrUser) ev (a^.addrToken) (a^.addrClient)
    deleteEndpoint ev

onTTLExpired :: Event -> Gundeck ()
onTTLExpired ev = Log.warn $
       "arn"   .= toText (ev^.evEndpoint)
    ~~ "cause" .= toText (ev^.evType)
    ~~ msg (val "Notification TTL expired")

onUnknownFailure :: Event -> Text -> Gundeck ()
onUnknownFailure ev r = Log.warn $
       "arn"   .= toText (ev^.evEndpoint)
    ~~ "cause" .= toText (ev^.evType)
    ~~ msg (val "Unknown failure, reason: " +++ r)

onUnhandledEventType :: Event -> Gundeck ()
onUnhandledEventType ev = Log.warn $
       "arn"   .= toText (ev^.evEndpoint)
    ~~ "cause" .= toText (ev^.evType)
    ~~ msg (val "Unhandled event type")


-------------------------------------------------------------------------------
-- Utilities

withEndpoint
    :: Event
    -> (SNSEndpoint -> [Address "no-keys"] -> Gundeck ())
    -> Gundeck ()
withEndpoint ev f = do
    v <- view awsEnv
    e <- Aws.execute v (Aws.lookupEndpoint (ev^.evEndpoint))
    for_ e $ \ep -> do
        let us = Set.toList (ep^.endpointUsers)
        as <- concat <$> mapM (`Push.lookup` Push.Quorum) us
        case List.filter ((== (ev^.evEndpoint)) . view addrEndpoint) as of
            []  -> do
                logEvent ev $ "token" .= Text.take 16 (tokenText (ep^.endpointToken))
                           ~~ msg (val "Deleting orphaned SNS endpoint")
                Aws.execute v (Aws.deleteEndpoint (ev^.evEndpoint))
            as' -> f ep as'

deleteEndpoint :: Event -> Gundeck ()
deleteEndpoint ev = do
    logEvent ev $ msg (val "Deleting SNS endpoint")
    v <- view awsEnv
    Aws.execute v (Aws.deleteEndpoint (ev^.evEndpoint))

updateEndpoint :: Event -> SNSEndpoint -> [UserId] -> Gundeck ()
updateEndpoint ev ep us = do
    logEvent ev $ msg (val "Updating SNS endpoint")
    v <- view awsEnv
    Aws.execute v (Aws.updateEndpoint (Set.fromList us) (ep^.endpointToken) (ev^.evEndpoint))

deleteToken :: UserId -> Event -> Token -> ClientId -> Gundeck ()
deleteToken u ev tk cl = do
    logUserEvent u ev $ "token" .= Text.take 16 (tokenText tk)
                 ~~ msg (val "Deleting push token")
    i <- mkNotificationId
    let t = mkPushToken ev tk cl
        p = singletonPayload (PushRemove t)
        n = Notification i False p
        r = singleton (target u & targetClients .~ [cl])
    void $ Web.push n r u Nothing Set.empty
    Stream.add i r p =<< view (options.optSettings.notificationTTL)
    Push.delete u (t^.tokenTransport) (t^.tokenApp) tk

mkPushToken :: Event -> Token -> ClientId -> PushToken
mkPushToken ev tk cl = let t = ev^.evEndpoint.snsTopic in
    pushToken (t^.endpointTransport) (t^.endpointAppName) tk cl

logEvent :: Event -> (Msg -> Msg) -> Gundeck ()
logEvent ev f = Log.info $
       "arn"   .= toText (ev^.evEndpoint)
    ~~ "cause" .= toText (ev^.evType)
    ~~ f

logUserEvent :: UserId -> Event -> (Msg -> Msg) -> Gundeck ()
logUserEvent u ev f = logEvent ev $
       "user"  .= toByteString u
    ~~ f
