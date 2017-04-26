{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Curate native push tokens based on provider feedback reported
-- via SNS events.
module Gundeck.React (onEvent) where

import Control.Exception (ErrorCall (..))
import Control.Lens ((^.), view, (.~), (&))
import Control.Monad
import Control.Monad.Catch
import Data.ByteString.Conversion
import Data.Id (UserId, ClientId)
import Data.List1
import Data.Foldable (for_)
import Gundeck.Aws (Endpoint, endpointEnabled, endpointToken)
import Gundeck.Aws.Arn
import Gundeck.Aws.Sns
import Gundeck.Env
import Gundeck.Instances ()
import Gundeck.Monad
import Gundeck.Options (notificationTTL)
import Gundeck.Push.Native.Types
import Gundeck.Types
import Gundeck.Util
import System.Logger.Class (Msg, msg, (.=), (~~), val)

import qualified Data.List                 as List
import qualified Data.Set                  as Set
import qualified Data.Text                 as Text
import qualified Gundeck.Aws               as Aws
import qualified Gundeck.Notification.Data as Stream
import qualified Gundeck.Push.Data         as Data
import qualified Gundeck.Push.Websocket    as Web
import qualified System.Logger.Class       as Log

onEvent :: Event -> Gundeck ()
onEvent ev = case ev^.evType of
    EndpointUpdated                          -> onUpdated ev
    DeliveryFailure DeliveryInvalidToken     -> onPermFailure ev
    DeliveryFailure DeliveryEndpointDisabled -> onFailure ev
    DeliveryFailure DeliveryFailedPerm       -> onPermFailure ev
    DeliveryFailure DeliveryTTLExpired       -> onTTLExpired ev
    _                                        -> return ()

onUpdated :: Event -> Gundeck ()
onUpdated ev = withToken ev $ \u e a ->
    if | not (e^.endpointEnabled) -> do
            logEvent u ev $ msg (val "Removing disabled endpoint and token")
            deleteToken u ev (a^.addrToken) (a^.addrClient)
            deleteEndpoint u ev
       | e^.endpointToken /= a^.addrToken -> do
            logEvent u ev $ msg (val "Removing superseded endpoint and token")
            deleteToken u ev (a^.addrToken) (a^.addrClient)
            deleteEndpoint u ev
       | otherwise -> return ()

onFailure :: Event -> Gundeck ()
onFailure ev = withToken ev $ \u e a ->
    unless (e^.endpointEnabled) $ do
        logEvent u ev $ msg (val "Removing disabled endpoint and token")
        deleteToken u ev (a^.addrToken) (a^.addrClient)
        deleteEndpoint u ev

onPermFailure :: Event -> Gundeck ()
onPermFailure ev = withToken ev $ \u _ a -> do
    logEvent u ev $ msg (val "Removing invalid endpoint and token")
    deleteToken u ev (a^.addrToken) (a^.addrClient)
    deleteEndpoint u ev

onTTLExpired :: Event -> Gundeck ()
onTTLExpired ev = Log.warn $
       "arn"   .= toText (ev^.evEndpoint)
    ~~ "cause" .= toText (ev^.evType)
    ~~ msg (val "Notification TTL expired")

-------------------------------------------------------------------------------
-- Utilities

withToken :: Event
          -> (UserId -> Endpoint -> Address "no-keys" -> Gundeck ())
          -> Gundeck ()
withToken ev f = do
    v <- view awsEnv
    e <- Aws.execute v (Aws.lookupEndpoint (ev^.evEndpoint))
    for_ e $ \ep -> do
        u  <- Aws.readEndpointData invalidData ep
        as <- Data.lookup u
        case List.find ((== (ev^.evEndpoint)) . view addrEndpoint) as of
            Nothing -> do
                logEvent u ev $ "token" .= Text.take 16 (tokenText (ep^.endpointToken))
                             ~~ msg (val "Deleting orphaned platform endpoint")
                Aws.execute v (Aws.deleteEndpoint (ev^.evEndpoint))
            Just  a -> f u ep a

deleteEndpoint :: UserId -> Event -> Gundeck ()
deleteEndpoint u ev = do
    logEvent u ev $ msg (val "Deleting SNS endpoint")
    v <- view awsEnv
    Aws.execute v (Aws.deleteEndpoint (ev^.evEndpoint))

deleteToken :: UserId -> Event -> Token -> ClientId -> Gundeck ()
deleteToken u ev tk cl = do
    logEvent u ev $ "token" .= Text.take 16 (tokenText tk)
                 ~~ msg (val "Deleting push token")
    i <- mkNotificationId
    let t = mkPushToken ev tk cl
        p = singletonPayload (PushRemove t)
        n = Notification i False p
        r = singleton (target u & targetClients .~ [cl])
    void $ Web.push n r u Nothing Set.empty
    Stream.add i r p =<< view (options.notificationTTL)
    Data.delete u (t^.tokenTransport) (t^.tokenApp) tk

mkPushToken :: Event -> Token -> ClientId -> PushToken
mkPushToken ev tk cl = let t = ev^.evEndpoint.snsTopic in
    pushToken (t^.endpointTransport) (t^.endpointAppName) tk cl

invalidData :: MonadThrow m => String -> m a
invalidData m = throwM (ErrorCall $ "invalid endpoint data: " ++ m)

logEvent :: UserId -> Event -> (Msg -> Msg) -> Gundeck ()
logEvent u ev f = Log.info $
       "user"  .= toByteString u
    ~~ "arn"   .= toText (ev^.evEndpoint)
    ~~ "cause" .= toText (ev^.evType)
    ~~ f
