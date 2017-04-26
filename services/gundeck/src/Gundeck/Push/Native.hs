{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gundeck.Push.Native
    ( push
    , module Types
    ) where

import Control.Concurrent.Async.Lifted.Safe
import Control.Exception (SomeAsyncException)
import Control.Exception.Enclosed (handleAny)
import Control.Lens ((^.), view, (&), (.~))
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ByteString.Conversion.To
import Data.List1
import Data.Maybe (isJust)
import Data.Metrics (path, counterIncr)
import Data.Text (Text)
import Gundeck.Env
import Gundeck.Monad
import Gundeck.Options (notificationTTL)
import Gundeck.Push.Native.Serialise
import Gundeck.Push.Native.Types as Types
import Gundeck.Types
import Gundeck.Util
import Network.AWS.Data (toText)
import System.Logger.Class (MonadLogger, (~~), msg, val, field)

import qualified Gundeck.Aws               as Aws
import qualified Gundeck.Notification.Data as Stream
import qualified Gundeck.Push.Data         as Data
import qualified System.Logger.Class       as Log

push :: Message s -> [Address s] -> Gundeck [Result s]
push _    [] = return []
push m   [a] = pure <$> push1 m a
push m addrs = mapConcurrently (push1 m) addrs

push1 :: Message s -> Address s -> Gundeck (Result s)
push1 m a = do
    e <- view awsEnv
    r <- Aws.execute e (publish m a ttl)
    case r of
        Success _                    -> do
            Log.debug $ field "user" (toByteString (a^.addrUser))
                ~~ field "arn" (toText (a^.addrEndpoint))
                ~~ Log.msg (val "Native push success")
            view monitor >>= counterIncr (path "push.native.success")
        Failure EndpointDisabled   _ -> onDisabled
        Failure PayloadTooLarge    _ -> onPayloadTooLarge
        Failure EndpointInvalid    _ -> onInvalidEndpoint
        Failure MissingKeys        _ -> onMissingKeys
        Failure (PushException ex) _ -> do
            logError a "Native push failed" ex
            view monitor >>= counterIncr (path "push.native.errors")
    return r
  where
    -- Transient notifications as well as those with a fallback
    -- transport must be delivered "now or never".
    ttl | msgTransient m || hasFallback = Just (Aws.Seconds 0)
        | otherwise                     = Nothing

    hasFallback = isJust (a^.addrFallback)

    onDisabled =
        handleAny (logError a "Failed to cleanup disabled endpoint") $ do
            Log.info $ field "user"  (toByteString (a^.addrUser))
                    ~~ field "arn"   (toText (a^.addrEndpoint))
                    ~~ field "cause" ("EndpointDisabled" :: Text)
                    ~~ msg (val "Removing disabled endpoint and token")
            view monitor >>= counterIncr (path "push.native.disabled")
            Data.delete (a^.addrUser) (a^.addrTransport) (a^.addrApp) (a^.addrToken)
            onTokenRemoved
            e <- view awsEnv
            Aws.execute e (Aws.deleteEndpoint (a^.addrEndpoint))

    onPayloadTooLarge =
        Log.warn $ field "user" (toByteString (a^.addrUser))
                ~~ field "arn" (toText (a^.addrEndpoint))
                ~~ msg (val "Payload too large")

    onInvalidEndpoint =
        handleAny (logError a "Failed to cleanup orphaned push token") $ do
            Log.warn $ field "user"  (toByteString (a^.addrUser))
                    ~~ field "arn"   (toText (a^.addrEndpoint))
                    ~~ field "cause" ("InvalidEndpoint" :: Text)
                    ~~ msg (val "Invalid ARN. Deleting orphaned push token")
            view monitor >>= counterIncr (path "push.native.invalid")
            Data.delete (a^.addrUser) (a^.addrTransport) (a^.addrApp) (a^.addrToken)
            onTokenRemoved

    onMissingKeys =
        Log.warn $ field "user" (toByteString (a^.addrUser))
                ~~ field "arn" (toText (a^.addrEndpoint))
                ~~ msg (val "Missing signaling keys")

    onTokenRemoved = do
        i <- mkNotificationId
        let c = a^.addrClient
        let r = singleton (target (a^.addrUser) & targetClients .~ [c])
        let t = pushToken (a^.addrTransport) (a^.addrApp) (a^.addrToken) c
        let p = singletonPayload (PushRemove t)
        Stream.add i r p =<< view (options.notificationTTL)

publish :: Message s -> Address s -> Maybe Aws.Seconds -> Aws.Amazon (Result s)
publish m a t = flip catches pushException $ do
    let ept = a^.addrEndpoint
    let ttl = maybe mempty (Aws.timeToLive (a^.addrTransport)) t
    txt <- liftIO $ serialise m a
    case txt of
        Left  f -> return $! Failure f a
        Right v -> toResult <$> Aws.publish ept v ttl
  where
    toResult (Left  (Aws.EndpointDisabled _)) = Failure EndpointDisabled a
    toResult (Left  (Aws.PayloadTooLarge  _)) = Failure PayloadTooLarge  a
    toResult (Left  (Aws.InvalidEndpoint  _)) = Failure EndpointInvalid  a
    toResult (Right ())                       = Success a

    pushException =
        [ Handler (\(ex :: SomeAsyncException) -> throwM ex)
        , Handler (\(ex ::      SomeException) ->
            return (Failure (PushException ex) a))
        ]

logError :: (Exception e, MonadLogger m) => Address s -> Text -> e -> m ()
logError a m exn = Log.err $
       field "user" (toByteString (a^.addrUser))
    ~~ field "arn" (toText (a^.addrEndpoint))
    ~~ field "error" (show exn)
    ~~ msg m
