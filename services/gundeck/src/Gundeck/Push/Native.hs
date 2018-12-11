{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gundeck.Push.Native
    ( push
    , deleteTokens
    , module Types
    ) where

import Imports
import Control.Lens ((^.), view, (.~))
import Control.Monad.Catch
import Data.ByteString.Conversion.To
import Data.Id
import Data.List1
import Data.Metrics (path, counterIncr)
import Gundeck.Env
import Gundeck.Monad
import Gundeck.Options
import Gundeck.Push.Native.Serialise
import Gundeck.Push.Native.Types as Types
import Gundeck.Types
import Gundeck.Util
import Network.AWS.Data (toText)
import System.Logger.Class (MonadLogger, (~~), msg, val, field)
import UnliftIO (mapConcurrently, handleAny)

import qualified Data.Set                  as Set
import qualified Data.Text                 as Text
import qualified Data.UUID                 as UUID
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
    r <- Aws.execute e $ publish m a ttl
    case r of
        Success _                    -> do
            Log.debug $ field "user" (toByteString (a^.addrUser))
                ~~ field "arn" (toText (a^.addrEndpoint))
                ~~ Log.msg (val "Native push success")
            view monitor >>= counterIncr (path "push.native.success")
        Failure EndpointDisabled   _ -> onDisabled
        Failure PayloadTooLarge    _ -> onPayloadTooLarge
        Failure EndpointInvalid    _ -> onInvalidEndpoint
        Failure (PushException ex) _ -> do
            logError a "Native push failed" ex
            view monitor >>= counterIncr (path "push.native.errors")
    return r
  where
    -- TODO: REFACTOR: this smells like a bug that we've dragged along for a while: according to
    -- current logic (previous to this PR), 'Notice' is never transient.  but we have sent nothing
    -- but 'Notice' native notifications for a while.  have we sent out native notifications for
    -- transient messages at all?  that may have led to clients being woken up, pulling the event
    -- queue, and finding nothing in it (because we don't store transient notifications).
    --
    -- behavior as of this PR is the same as before: no 'Notice' values are ever considered
    -- transient, but transient 'Notification's are translated to 'Notice' and queued for native
    -- push (i think).  we should decided whether that's what we want in a separate PR.
    ttl :: Maybe Aws.Seconds
    ttl = Nothing

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

    onPayloadTooLarge = do
        view monitor >>= counterIncr (path "push.native.too_large")
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

    onTokenRemoved = do
        i <- mkNotificationId
        let c = a^.addrClient
        let r = singleton (target (a^.addrUser) & targetClients .~ [c])
        let t = pushToken (a^.addrTransport) (a^.addrApp) (a^.addrToken) c
        let p = singletonPayload (PushRemove t)
        Stream.add i r p =<< view (options.optSettings.setNotificationTTL)

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

-- | Delete a list of push addresses, optionally specifying as a cause
-- a newly created address in the second argument. If such a new address
-- is given, shared owners of the deleted tokens have their addresses
-- migrated to the token and endpoint of the new address.
deleteTokens :: [Address a] -> Maybe (Address a) -> Gundeck ()
deleteTokens tokens new = do
    aws <- view awsEnv
    forM_ tokens $ \a -> do
        Log.info $ field "user" (UUID.toASCIIBytes (toUUID (a^.addrUser)))
                ~~ field "token" (Text.take 16 (tokenText (a^.addrToken)))
                ~~ field "arn" (toText (a^.addrEndpoint))
                ~~ msg (val "Deleting push token")
        Data.delete (a^.addrUser) (a^.addrTransport) (a^.addrApp) (a^.addrToken)
        ept <- Aws.execute aws (Aws.lookupEndpoint (a^.addrEndpoint))
        for_ ept $ \ep ->
            let us = Set.delete (a^.addrUser) (ep^.Aws.endpointUsers)
            in if Set.null us
                then delete aws a
                else case new of
                    Nothing -> update aws a us
                    Just a' -> do
                        mapM_ (migrate a a') us
                        update aws a' (ep^.Aws.endpointUsers)
                        delete aws a
  where
    delete aws a = do
        Log.info $ field "user" (UUID.toASCIIBytes (toUUID (a^.addrUser)))
                ~~ field "arn" (toText (a^.addrEndpoint))
                ~~ msg (val "Deleting SNS endpoint")
        Aws.execute aws (Aws.deleteEndpoint (a^.addrEndpoint))

    update aws a us = do
        Log.info $ field "user" (UUID.toASCIIBytes (toUUID (a^.addrUser)))
                ~~ field "arn" (toText (a^.addrEndpoint))
                ~~ msg (val "Updating SNS endpoint")
        Aws.execute aws (Aws.updateEndpoint us (a^.addrToken) (a^.addrEndpoint))

    migrate a a' u = do
        let oldArn = a^.addrEndpoint
        let oldTok = a^.addrToken
        let newArn = a'^.addrEndpoint
        let newTok = a'^.addrToken
        xs <- Data.lookup u Data.Quorum
        forM_ xs $ \x ->
            when (x^.addrEndpoint == oldArn) $ do
                Data.insert u (a^.addrTransport) (a^.addrApp) newTok newArn
                              (a^.addrConn) (a^.addrClient) Nothing
                Data.delete u (a^.addrTransport) (a^.addrApp) oldTok

logError :: (Exception e, MonadLogger m) => Address s -> Text -> e -> m ()
logError a m exn = Log.err $
       field "user" (toByteString (a^.addrUser))
    ~~ field "arn" (toText (a^.addrEndpoint))
    ~~ field "error" (show exn)
    ~~ msg m
