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

push :: NativePush -> [Address] -> Gundeck [Result]
push _    [] = return []
push m   [a] = pure <$> push1 m a
push m addrs = mapConcurrently (push1 m) addrs

push1 :: NativePush -> Address -> Gundeck Result
push1 m a = do
    e <- view awsEnv
    r <- Aws.execute e $ publish m a
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
        let t = a^.addrPushToken
        let p = singletonPayload (PushRemove t)
        Stream.add i r p =<< view (options.optSettings.setNotificationTTL)

publish :: NativePush -> Address -> Aws.Amazon Result
publish m a = flip catches pushException $ do
    let ept = a^.addrEndpoint
    txt <- liftIO $ serialise m a
    case txt of
        Left  f -> return $! Failure f a
        Right v -> toResult <$> Aws.publish ept v mempty
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
deleteTokens :: [Address] -> Maybe Address -> Gundeck ()
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
                              (a^.addrConn) (a^.addrClient)
                Data.delete u (a^.addrTransport) (a^.addrApp) oldTok

logError :: (Exception e, MonadLogger m) => Address -> Text -> e -> m ()
logError a m exn = Log.err $
       field "user" (toByteString (a^.addrUser))
    ~~ field "arn" (toText (a^.addrEndpoint))
    ~~ field "error" (show exn)
    ~~ msg m
