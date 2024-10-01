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

module Gundeck.Push.Native
  ( push,
    deleteTokens,
    module Types,
  )
where

import Amazonka.Data (toText)
import Control.Lens (view, (.~), (^.))
import Control.Monad.Catch
import Data.ByteString.Conversion.To
import Data.Id
import Data.List1
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.UUID qualified as UUID
import Gundeck.Aws qualified as Aws
import Gundeck.Env
import Gundeck.Monad
import Gundeck.Notification.Data qualified as Stream
import Gundeck.Options
import Gundeck.Push.Data qualified as Data
import Gundeck.Push.Native.Serialise
import Gundeck.Push.Native.Types as Types
import Gundeck.Util
import Imports
import Prometheus qualified as Prom
import System.Logger.Class (MonadLogger, field, msg, val, (.=), (~~))
import System.Logger.Class qualified as Log
import UnliftIO (handleAny, mapConcurrently, pooledMapConcurrentlyN_)
import Wire.API.Event.Gundeck
import Wire.API.Internal.Notification
import Wire.API.Push.V2

push :: NativePush -> [Address] -> Gundeck ()
push _ [] = pure ()
push m [a] = push1 m a
push m addrs = do
  perPushConcurrency <- view (options . settings . perNativePushConcurrency)
  case perPushConcurrency of
    -- send all at once
    Nothing -> void $ mapConcurrently (push1 m) addrs
    -- avoid high amounts of fresh parallel network requests by
    -- parallelizing only chunkSize native pushes at a time
    Just chunkSize -> pooledMapConcurrentlyN_ chunkSize (push1 m) addrs

{-# NOINLINE nativePushSuccessCounter #-}
nativePushSuccessCounter :: Prom.Counter
nativePushSuccessCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "push_native_success",
          Prom.metricHelp = "Number of times native pushes were successfully pushed"
        }

{-# NOINLINE nativePushDisabledCounter #-}
nativePushDisabledCounter :: Prom.Counter
nativePushDisabledCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "push_native_disabled",
          Prom.metricHelp = "Number of times native pushes were not pushed due to a disabled endpoint"
        }

{-# NOINLINE nativePushInvalidCounter #-}
nativePushInvalidCounter :: Prom.Counter
nativePushInvalidCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "push_native_invalid",
          Prom.metricHelp = "Number of times native pushes were not pushed due to an invalid endpoint"
        }

{-# NOINLINE nativePushTooLargeCounter #-}
nativePushTooLargeCounter :: Prom.Counter
nativePushTooLargeCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "push_native_too_large",
          Prom.metricHelp = "Number of times native pushes were not pushed due to payload being too large"
        }

{-# NOINLINE nativePushUnauthorizedCounter #-}
nativePushUnauthorizedCounter :: Prom.Counter
nativePushUnauthorizedCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "push_native_unauthorized",
          Prom.metricHelp = "Number of times native pushes were not pushed due to an unauthorized endpoint"
        }

{-# NOINLINE nativePushErrorCounter #-}
nativePushErrorCounter :: Prom.Counter
nativePushErrorCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "push_native_errors",
          Prom.metricHelp = "Number of times native pushes were not pushed due to an unexpected error"
        }

push1 :: NativePush -> Address -> Gundeck ()
push1 = push1' 0
  where
    push1' :: Int -> NativePush -> Address -> Gundeck ()
    push1' n m a =
      if n > retryUnauthorisedThreshold
        then onPersistentlyUnauthorisedEndpoint
        else do
          e <- view awsEnv
          r <- Aws.execute e $ publish m a
          case r of
            Success _ -> onSuccess
            Failure EndpointDisabled _ -> onDisabled
            Failure PayloadTooLarge _ -> onPayloadTooLarge
            Failure EndpointInvalid _ -> onInvalidEndpoint
            Failure EndpointUnauthorised _ ->
              if n < retryUnauthorisedThreshold
                then onUnauthorisedEndpoint
                else onPersistentlyUnauthorisedEndpoint
            Failure (PushException ex) _ -> onPushException ex
      where
        onSuccess = do
          Log.debug $
            field "user" (toByteString (a ^. addrUser))
              ~~ field "notificationId" (toText (npNotificationid m))
              ~~ Log.msg (val "Native push success")
          Prom.incCounter nativePushSuccessCounter
        onDisabled =
          handleAny (logError a "Failed to cleanup disabled endpoint") $ do
            Log.info $
              field "user" (toByteString (a ^. addrUser))
                ~~ field "arn" (toText (a ^. addrEndpoint))
                ~~ field "cause" ("EndpointDisabled" :: Text)
                ~~ msg (val "Removing disabled endpoint and token")
            Prom.incCounter nativePushDisabledCounter
            Data.delete (a ^. addrUser) (a ^. addrTransport) (a ^. addrApp) (a ^. addrToken)
            onTokenRemoved
            e <- view awsEnv
            Aws.execute e (Aws.deleteEndpoint (a ^. addrEndpoint))
        onPayloadTooLarge = do
          Prom.incCounter nativePushTooLargeCounter
          Log.warn $
            field "user" (toByteString (a ^. addrUser))
              ~~ field "arn" (toText (a ^. addrEndpoint))
              ~~ msg (val "Payload too large")
        onInvalidEndpoint =
          handleAny (logError a "Failed to cleanup orphaned push token") $ do
            Log.warn $
              field "user" (toByteString (a ^. addrUser))
                ~~ field "arn" (toText (a ^. addrEndpoint))
                ~~ field "cause" ("InvalidEndpoint" :: Text)
                ~~ msg (val "Invalid ARN. Deleting orphaned push token")
            Prom.incCounter nativePushInvalidCounter
            Data.delete (a ^. addrUser) (a ^. addrTransport) (a ^. addrApp) (a ^. addrToken)
            onTokenRemoved
        retryUnauthorisedThreshold = 1
        onUnauthorisedEndpoint = do
          -- try to recreate ARN (cf. Gundeck.Push.addToken.create)
          let uid = a ^. addrUser
          let t = a ^. addrPushToken
          let trp = t ^. tokenTransport
          let app = t ^. tokenApp
          let tok = t ^. token
          env <- view (options . aws . arnEnv)
          aws' <- view awsEnv
          ept <- Aws.execute aws' (Aws.createEndpoint uid trp env app tok)
          case ept of
            Left (Aws.EndpointInUse arn) ->
              Log.info $ "arn" .= toText arn ~~ msg (val "ARN in use")
            Left (Aws.AppNotFound app') ->
              Log.info $ msg ("Push token of unknown application: '" <> appNameText app' <> "'")
            Left (Aws.InvalidToken _) ->
              Log.info $
                "token"
                  .= tokenText tok
                  ~~ msg (val "Invalid push token.")
            Left (Aws.TokenTooLong l) ->
              Log.info $ msg ("Push token is too long: token length = " ++ show l)
            Right arn -> do
              Data.updateArn uid trp app tok arn
              push1' (succ n) m (a & addrEndpoint .~ arn) -- try to send the push message with the new ARN
        onPersistentlyUnauthorisedEndpoint = handleAny (logError a "Found orphaned push token") $ do
          Log.warn $
            field "user" (toByteString (a ^. addrUser))
              ~~ field "arn" (toText (a ^. addrEndpoint))
              ~~ field "cause" ("UnauthorisedEndpoint" :: Text)
              ~~ msg (val "Invalid ARN. Dropping push message.")
          Prom.incCounter nativePushUnauthorizedCounter
        onPushException ex = do
          logError a "Native push failed" ex
          Prom.incCounter nativePushErrorCounter
        onTokenRemoved = do
          i <- mkNotificationId
          let c = a ^. addrClient
          let r = singleton (target (a ^. addrUser) & targetClients .~ [c])
          let t = a ^. addrPushToken
          let p = singletonPayload (PushRemove t)
          Stream.add i r p =<< view (options . settings . notificationTTL)

publish :: NativePush -> Address -> Aws.Amazon Result
publish m a = flip catches pushException $ do
  let ept = a ^. addrEndpoint
      uid = a ^. addrUser
      transp = a ^. addrTransport
      txt = serialise m uid transp
  Log.debug $
    field "user" (toByteString (a ^. addrUser))
      ~~ field "arn" (toText (a ^. addrEndpoint))
      ~~ field "notificationId" (toText (npNotificationid m))
      ~~ field "prio" (show (npPriority m))
      ~~ Log.msg (val "Native push")
  case txt of
    Left f -> pure $! Failure f a
    Right v -> toResult <$> Aws.publish ept v mempty
  where
    toResult (Left (Aws.EndpointDisabled _)) = Failure EndpointDisabled a
    toResult (Left (Aws.PayloadTooLarge _)) = Failure PayloadTooLarge a
    toResult (Left (Aws.InvalidEndpoint _)) = Failure EndpointInvalid a
    toResult (Left (Aws.UnauthorisedEndpoint _)) = Failure EndpointUnauthorised a
    toResult (Right ()) = Success a
    pushException =
      [ Handler (\(ex :: SomeAsyncException) -> throwM ex),
        Handler
          ( \(ex :: SomeException) ->
              pure (Failure (PushException ex) a)
          )
      ]

-- | Delete a list of push addresses, optionally specifying as a cause
-- a newly created address in the second argument. If such a new address
-- is given, shared owners of the deleted tokens have their addresses
-- migrated to the token and endpoint of the new address.
deleteTokens :: [Address] -> Maybe Address -> Gundeck ()
deleteTokens tokens new = do
  aws' <- view awsEnv
  forM_ tokens $ \a -> do
    Log.info $
      field "user" (UUID.toASCIIBytes (toUUID (a ^. addrUser)))
        ~~ field "token" (Text.take 16 (tokenText (a ^. addrToken)))
        ~~ field "arn" (toText (a ^. addrEndpoint))
        ~~ msg (val "Deleting push token")
    Data.delete (a ^. addrUser) (a ^. addrTransport) (a ^. addrApp) (a ^. addrToken)
    ept <- Aws.execute aws' (Aws.lookupEndpoint (a ^. addrEndpoint))
    for_ ept $ \ep ->
      let us = Set.delete (a ^. addrUser) (ep ^. Aws.endpointUsers)
       in if Set.null us
            then delete aws' a
            else case new of
              Nothing -> update aws' a us
              Just a' -> do
                mapM_ (migrate a a') us
                update aws' a' (ep ^. Aws.endpointUsers)
                delete aws' a
  where
    delete aws' a = do
      Log.info $
        field "user" (UUID.toASCIIBytes (toUUID (a ^. addrUser)))
          ~~ field "arn" (toText (a ^. addrEndpoint))
          ~~ msg (val "Deleting SNS endpoint")
      Aws.execute aws' (Aws.deleteEndpoint (a ^. addrEndpoint))
    update aws' a us = do
      Log.info $
        field "user" (UUID.toASCIIBytes (toUUID (a ^. addrUser)))
          ~~ field "arn" (toText (a ^. addrEndpoint))
          ~~ msg (val "Updating SNS endpoint")
      Aws.execute aws' (Aws.updateEndpoint us (a ^. addrToken) (a ^. addrEndpoint))
    migrate a a' u = do
      let oldArn = a ^. addrEndpoint
      let oldTok = a ^. addrToken
      let newArn = a' ^. addrEndpoint
      let newTok = a' ^. addrToken
      xs <- Data.lookup u Data.LocalQuorum
      forM_ xs $ \x ->
        when (x ^. addrEndpoint == oldArn) $ do
          Data.insert
            u
            (a ^. addrTransport)
            (a ^. addrApp)
            newTok
            newArn
            (a ^. addrConn)
            (a ^. addrClient)
          Data.delete u (a ^. addrTransport) (a ^. addrApp) oldTok

logError :: (Exception e, MonadLogger m) => Address -> Text -> e -> m ()
logError a m exn =
  Log.err $
    field "user" (toByteString (a ^. addrUser))
      ~~ field "arn" (toText (a ^. addrEndpoint))
      ~~ field "error" (show exn)
      ~~ msg m
