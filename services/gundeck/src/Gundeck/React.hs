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

-- | Curate native push tokens based on provider feedback reported
-- via SNS events.
module Gundeck.React
  ( onEvent,
  )
where

import Control.Lens (view, (.~), (^.))
import Data.ByteString.Conversion
import Data.Id (ClientId, UserId)
import Data.List qualified as List
import Data.List1
import Data.Set qualified as Set
import Data.Text qualified as Text
import Gundeck.Aws (SNSEndpoint, endpointEnabled, endpointToken, endpointUsers)
import Gundeck.Aws qualified as Aws
import Gundeck.Aws.Arn
import Gundeck.Aws.Sns
import Gundeck.Env
import Gundeck.Instances ()
import Gundeck.Monad
import Gundeck.Notification.Data qualified as Stream
import Gundeck.Options (notificationTTL, settings)
import Gundeck.Push.Data qualified as Push
import Gundeck.Push.Native.Types
import Gundeck.Push.Websocket qualified as Web
import Imports
import System.Logger.Class (Msg, msg, val, (+++), (.=), (~~))
import System.Logger.Class qualified as Log
import Wire.API.Event.Gundeck
import Wire.API.Internal.Notification
import Wire.API.Notification (mkNotificationId)
import Wire.API.Push.V2

onEvent :: Event -> Gundeck ()
onEvent ev = case ev ^. evType of
  EndpointUpdated -> onUpdated ev
  DeliveryFailure DeliveryInvalidToken -> onPermFailure ev
  DeliveryFailure DeliveryEndpointDisabled -> onFailure ev
  DeliveryFailure DeliveryFailedPerm -> onPermFailure ev
  DeliveryFailure DeliveryTTLExpired -> onTTLExpired ev
  DeliveryFailure (DeliveryUnknownFailure r) -> onUnknownFailure ev r
  _ -> onUnhandledEventType ev

onUpdated :: Event -> Gundeck ()
onUpdated ev = withEndpoint ev $ \e as ->
  if not (e ^. endpointEnabled)
    then do
      forM_ as $ \a -> do
        logUserEvent (a ^. addrUser) ev $ msg (val "Removing disabled token")
        deleteToken (a ^. addrUser) ev (a ^. addrToken) (a ^. addrClient)
      deleteEndpoint ev
    else do
      -- Note: The assumption is always that if the tokens differ, then
      --       the token in the SNS endpoint has been updated due to provider
      --       feedback (e.g. GCM canonical IDs) and the new token must
      --       already have been registered by the client. Hence the
      --       existing tokens are considered superseded and removed.
      --       This is crucial for proper token curation.
      let (sup, cur) = List.partition (\a -> e ^. endpointToken /= a ^. addrToken) as
      forM_ sup $ \a -> do
        logUserEvent (a ^. addrUser) ev $ msg (val "Removing superseded token")
        deleteToken (a ^. addrUser) ev (a ^. addrToken) (a ^. addrClient)
      if
        | null sup -> pure ()
        | null cur -> deleteEndpoint ev
        | otherwise -> updateEndpoint ev e (map (view addrUser) cur)

onFailure :: Event -> Gundeck ()
onFailure ev = withEndpoint ev $ \e as ->
  unless (e ^. endpointEnabled) $ do
    forM_ as $ \a -> do
      logUserEvent (a ^. addrUser) ev $ msg (val "Removing disabled token")
      deleteToken (a ^. addrUser) ev (a ^. addrToken) (a ^. addrClient)
    deleteEndpoint ev

onPermFailure :: Event -> Gundeck ()
onPermFailure ev = withEndpoint ev $ \_ as -> do
  forM_ as $ \a -> do
    logUserEvent (a ^. addrUser) ev $ msg (val "Removing invalid token")
    deleteToken (a ^. addrUser) ev (a ^. addrToken) (a ^. addrClient)
  deleteEndpoint ev

onTTLExpired :: Event -> Gundeck ()
onTTLExpired ev =
  Log.warn $
    "arn"
      .= toText (ev ^. evEndpoint)
      ~~ "cause"
      .= toText (ev ^. evType)
      ~~ msg (val "Notification TTL expired")

onUnknownFailure :: Event -> Text -> Gundeck ()
onUnknownFailure ev r =
  Log.warn $
    "arn"
      .= toText (ev ^. evEndpoint)
      ~~ "cause"
      .= toText (ev ^. evType)
      ~~ msg (val "Unknown failure, reason: " +++ r)

onUnhandledEventType :: Event -> Gundeck ()
onUnhandledEventType ev =
  Log.warn $
    "arn"
      .= toText (ev ^. evEndpoint)
      ~~ "cause"
      .= toText (ev ^. evType)
      ~~ msg (val "Unhandled event type")

-------------------------------------------------------------------------------
-- Utilities

withEndpoint ::
  Event ->
  (SNSEndpoint -> [Address] -> Gundeck ()) ->
  Gundeck ()
withEndpoint ev f = do
  v <- view awsEnv
  e <- Aws.execute v (Aws.lookupEndpoint (ev ^. evEndpoint))
  for_ e $ \ep -> do
    let us = Set.toList (ep ^. endpointUsers)
    as <- concat <$> mapM (`Push.lookup` Push.LocalQuorum) us
    case filter ((== (ev ^. evEndpoint)) . view addrEndpoint) as of
      [] -> do
        logEvent ev $
          "token"
            .= Text.take 16 (tokenText (ep ^. endpointToken))
            ~~ msg (val "Deleting orphaned SNS endpoint")
        Aws.execute v (Aws.deleteEndpoint (ev ^. evEndpoint))
      as' -> f ep as'

deleteEndpoint :: Event -> Gundeck ()
deleteEndpoint ev = do
  logEvent ev $ msg (val "Deleting SNS endpoint")
  v <- view awsEnv
  Aws.execute v (Aws.deleteEndpoint (ev ^. evEndpoint))

updateEndpoint :: Event -> SNSEndpoint -> [UserId] -> Gundeck ()
updateEndpoint ev ep us = do
  logEvent ev $ msg (val "Updating SNS endpoint")
  v <- view awsEnv
  Aws.execute v (Aws.updateEndpoint (Set.fromList us) (ep ^. endpointToken) (ev ^. evEndpoint))

deleteToken :: UserId -> Event -> Token -> ClientId -> Gundeck ()
deleteToken u ev tk cl = do
  logUserEvent u ev $
    "token"
      .= Text.take 16 (tokenText tk)
      ~~ msg (val "Deleting push token")
  i <- mkNotificationId
  let t = mkPushToken ev tk cl
      p = singletonPayload (PushRemove t)
      n = Notification i False p
      r = singleton (target u & targetClients .~ [cl])
  void $ Web.push n r (Just u) Nothing Set.empty
  Stream.add i r p =<< view (options . settings . notificationTTL)
  Push.delete u (t ^. tokenTransport) (t ^. tokenApp) tk

mkPushToken :: Event -> Token -> ClientId -> PushToken
mkPushToken ev tk cl =
  let t = ev ^. evEndpoint . snsTopic
   in pushToken (t ^. endpointTransport) (t ^. endpointAppName) tk cl

logEvent :: Event -> (Msg -> Msg) -> Gundeck ()
logEvent ev f =
  Log.info $
    "arn"
      .= toText (ev ^. evEndpoint)
      ~~ "cause"
      .= toText (ev ^. evType)
      ~~ f

logUserEvent :: UserId -> Event -> (Msg -> Msg) -> Gundeck ()
logUserEvent u ev f =
  logEvent ev $
    "user"
      .= toByteString u
      ~~ f
