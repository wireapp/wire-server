{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

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

module Cannon.API.Internal (internalServer) where

import Cannon.App
import Cannon.Dict qualified as D
import Cannon.Types
import Cannon.WS
import Control.Monad.Catch
import Data.Aeson (eitherDecode', encode)
import Data.Id
import Data.Text qualified as T
import Imports
import Servant
import Servant.Conduit ()
import System.Logger.Class (msg, val)
import System.Logger.Class qualified as LC
import Wire.API.Event.Transmit (transmitInternalNotification)
import Wire.API.Internal.BulkPush
import Wire.API.Internal.Notification
import Wire.API.RawJson
import Wire.API.Routes.Internal.Cannon qualified as Internal
import Wire.API.Routes.Named

internalServer :: ServerT Internal.API Cannon
internalServer =
  Named @"get-status" (pure ())
    :<|> Named @"push-notification" pushHandler
    :<|> Named @"bulk-push-notifications" bulkPushHandler
    :<|> Named @"check-presence" checkPresenceHandler

pushHandler :: UserId -> ConnId -> RawJson -> Cannon (Maybe ())
pushHandler user conn body =
  singlePush (eitherDecode' (rawJsonBytes body)) (PushTarget user conn) >>= \case
    PushStatusOk -> pure $ Just ()
    PushStatusGone -> pure Nothing

-- | Take notification @n@ and send it to the 'PushTarget', encoding it for
-- the target connection's API version.  A notification that is fully
-- filtered out for that version is skipped but still reported as
-- 'PushStatusOk' so gundeck neither falls back to native push nor treats the
-- client as gone.
singlePush :: Either String Notification -> PushTarget -> Cannon PushStatus
singlePush (Left err) (PushTarget usrid conid) = do
  -- fail closed: a body we cannot decode cannot be version-gated, but the
  -- drop must be observable (schema skew between gundeck and cannon would
  -- otherwise silently discard all legacy /await pushes).
  LC.err $
    client (key2bytes (mkKey usrid conid))
      . msg ("push: failed to decode notification: " <> T.pack err)
  pure PushStatusOk
singlePush (Right n) (PushTarget usrid conid) = do
  let k = mkKey usrid conid
  d <- clients
  LC.debug $ client (key2bytes k) . msg (val "push")
  c <- D.lookup k d
  case c of
    Nothing -> do
      LC.debug $ client (key2bytes k) . msg (val "push: client gone")
      pure PushStatusGone
    Just x -> case transmitInternalNotification (wsApiVersion x) n of
      Nothing -> pure PushStatusOk
      Just n' -> do
        e <- wsenv
        runWS e $ do
          catchAll
            (runWS e (sendMsg (encode n') k x) >> pure PushStatusOk)
            (const (terminate k x >> pure PushStatusGone))

bulkPushHandler :: BulkPushRequest -> Cannon BulkPushResponse
bulkPushHandler (BulkPushRequest ns) =
  BulkPushResponse . mconcat . zipWith compileResp ns <$> (uncurry doNotify `Imports.mapM` ns)
  where
    doNotify :: Notification -> [PushTarget] -> Cannon [PushStatus]
    doNotify notif =
      mapConcurrentlyCannon (singlePush (Right notif))
    compileResp ::
      (Notification, [PushTarget]) ->
      [PushStatus] ->
      [(NotificationId, PushTarget, PushStatus)]
    compileResp (notif, prcs) pss = zip3 (repeat (ntfId notif)) prcs pss

checkPresenceHandler :: UserId -> ConnId -> Cannon (Maybe ())
checkPresenceHandler u c = do
  e <- wsenv
  registered <- runWS e $ isRemoteRegistered u c
  if registered
    then pure $ Just ()
    else pure Nothing
