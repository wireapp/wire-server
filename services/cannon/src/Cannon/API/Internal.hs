{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

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
import qualified Cannon.Dict as D
import Cannon.Types
import Cannon.WS
import Control.Monad.Catch
import Data.Aeson (encode)
import Data.Id hiding (client)
import Imports
import Network.WebSockets
import Servant
import Servant.Conduit ()
import System.Logger.Class (msg, val)
import qualified System.Logger.Class as LC
import Wire.API.Internal.BulkPush
import Wire.API.Internal.Notification
import Wire.API.RawJson
import qualified Wire.API.Routes.Internal.Cannon as Internal
import Wire.API.Routes.Named

internalServer :: ServerT Internal.API Cannon
internalServer =
  Named @"get-status" (pure ())
    :<|> Named @"push-notification" pushHandler
    :<|> Named @"bulk-push-notifications" bulkPushHandler
    :<|> Named @"check-presence" checkPresenceHandler

pushHandler :: UserId -> ConnId -> RawJson -> Cannon (Maybe ())
pushHandler user conn body =
  singlePush (rawJsonBytes body) (PushTarget user conn) >>= \case
    PushStatusOk -> pure $ Just ()
    PushStatusGone -> pure Nothing

-- | Take notification @n@ and send it to the 'PushTarget'.
singlePush :: (WebSocketsData a) => a -> PushTarget -> Cannon PushStatus
singlePush n (PushTarget usrid conid) = do
  let k = mkKey usrid conid
  d <- clients
  LC.debug $ client (key2bytes k) . msg (val "push")
  c <- D.lookup k d
  case c of
    Nothing -> do
      LC.debug $ client (key2bytes k) . msg (val "push: client gone")
      return PushStatusGone
    Just x -> do
      e <- wsenv
      runWS e $ do
        catchAll
          (runWS e (sendMsg n k x) >> pure PushStatusOk)
          (const (terminate k x >> pure PushStatusGone))

bulkPushHandler :: BulkPushRequest -> Cannon BulkPushResponse
bulkPushHandler (BulkPushRequest ns) =
  BulkPushResponse . mconcat . zipWith compileResp ns <$> (uncurry doNotify `Imports.mapM` ns)
  where
    doNotify :: Notification -> [PushTarget] -> Cannon [PushStatus]
    doNotify (encode -> notification) =
      mapConcurrentlyCannon (singlePush notification)
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
