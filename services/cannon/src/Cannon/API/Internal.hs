-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Cannon.API.Internal
  ( sitemap,
  )
where

import Cannon.App
import qualified Cannon.Dict as D
import Cannon.Types
import Cannon.WS
import Control.Monad.Catch
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as L
import Data.Id (ConnId, UserId)
import Data.Swagger.Build.Api hiding (Response)
import Gundeck.Types
import Gundeck.Types.BulkPush
import Imports hiding (head)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate
import Network.Wai.Routing
import Network.Wai.Utilities
import Network.Wai.Utilities.Request (parseBody')
import Network.Wai.Utilities.Response (json)
import System.Logger.Class (msg, val)
import qualified System.Logger.Class as LC

sitemap :: Routes ApiBuilder Cannon ()
sitemap = do
  post "/i/push/:user/:conn" (continue pushH) $
    capture "user" .&. capture "conn" .&. request
  post "/i/bulkpush" (continue bulkpushH) $
    request
  head "/i/presences/:uid/:conn" (continue checkPresenceH) $
    param "uid" .&. param "conn"

pushH :: UserId ::: ConnId ::: Request -> Cannon Response
pushH (user ::: conn ::: req) =
  singlePush (readBody req) (PushTarget user conn) >>= \case
    PushStatusOk -> return empty
    PushStatusGone -> return $ errorRs status410 "general" "client gone"

-- | Parse the entire list of notifcations and targets, then call 'singlePush' on the each of them
-- in order.
bulkpushH :: Request -> Cannon Response
bulkpushH req = json <$> (parseBody' (JsonRequest req) >>= bulkpush)

-- | The typed part of 'bulkpush'.
bulkpush :: BulkPushRequest -> Cannon BulkPushResponse
bulkpush (BulkPushRequest notifs) =
  BulkPushResponse . mconcat . zipWith compileResp notifs <$> (uncurry doNotif `mapM` notifs)
  where
    doNotif :: Notification -> [PushTarget] -> Cannon [PushStatus]
    doNotif (pure . encode -> notif) = mapConcurrentlyCannon (singlePush notif)
    compileResp ::
      (Notification, [PushTarget]) ->
      [PushStatus] ->
      [(NotificationId, PushTarget, PushStatus)]
    compileResp (notif, prcs) pss = zip3 (repeat (ntfId notif)) prcs pss

-- | Take a serialized 'Notification' string and send it to the 'PushTarget'.
singlePush :: Cannon L.ByteString -> PushTarget -> Cannon PushStatus
singlePush notification (PushTarget usrid conid) = do
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
      b <- notification
      runWS e $
        (sendMsg b k x >> return PushStatusOk)
          `catchAll` const (terminate k x >> return PushStatusGone)

checkPresenceH :: UserId ::: ConnId -> Cannon Response
checkPresenceH (u ::: c) = do
  e <- wsenv
  registered <- runWS e $ isRemoteRegistered u c
  if registered
    then return empty
    else return $ errorRs status404 "not-found" "presence not registered"
