{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Cannon.API.Internal
  ( InternalAPI,
    internalServer,
  )
where

import Cannon.App
import qualified Cannon.Dict as D
import Cannon.Types
import Cannon.WS
import Conduit
import Control.Monad.Catch
import Data.Aeson (encode)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Conduit.List
import Data.Id hiding (client)
import Gundeck.Types
import Gundeck.Types.BulkPush
import Imports
import Servant
import Servant.API.Verbs
import Servant.Conduit ()
import System.Logger.Class (msg, val)
import qualified System.Logger.Class as LC
import Wire.API.ErrorDescription
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named

newtype PushNotificationStream = PushNotificationStream
  { getPushNotificationStream :: ConduitT () ByteString (ResourceT WS) ()
  }
  deriving newtype (FromSourceIO ByteString)

type InternalAPI =
  "i"
    :> ( Named
           "get-status-plain-text"
           ( "status"
               :> Get '[PlainText] String
           )
           :<|> Named
                  "get-status-no-content"
                  ( "status"
                      :> HeadNoContent
                  )
           :<|> Named
                  "push-notification"
                  ( "push"
                      :> Capture "user" UserId
                      :> Capture "conn" ConnId
                      :> StreamBody NoFraming OctetStream (SourceIO ByteString)
                      :> MultiVerb
                           'POST
                           '[JSON]
                           '[ ClientGone,
                              RespondEmpty 200 "Successfully pushed."
                            ]
                           (Maybe ())
                  )
           :<|> Named
                  "bulk-push-notifications"
                  ( "bulkpush"
                      :> ReqBody '[JSON] BulkPushRequest
                      :> Post '[JSON] BulkPushResponse
                  )
           :<|> Named
                  "get-preferences"
                  ( "presences"
                      :> Capture "uid" UserId
                      :> Capture "conn" ConnId
                      :> MultiVerb
                           'HEAD
                           '[JSON]
                           '[ PresenceNotRegistered,
                              RespondEmpty 200 "Presence checked successfully."
                            ]
                           (Maybe ())
                  )
       )

internalServer :: ServerT InternalAPI Cannon
internalServer =
  Named @"get-status-plain-text" (pure "")
    :<|> Named @"get-status-no-content" (pure NoContent)
    :<|> Named @"push-notification" pushHandler
    :<|> Named @"bulk-push-notifications" bulkPushHandler
    :<|> Named @"get-preferences" checkPresenceHandler

pushHandler :: UserId -> ConnId -> SourceIO ByteString -> Cannon (Maybe ())
pushHandler user conn body =
  singlePush body (PushTarget user conn) >>= \case
    PushStatusOk -> pure $ Just ()
    PushStatusGone -> pure Nothing

-- | Take a serialized 'Notification' string and send it to the 'PushTarget'.
singlePush :: SourceIO ByteString -> PushTarget -> Cannon PushStatus
singlePush notification = singlePush' (getPushNotificationStream (fromSourceIO notification))

singlePush' :: ConduitM () ByteString (ResourceT WS) () -> PushTarget -> Cannon PushStatus
singlePush' notificationC (PushTarget usrid conid) = do
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
          ( runConduitRes $
              notificationC .| (sendMsgConduit k x >> pure PushStatusOk)
          )
          (const (terminate k x >> pure PushStatusGone))

bulkPushHandler :: BulkPushRequest -> Cannon BulkPushResponse
bulkPushHandler (BulkPushRequest ns) =
  BulkPushResponse . mconcat . zipWith compileResp ns <$> (uncurry doNotify `Imports.mapM` ns)
  where
    doNotify :: Notification -> [PushTarget] -> Cannon [PushStatus]
    doNotify (encode -> notification) =
      mapConcurrentlyCannon
        ( singlePush'
            (sourceLbs notification)
        )
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

sourceLbs :: Monad m => L.ByteString -> ConduitT i S.ByteString m ()
sourceLbs = sourceList . L.toChunks
