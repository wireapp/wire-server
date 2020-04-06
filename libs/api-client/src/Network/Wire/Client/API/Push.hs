{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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

module Network.Wire.Client.API.Push
  ( Notification (..),
    awaitNotifications,
    fetchNotifications,
    lastNotification,

    -- * Event Data
    Event (..),
    ConvEvent (..),
    SimpleMembers (..),
    UserIdList (..),
    OtrMessage (..),
    NoData,
    UserInfo (..),

    -- * Event Type
    EventType (..),
    eventType,
    eventTypeText,
    showEventType,
  )
where

import Bilge
import Brig.Types
import Control.Concurrent (myThreadId)
import Control.Concurrent.Async
import Control.Exception (bracket, finally, onException)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Default.Class
import qualified Data.HashMap.Strict as M
import Data.Id
import Data.List.NonEmpty
import Data.Text (pack)
import qualified Data.Text as T
import Data.Time.Clock
import Data.UUID (UUID, fromString)
import Galley.Types hiding (Event, EventType)
import Imports hiding (fromString)
import Network.Connection
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status hiding (statusCode)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS
import Network.Wire.Client.API.Auth
import Network.Wire.Client.HTTP
import Network.Wire.Client.Monad
import Network.Wire.Client.Session
-- FUTUREWORK: We can probably monadunliftio the Session monad somehow?
import qualified System.Logger as Log

-------------------------------------------------------------------------------

-- * Notifications

newtype NotifId = NotifId UUID deriving (Eq, Show)

data Notification
  = Notification
      { notifId :: NotifId,
        notifEvents :: [Event]
      }

awaitNotifications ::
  (MonadSession m, Functor m) =>
  -- TODO: Maybe ClientId
  (Notification -> IO ()) ->
  m (Async ())
awaitNotifications f = do
  s <- getServer
  l <- getLogger
  a <- access . authToken <$> getAuth
  let hst = C.unpack $ fromMaybe (serverHost s) (serverWsHost s)
  let prt = fromIntegral $ fromMaybe (serverPort s) (serverWsPort s)
  let pth = "/await?access_token=" ++ C.unpack (L.toStrict a)
  liftIO $ do
    latch <- newEmptyMVar
    worker <-
      async $
        do
          ctx <- initConnectionContext
          bracket (connectTo ctx (params hst prt)) connectionClose $ \con -> do
            when (serverSSL s) $
              connectionSetSecure ctx con def
            ws <- WS.makeStream (readChunk con) (writeChunk con)
            WS.runClientWithStream ws hst pth WS.defaultConnectionOptions [] $ \c ->
              putMVar latch () >> consume l c
          `onException` tryPutMVar latch ()
    takeMVar latch
    return worker
  where
    params h p = ConnectionParams h p Nothing Nothing
    consume l c = forever (WS.receiveData c >>= forward l) `finally` close c
    close c = WS.sendClose c ("close" :: ByteString)
    forward l b = do
      tid <- myThreadId
      Log.debug l $ Log.field "Thread" (show tid) . Log.msg b
      case eitherDecode b of
        Right event -> f event
        Left e -> Log.err l $ Log.msg ("parse-event: " ++ e)
    readChunk c = (\x -> if C.null x then Nothing else Just x) <$> connectionGetChunk c
    writeChunk c = maybe (return ()) (connectionPut c . L.toStrict)

fetchNotifications ::
  (MonadSession m, MonadThrow m) =>
  Maybe ByteString ->
  m (Bool, [Notification])
fetchNotifications snc = do
  rs <- sessionRequest req rsc consumeBody
  case statusCode rs of
    200 -> (True,) <$> responseJsonThrow (ParseError . pack) rs
    404 -> (False,) <$> responseJsonThrow (ParseError . pack) rs
    _ -> unexpected rs "fetch: status code"
  where
    req =
      method GET
        . paths ["notifications"]
        . acceptJson
        . maybe id (queryItem "since") snc
        $ empty
    rsc = status200 :| [status404]

lastNotification :: (MonadSession m, MonadThrow m) => m (Maybe Notification)
lastNotification = do
  rs <- sessionRequest req rsc consumeBody
  case statusCode rs of
    200 -> Just <$> responseJsonThrow (ParseError . pack) rs
    404 -> return Nothing
    _ -> unexpected rs "last: status code"
  where
    req =
      method GET
        . paths ["notifications", "last"]
        . acceptJson
        $ empty
    rsc = status200 :| [status404]

-------------------------------------------------------------------------------

-- * Event Data

data Event
  = -- User events
    EConnection UserConnection (Maybe UserInfo)
  | ENewUser User
  | -- TODO: EUserUpdate UserUpdate
    -- Conversation events
    EConvCreate (ConvEvent Conversation)
  | EMemberJoin (ConvEvent SimpleMembers)
  | EMemberLeave (ConvEvent UserIdList)
  | EConnect (ConvEvent Connect)
  | EConvRename (ConvEvent ConversationRename)
  | EMemberStateUpdate (ConvEvent MemberUpdate)
  | EOtrMessage (ConvEvent OtrMessage)

instance Show Event where
  show (EConnection x u) = "EConnection: " ++ show x ++ " with UserInfo: " ++ show u
  show (ENewUser x) = "ENewUser: " ++ show (userEmail x)
  show (EConvCreate x) = "EConvCreate: " ++ show x
  show (EMemberJoin x) = "EMemberJoin: " ++ show x
  show (EConnect x) = "EConnect: " ++ show x
  show (EMemberLeave x) = "EMemberLeave: " ++ show x
  show (EConvRename x) = "EConvRename: " ++ show x
  show (EMemberStateUpdate x) = "EMemberStateUpdate: " ++ show x
  show (EOtrMessage x) = "EOtrMessage: " ++ show x

-- | An event in a 'Conversation'.
data ConvEvent a
  = ConvEvent
      { convEvtConv :: !ConvId,
        convEvtFrom :: !UserId,
        convEvtTime :: !UTCTime,
        convEvtData :: !a
      }
  deriving (Eq, Show)

data NoData = NoData deriving (Show)

-- | Auxiliary user information attached to other events.
newtype UserInfo = UserInfo {uiName :: Name} deriving (Eq, Show)

-------------------------------------------------------------------------------

-- * Event Type

data EventType
  = TUserConnection
  | TUserNew
  | TConvCreate
  | TConvMemberJoin
  | TConvConnect
  | TConvMemberLeave
  | TConvRename
  | TConvMemberStateUpdate
  | TConvOtrMessageAdd
  deriving (Eq, Enum, Bounded)

instance Show EventType where
  show = T.unpack . eventTypeText

eventType :: Event -> EventType
eventType (ENewUser _) = TUserNew
eventType (EConnection _ _) = TUserConnection
eventType (EConvCreate _) = TConvCreate
eventType (EConnect _) = TConvConnect
eventType (EMemberJoin _) = TConvMemberJoin
eventType (EMemberLeave _) = TConvMemberLeave
eventType (EConvRename _) = TConvRename
eventType (EMemberStateUpdate _) = TConvMemberStateUpdate
eventType (EOtrMessage _) = TConvOtrMessageAdd

eventTypeText :: EventType -> Text
eventTypeText TUserNew = "user.new"
eventTypeText TUserConnection = "user.connection"
eventTypeText TConvCreate = "conversation.create"
eventTypeText TConvConnect = "conversation.connect-request"
eventTypeText TConvMemberJoin = "conversation.member-join"
eventTypeText TConvMemberLeave = "conversation.member-leave"
eventTypeText TConvRename = "conversation.rename"
eventTypeText TConvMemberStateUpdate = "conversation.member-state-update"
eventTypeText TConvOtrMessageAdd = "conversation.otr-message-add"

showEventType :: Event -> Text
showEventType = eventTypeText . eventType

-------------------------------------------------------------------------------
-- JSON decoding

parseEvent :: Object -> Text -> Parser Event
parseEvent o "user.connection" = EConnection <$> o .: "connection" <*> o .:? "user"
parseEvent o "user.new" = ENewUser <$> o .: "user"
parseEvent o "conversation.create" = EConvCreate <$> parseJSON (Object o)
parseEvent o "conversation.connect-request" = EConnect <$> parseJSON (Object o)
parseEvent o "conversation.otr-message-add" = EOtrMessage <$> parseJSON (Object o)
parseEvent o "conversation.member-join" = EMemberJoin <$> parseJSON (Object o)
parseEvent o "conversation.member-leave" = EMemberLeave <$> parseJSON (Object o)
parseEvent o "conversation.rename" = EConvRename <$> parseJSON (Object o)
parseEvent o "conversation.member-state-update" = EMemberStateUpdate <$> parseJSON (Object o)
parseEvent _ t = fail $ "Unknown event type: " ++ T.unpack t

instance FromJSON Event where
  parseJSON = withObject "event" $ \o ->
    case M.lookup "type" o of
      Just (String t) -> parseEvent o t
      Just _ -> fail "Event type is not a string"
      Nothing -> fail "Missing event type"

instance FromJSON NotifId where
  parseJSON =
    withText "notification-id" $
      maybe (fail "invalid uuid") (return . NotifId) . fromString . T.unpack

instance FromJSON Notification where
  parseJSON = withObject "notification" $ \o ->
    Notification <$> o .: "id" <*> o .: "payload"

instance FromJSON a => FromJSON (ConvEvent a) where
  parseJSON = withObject "conversation-event" $ \o ->
    ConvEvent <$> o .: "conversation"
      <*> o .: "from"
      <*> o .: "time"
      <*> o .: "data"

instance FromJSON NoData where
  parseJSON Null = return NoData
  parseJSON _ = fail "Unexpected event data. Expecting nothing/null."

instance FromJSON UserInfo where
  parseJSON = withObject "UserInfo" $ \o ->
    UserInfo <$> o .: "name"
