{-# LANGUAGE TemplateHaskell #-}

module FanInPerf.NotificationsStore where

import Data.Aeson
import Data.Id
import Data.List.NonEmpty qualified as NE
import Data.Qualified
import Data.UUID (UUID)
import Imports
import Polysemy
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.Message
import Wire.API.Push.V2 (ApsData, Route)

data FanInPush = FanInPush
  { conn :: Maybe ConnId,
    transient :: Bool,
    route :: Route,
    nativePriority :: Maybe Priority,
    origin :: Maybe UserId,
    targets :: [Target], -- replaces the previous 'recipients'
    json :: Object,
    apsData :: Maybe ApsData,
    isCellsEvent :: Bool
  }

data Target
  = TargetUser !UserId
  | TargetUserClients !(UserId, NE.NonEmpty ClientId)
  | TargetTeam !TeamId -- All users of a team
  | TargetEpoch !(GroupId, Epoch) -- All users in an epoch
  | TargetConnections !(Qualified UserId) -- All users connected to this user
  deriving (Eq)

-- | One notification stream, i.e. one key of a notification table.
data Stream
  = StreamUser !UserId
  | StreamClient !UserId !ClientId
  | StreamTeam !TeamId
  | StreamEpoch !GroupId !Epoch
  deriving (Eq, Show)

data FanInNotificationsStore m a where
  PushViaFanIn :: FanInPush -> FanInNotificationsStore m ()
  -- | Notifications of a stream with an id greater than the given one, oldest
  -- first, at most the given number.
  FetchNotifications :: Stream -> UUID -> Int -> FanInNotificationsStore m [(UUID, Value)]
  -- | Id of the latest notification ever pushed to the stream.
  GetLastNotificationId :: Stream -> FanInNotificationsStore m (Maybe UUID)
  -- | RFC "when nothing is acknowledged": sets the ack of the client (user id,
  -- client id) for the stream to the stream's latest notification and returns
  -- it. 'Nothing': nothing was ever pushed to the stream; no ack row is written.
  InitAcks :: UserId -> ClientId -> Stream -> FanInNotificationsStore m (Maybe UUID)
  -- | Raises the client's ack of the stream to the id; never lowers it.
  AckNotification :: UserId -> ClientId -> Stream -> UUID -> FanInNotificationsStore m ()

makeSem ''FanInNotificationsStore
