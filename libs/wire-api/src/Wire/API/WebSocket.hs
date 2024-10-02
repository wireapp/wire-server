{-# LANGUAGE TemplateHaskell #-}

-- TODO: Rename this module to something that is more specific than "websocket"
module Wire.API.WebSocket where

import Control.Lens (makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Schema
import Data.Word
import Imports
import Network.WebSockets

data AckData = AckData
  { deliveryTag :: Word64,
    -- | Acknowledge all deliveryTags <= 'deliveryTag', see RabbitMQ
    -- documenation:
    -- https://www.rabbitmq.com/docs/confirms#consumer-acks-multiple-parameter
    multiple :: Bool
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Schema AckData)

instance ToSchema AckData where
  schema =
    object "AckData" $
      AckData
        <$> (.deliveryTag) .= field "delivery_tag" schema
        <*> multiple .= field "multiple" schema

data EventData = EventData
  { payload :: [A.Object],
    deliveryTag :: Word64
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Schema EventData)

instance ToSchema EventData where
  schema =
    object "EventData" $
      EventData
        <$> payload .= field "payload" (array genericToSchema)
        <*> (.deliveryTag) .= field "delivery_tag" schema

data Direction = ServerToClient | ClientToServer
  deriving (Eq, Show)

class KnownDirection (k :: Direction) where
  directionVal :: Direction

instance KnownDirection 'ServerToClient where
  directionVal = ServerToClient

instance KnownDirection 'ClientToServer where
  directionVal = ClientToServer

data WSMessage (direction :: Direction) where
  EventMessage :: EventData -> WSMessage ServerToClient
  AckMessage :: AckData -> WSMessage ClientToServer
  PingMessage :: WSMessage direction
  PongMessage :: WSMessage direction

deriving instance Show (WSMessage dir)

deriving instance Eq (WSMessage dir)

makePrisms ''WSMessage

-- | Only useful for writing the ToSchema instance for 'WSMessage'
data WSMessageType (dir :: Direction) where
  MsgTypeEvent :: WSMessageType ServerToClient
  MsgTypeAck :: WSMessageType ClientToServer
  MsgTypePing :: WSMessageType dir
  MsgTypePong :: WSMessageType dir

deriving instance Show (WSMessageType dir)

deriving instance Eq (WSMessageType dir)

instance Enum (WSMessageType ServerToClient) where
  toEnum = \case
    0 -> MsgTypeEvent
    1 -> MsgTypePing
    2 -> MsgTypePong
    _ -> error "Enum out of bound"
  fromEnum = \case
    MsgTypeEvent -> 0
    MsgTypePing -> 1
    MsgTypePong -> 2

instance Bounded (WSMessageType ServerToClient) where
  minBound = MsgTypeEvent
  maxBound = MsgTypePong

instance Enum (WSMessageType ClientToServer) where
  toEnum = \case
    0 -> MsgTypeAck
    1 -> MsgTypePing
    2 -> MsgTypePong
    _ -> error "Enum out of bound"
  fromEnum = \case
    MsgTypeAck -> 0
    MsgTypePing -> 1
    MsgTypePong -> 2

instance Bounded (WSMessageType ClientToServer) where
  minBound = MsgTypeAck
  maxBound = MsgTypePong

instance ToSchema (WSMessageType ServerToClient) where
  schema =
    enum @Text "WSMessageType S2C" $
      mconcat
        [ element "event" MsgTypeEvent,
          element "ping" MsgTypePing,
          element "pong" MsgTypePong
        ]

instance ToSchema (WSMessageType ClientToServer) where
  schema =
    enum @Text "WSMessageType C2S" $
      mconcat
        [ element "ack" MsgTypeAck,
          element "ping" MsgTypePing,
          element "pong" MsgTypePong
        ]

instance forall (dir :: Direction). (ToSchema (WSMessageType dir), Bounded (WSMessageType dir), Enum (WSMessageType dir)) => ToSchema (WSMessage dir) where
  schema =
    object "WSMessage" $
      fromTagged
        <$> toTagged
          .= bind
            (fst .= field "type" (schema @(WSMessageType dir)))
            (snd .= untaggedSchema)
    where
      toTagged :: WSMessage dir -> (WSMessageType dir, WSMessage dir)
      toTagged d@(EventMessage _) = (MsgTypeEvent, d)
      toTagged d@(AckMessage _) = (MsgTypeAck, d)
      toTagged d@PingMessage = (MsgTypePing, d)
      toTagged d@PongMessage = (MsgTypePong, d)

      fromTagged :: (WSMessageType dir, WSMessage dir) -> WSMessage dir
      fromTagged = snd

      untaggedSchema :: SchemaP SwaggerDoc (A.Object, WSMessageType dir) [A.Pair] (WSMessage dir) (WSMessage dir)
      untaggedSchema = dispatch $ \case
        MsgTypeEvent -> tag _EventMessage (id .= field "data" schema)
        MsgTypeAck -> tag _AckMessage (id .= field "data" schema)
        MsgTypePing -> tag _PingMessage (id .= pure ())
        MsgTypePong -> tag _PongMessage (id .= pure ())

deriving via Schema (WSMessage dir) instance (ToSchema (WSMessageType dir), Bounded (WSMessageType dir), Enum (WSMessageType dir)) => FromJSON (WSMessage dir)

deriving via Schema (WSMessage dir) instance (ToSchema (WSMessageType dir), Bounded (WSMessageType dir), Enum (WSMessageType dir)) => ToJSON (WSMessage dir)
