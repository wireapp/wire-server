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

data WSMessage (direction :: Direction)
  = EventMessage EventData
  | AckMessage AckData
  | PingMessage
  | PongMessage
  deriving (Show, Eq)

makePrisms ''WSMessage

-- | Only useful for writing the ToSchema instance for 'WSMessage'
data WSMessageType = MsgTypeEvent | MsgTypeAck | MsgTypePing | MsgTypePong
  deriving (Eq, Enum, Bounded)

msgTypeSchema :: Direction -> ValueSchema NamedSwaggerDoc WSMessageType
msgTypeSchema dir =
  enum @Text "WSMessageType" $
    mconcat $
      [element "event" MsgTypeEvent | dir == ServerToClient]
        <> [element "ack" MsgTypeAck | dir == ClientToServer]
        <> [ element "ping" MsgTypePing,
             element "pong" MsgTypePong
           ]

instance forall dir. (KnownDirection dir) => ToSchema (WSMessage dir) where
  schema =
    object "WSMessage" $
      fromTagged
        <$> toTagged
          .= bind
            (fst .= field "type" (msgTypeSchema $ directionVal @dir))
            (snd .= untaggedSchema)
    where
      toTagged :: WSMessage dir -> (WSMessageType, WSMessage dir)
      toTagged d@(EventMessage _) = (MsgTypeEvent, d)
      toTagged d@(AckMessage _) = (MsgTypeAck, d)
      toTagged d@PingMessage = (MsgTypePing, d)
      toTagged d@PongMessage = (MsgTypePong, d)

      fromTagged :: (WSMessageType, WSMessage dir) -> WSMessage dir
      fromTagged = snd

      untaggedSchema :: SchemaP SwaggerDoc (A.Object, WSMessageType) [A.Pair] (WSMessage dir) (WSMessage dir)
      untaggedSchema = dispatch $ \case
        MsgTypeEvent -> tag _EventMessage (id .= field "data" schema)
        MsgTypeAck -> tag _AckMessage (id .= field "data" schema)
        MsgTypePing -> tag _PingMessage (id .= pure ())
        MsgTypePong -> tag _PongMessage (id .= pure ())

deriving via Schema (WSMessage dir) instance (KnownDirection dir) => FromJSON (WSMessage dir)

deriving via Schema (WSMessage dir) instance (KnownDirection dir) => ToJSON (WSMessage dir)
