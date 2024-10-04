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

data WSMessageServerToClient
  = EventMessage EventData
  | PingDownMessage
  | PongDownMessage
  deriving (Show, Eq)

makePrisms ''WSMessageServerToClient

data WSMessageClientToServer
  = AckMessage AckData
  | PingUpMessage
  | PongUpMessage
  deriving (Show, Eq)

makePrisms ''WSMessageClientToServer

----------------------------------------------------------------------
-- ServerToClient

-- | Local type, only needed for writing the ToSchema instance for 'WSMessage'.
data WSMessageTypeServerToClient = MsgTypeEvent | MsgTypePingDown | MsgTypePongDown
  deriving (Eq, Enum, Bounded)

msgTypeSchemaServerToClient :: ValueSchema NamedSwaggerDoc WSMessageTypeServerToClient
msgTypeSchemaServerToClient =
  enum @Text "WSMessageTypeServerToClient" $
    mconcat $
      [ element "event" MsgTypeEvent,
        element "ping" MsgTypePingDown,
        element "pong" MsgTypePongDown
      ]

instance ToSchema WSMessageServerToClient where
  schema =
    object "WSMessageServerToClient" $
      fromTagged <$> toTagged .= bind (fst .= field "type" msgTypeSchemaServerToClient) (snd .= untaggedSchema)
    where
      toTagged :: WSMessageServerToClient -> (WSMessageTypeServerToClient, WSMessageServerToClient)
      toTagged d@(EventMessage _) = (MsgTypeEvent, d)
      toTagged d@PingDownMessage = (MsgTypePingDown, d)
      toTagged d@PongDownMessage = (MsgTypePongDown, d)

      fromTagged :: (WSMessageTypeServerToClient, WSMessageServerToClient) -> WSMessageServerToClient
      fromTagged = snd

      untaggedSchema :: SchemaP SwaggerDoc (A.Object, WSMessageTypeServerToClient) [A.Pair] (WSMessageServerToClient) (WSMessageServerToClient)
      untaggedSchema = dispatch $ \case
        MsgTypeEvent -> tag _EventMessage (id .= field "data" schema)
        MsgTypePingDown -> tag _PingDownMessage (id .= pure ())
        MsgTypePongDown -> tag _PongDownMessage (id .= pure ())

deriving via Schema WSMessageServerToClient instance FromJSON WSMessageServerToClient

deriving via Schema WSMessageServerToClient instance ToJSON WSMessageServerToClient

----------------------------------------------------------------------
-- ClientToServer

-- | Local type, only needed for writing the ToSchema instance for 'WSMessage'.
data WSMessageTypeClientToServer = MsgTypeAck | MsgTypePingUp | MsgTypePongUp
  deriving (Eq, Enum, Bounded)

msgTypeSchemaClientToServer :: ValueSchema NamedSwaggerDoc WSMessageTypeClientToServer
msgTypeSchemaClientToServer =
  enum @Text "WSMessageTypeClientToServer" $
    mconcat $
      [ element "ack" MsgTypeAck,
        element "ping" MsgTypePingUp,
        element "pong" MsgTypePongUp
      ]

instance ToSchema WSMessageClientToServer where
  schema =
    object "WSMessageClientToServer" $
      fromTagged <$> toTagged .= bind (fst .= field "type" msgTypeSchemaClientToServer) (snd .= untaggedSchema)
    where
      toTagged :: WSMessageClientToServer -> (WSMessageTypeClientToServer, WSMessageClientToServer)
      toTagged d@(AckMessage _) = (MsgTypeAck, d)
      toTagged d@PingUpMessage = (MsgTypePingUp, d)
      toTagged d@PongUpMessage = (MsgTypePongUp, d)

      fromTagged :: (WSMessageTypeClientToServer, WSMessageClientToServer) -> WSMessageClientToServer
      fromTagged = snd

      untaggedSchema :: SchemaP SwaggerDoc (A.Object, WSMessageTypeClientToServer) [A.Pair] WSMessageClientToServer WSMessageClientToServer
      untaggedSchema = dispatch $ \case
        MsgTypeAck -> tag _AckMessage (id .= field "data" schema)
        MsgTypePingUp -> tag _PingUpMessage (id .= pure ())
        MsgTypePongUp -> tag _PongUpMessage (id .= pure ())

deriving via Schema WSMessageClientToServer instance FromJSON WSMessageClientToServer

deriving via Schema WSMessageClientToServer instance ToJSON WSMessageClientToServer
