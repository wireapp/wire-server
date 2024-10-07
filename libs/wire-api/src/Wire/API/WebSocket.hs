{-# LANGUAGE TemplateHaskell #-}

-- TODO: Rename this module to something that is more specific than "websocket".
-- - "Wire.API.Event.WebSocket"?
-- - "Wire.API.Event.ClientChan"?
-- - "Wire.API.Event.APIChan"?  or "ApiChan"?
-- - "MessageBus"?  "Bus"?
module Wire.API.WebSocket where

import Control.Lens (makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Schema
import Data.Word
import Imports
import Wire.Arbitrary

data AckData = AckData
  { deliveryTag :: Word64,
    -- | Acknowledge all deliveryTags <= 'deliveryTag', see RabbitMQ
    -- documenation:
    -- https://www.rabbitmq.com/docs/confirms#consumer-acks-multiple-parameter
    multiple :: Bool
  }
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform AckData)
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
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform EventData)
  deriving (FromJSON, ToJSON) via (Schema EventData)

instance ToSchema EventData where
  schema =
    object "EventData" $
      EventData
        <$> payload .= field "payload" (array genericToSchema)
        <*> (.deliveryTag) .= field "delivery_tag" schema

data MessageServerToClient
  = EventMessage EventData
  | PingDownMessage
  | PongDownMessage
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform MessageServerToClient)

makePrisms ''MessageServerToClient

data MessageClientToServer
  = AckMessage AckData
  | PingUpMessage
  | PongUpMessage
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform MessageClientToServer)

makePrisms ''MessageClientToServer

----------------------------------------------------------------------
-- ServerToClient

-- | Local type, only needed for writing the ToSchema instance for 'MessageServerToClient'.
data MessageTypeServerToClient = MsgTypeEvent | MsgTypePingDown | MsgTypePongDown
  deriving (Eq, Enum, Bounded)

msgTypeSchemaServerToClient :: ValueSchema NamedSwaggerDoc MessageTypeServerToClient
msgTypeSchemaServerToClient =
  enum @Text "MessageTypeServerToClient" $
    mconcat $
      [ element "event" MsgTypeEvent,
        element "ping" MsgTypePingDown,
        element "pong" MsgTypePongDown
      ]

instance ToSchema MessageServerToClient where
  schema =
    object "MessageServerToClient" $
      fromTagged <$> toTagged .= bind (fst .= field "type" msgTypeSchemaServerToClient) (snd .= untaggedSchema)
    where
      toTagged :: MessageServerToClient -> (MessageTypeServerToClient, MessageServerToClient)
      toTagged d@(EventMessage _) = (MsgTypeEvent, d)
      toTagged d@PingDownMessage = (MsgTypePingDown, d)
      toTagged d@PongDownMessage = (MsgTypePongDown, d)

      fromTagged :: (MessageTypeServerToClient, MessageServerToClient) -> MessageServerToClient
      fromTagged = snd

      untaggedSchema :: SchemaP SwaggerDoc (A.Object, MessageTypeServerToClient) [A.Pair] (MessageServerToClient) (MessageServerToClient)
      untaggedSchema = dispatch $ \case
        MsgTypeEvent -> tag _EventMessage (id .= field "data" schema)
        MsgTypePingDown -> tag _PingDownMessage (id .= pure ())
        MsgTypePongDown -> tag _PongDownMessage (id .= pure ())

deriving via Schema MessageServerToClient instance FromJSON MessageServerToClient

deriving via Schema MessageServerToClient instance ToJSON MessageServerToClient

----------------------------------------------------------------------
-- ClientToServer

-- | Local type, only needed for writing the ToSchema instance for 'MessageClientToServer'.
data MessageTypeClientToServer = MsgTypeAck | MsgTypePingUp | MsgTypePongUp
  deriving (Eq, Enum, Bounded)

msgTypeSchemaClientToServer :: ValueSchema NamedSwaggerDoc MessageTypeClientToServer
msgTypeSchemaClientToServer =
  enum @Text "MessageTypeClientToServer" $
    mconcat $
      [ element "ack" MsgTypeAck,
        element "ping" MsgTypePingUp,
        element "pong" MsgTypePongUp
      ]

instance ToSchema MessageClientToServer where
  schema =
    object "MessageClientToServer" $
      fromTagged <$> toTagged .= bind (fst .= field "type" msgTypeSchemaClientToServer) (snd .= untaggedSchema)
    where
      toTagged :: MessageClientToServer -> (MessageTypeClientToServer, MessageClientToServer)
      toTagged d@(AckMessage _) = (MsgTypeAck, d)
      toTagged d@PingUpMessage = (MsgTypePingUp, d)
      toTagged d@PongUpMessage = (MsgTypePongUp, d)

      fromTagged :: (MessageTypeClientToServer, MessageClientToServer) -> MessageClientToServer
      fromTagged = snd

      untaggedSchema :: SchemaP SwaggerDoc (A.Object, MessageTypeClientToServer) [A.Pair] MessageClientToServer MessageClientToServer
      untaggedSchema = dispatch $ \case
        MsgTypeAck -> tag _AckMessage (id .= field "data" schema)
        MsgTypePingUp -> tag _PingUpMessage (id .= pure ())
        MsgTypePongUp -> tag _PongUpMessage (id .= pure ())

deriving via Schema MessageClientToServer instance FromJSON MessageClientToServer

deriving via Schema MessageClientToServer instance ToJSON MessageClientToServer
