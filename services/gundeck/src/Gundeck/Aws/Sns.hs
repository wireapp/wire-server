module Gundeck.Aws.Sns
  ( Event,
    EventType (..),
    DeliveryFailure (..),
    evType,
    evEndpoint,
  )
where

import Control.Error
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Gundeck.Aws.Arn (EndpointArn)
import Imports
import Network.AWS.Data (ToText (..), fromText)

data EventType
  = EndpointCreated
  | EndpointUpdated
  | EndpointDeleted
  | DeliveryFailure !DeliveryFailure
  | UnknownEvent !Text
  deriving (Eq, Show)

data DeliveryFailure
  = DeliveryInvalidToken
  | DeliveryFailedPerm
  | DeliveryEndpointDisabled
  | DeliveryTTLExpired
  | DeliveryUnknownFailure !Text
  deriving (Eq, Show)

data Event
  = Event
      { _evType :: !EventType,
        _evEndpoint :: !EndpointArn
      }
  deriving (Show)

makeLenses ''Event

readDeliveryFailure :: Text -> DeliveryFailure
readDeliveryFailure "InvalidPlatformToken" = DeliveryInvalidToken
readDeliveryFailure "DeliveryFailedPerm" = DeliveryFailedPerm
readDeliveryFailure "EndpointDisabled" = DeliveryEndpointDisabled
readDeliveryFailure "NotificationTTLExpired" = DeliveryTTLExpired
readDeliveryFailure other = DeliveryUnknownFailure other

instance ToText DeliveryFailure where
  toText DeliveryInvalidToken = "InvalidPlatformToken"
  toText DeliveryFailedPerm = "DeliveryFailedPerm"
  toText DeliveryEndpointDisabled = "EndpointDisabled"
  toText DeliveryTTLExpired = "NotificationTTLExpired"
  toText (DeliveryUnknownFailure other) = "UnknownFailure: " <> other

readEventType :: Text -> Maybe Text -> EventType
readEventType "EndpointCreated" _ = EndpointCreated
readEventType "EndpointUpdated" _ = EndpointUpdated
readEventType "EndpointDeleted" _ = EndpointDeleted
readEventType "DeliveryFailure" (Just e) = DeliveryFailure (readDeliveryFailure e)
readEventType other _ = UnknownEvent other

instance ToText EventType where
  toText EndpointCreated = "EndpointCreated"
  toText EndpointUpdated = "EndpointUpdated"
  toText EndpointDeleted = "EndpointDeleted"
  toText (DeliveryFailure e) = "DeliveryFailure: " <> toText e
  toText (UnknownEvent other) = "UnknownEvent: " <> other

instance FromJSON Event where
  -- n.b. The SNS topic publishing these events must be configured for raw
  -- message delivery: cf. https://aws.amazon.com/sns/faqs/#raw-message-delivery
  parseJSON m = maybe (fail "Failed to parse SNS event") return $ do
    e <- m ^? key "EndpointArn" . _String >>= hush . fromText
    t <- m ^? key "EventType" . _String
    let f = m ^? key "FailureType" . _String
    return $! Event (readEventType t f) e
