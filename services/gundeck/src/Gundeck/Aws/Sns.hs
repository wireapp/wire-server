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

data Event = Event
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
