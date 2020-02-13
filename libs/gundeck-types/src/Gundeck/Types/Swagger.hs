{-# LANGUAGE OverloadedStrings #-}

module Gundeck.Types.Swagger where

import Data.Swagger.Build.Api
import Imports

gundeckModels :: [Model]
gundeckModels =
  [ pushTokenList,
    pushToken,
    notificationList,
    notification,
    event
  ]

-------------------------------------------------------------------------------
-- Push Models

pushTransport :: DataType
pushTransport =
  string $
    enum
      [ "GCM",
        "APNS",
        "APNS_SANDBOX",
        "APNS_VOIP",
        "APNS_VOIP_SANDBOX"
      ]

pushToken :: Model
pushToken = defineModel "PushToken" $ do
  description "Native Push Token"
  property "transport" pushTransport $
    description "Transport"
  property "app" string' $
    description "Application"
  property "token" bytes' $
    description "Access Token"
  property "client" bytes' $ do
    description "Client ID"
    optional

pushTokenList :: Model
pushTokenList = defineModel "PushTokenList" $ do
  description "List of Native Push Tokens"
  property "tokens" (array (ref pushToken)) $
    description "Push tokens"

-------------------------------------------------------------------------------
-- Notification Models

notificationList :: Model
notificationList = defineModel "NotificationList" $ do
  description "Zero or more notifications"
  property "notifications" (array (ref notification)) $
    description "Notifications"
  property "has_more" bool' $
    description "Whether there are still more notifications."

notification :: Model
notification = defineModel "Notification" $ do
  description "A single notification"
  property "id" bytes' $
    description "Notification ID"
  property "payload" (array (ref event)) $
    description "List of events"

event :: Model
event = defineModel "NotificationEvent" $ do
  description "A single event"
  property "type" string' $
    description "Event type"
