{-# LANGUAGE OverloadedStrings #-}

module Gundeck.Types.Swagger where

import Data.Swagger.Build.Api

gundeckModels :: [Model]
gundeckModels =
    [ pushList
    , push
    , notificationList
    , notification
    , event
    ]

-------------------------------------------------------------------------------
-- Push Models

pushTransport :: DataType
pushTransport = string $ enum
    [ "GCM"
    , "APNS"
    , "APNS_SANDBOX"
    , "APNS_VOIP"
    , "APNS_VOIP_SANDBOX"
    ]

push :: Model
push = defineModel "Push" $ do
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
    property "fallback" pushTransport $ do
        description "Fallback transport"
        optional

pushList :: Model
pushList = defineModel "PushList" $ do
    description "List of Native Push Tokens"
    property "tokens" (array (ref push)) $
        description "Tokens"

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

