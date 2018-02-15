{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gundeck.Types.BulkPush where

import Data.Aeson
import Data.Id
import Data.Set (Set)
import GHC.Generics
import Gundeck.Types.Notification

data UserDevicePayload = UserDevicePayload
    { udUid  :: !UserId
    , udDid  :: !ConnId
    , udData :: !Notification
    } deriving ( Show
               , Generic
               )

instance FromJSON UserDevicePayload
instance ToJSON   UserDevicePayload

data BulkPush = BulkPush
    { bpRecipients :: ![UserDevicePayload]
    } deriving ( Show
               , Generic
               )

instance FromJSON BulkPush
instance ToJSON   BulkPush

data PushResponse = PushResponse
    { bpUid    :: !UserId
    , bpDid    :: !ConnId
    , bpStatus :: !Int
    } deriving ( Show
               , Generic
               )

instance FromJSON PushResponse
instance ToJSON PushResponse

data BulkPushResults = BulkPushResults
    { bprFails     :: ![PushResponse]
    , bprSuccesses :: ![PushResponse]
    } deriving ( Show
               , Generic
               )

instance FromJSON BulkPushResults
instance ToJSON BulkPushResults

data PushParams = PushParams
    { notif      :: !Notification
    , targets    :: ![NotificationTarget]
    , originUser :: !UserId -- Origin user.
    , originConn :: !(Maybe ConnId) -- Origin device connection.
    , conns      :: !(Set ConnId) -- Only target these connections.
    }
