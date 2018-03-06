{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gundeck.Types.BulkPush where

import Data.Aeson
import Data.Id
import GHC.Generics
import Gundeck.Types.Notification


type PushTarget = (UserId, ConnId)

-- | A lazy bytestring as needed for constructing a binary
-- <https://hackage.haskell.org/package/websockets-0.12.3.1/docs/Network-WebSockets.html#t:DataMessage>.
data BulkPushRequest = BulkPushRequest
    { bpBody :: ![(Notification, [PushTarget])]
    } deriving ( Show
               , Generic
               )

instance FromJSON BulkPushRequest
instance ToJSON BulkPushRequest

data PushStatus = PushStatusOk | PushStatusGone
  deriving (Eq, Show, Generic)

instance FromJSON PushStatus
instance ToJSON PushStatus

data BulkPushResponse = BulkPushResponse
    { bprespBody :: ![(PushTarget, PushStatus)]
    } deriving ( Show
               , Generic
               )

instance FromJSON BulkPushResponse
instance ToJSON BulkPushResponse
