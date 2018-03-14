{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module defines the types used by the Cannon API.  It is contained in package gundeck-types
-- for the pragmatic reason that it allows us to re-use types from the gundeck API.  (This move can
-- be justified by picturing Cannon as a local service that makes only sense in the context of a
-- Gundeck.)
module Gundeck.Types.BulkPush where

import Data.Aeson
import Data.Id
import GHC.Generics
import Gundeck.Types.Notification


type PushTarget = (UserId, ConnId)

data BulkPushRequest = BulkPushRequest
    { fromBulkPushRequest :: ![(Notification, [PushTarget])]
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
    { fromBulkPushResponse :: ![(NotificationId, PushTarget, PushStatus)]
    } deriving ( Show
               , Generic
               )

instance FromJSON BulkPushResponse
instance ToJSON BulkPushResponse
