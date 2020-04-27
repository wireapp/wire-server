{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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

-- | This module defines the types used by the Cannon API.  It is contained in package gundeck-types
-- for the pragmatic reason that it allows us to re-use types from the gundeck API.  (This move can
-- be justified by picturing Cannon as a local service that makes only sense in the context of a
-- Gundeck.)
module Gundeck.Types.BulkPush where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Id
import Gundeck.Types.Notification
import Imports

data PushTarget = PushTarget
  { ptUserId :: !UserId,
    ptConnId :: !ConnId
  }
  deriving
    ( Eq,
      Ord,
      Show,
      Generic
    )

instance FromJSON PushTarget where
  parseJSON = withObject "push target object" $ \hm ->
    PushTarget <$> (hm .: "user_id") <*> (hm .: "conn_id")

instance ToJSON PushTarget where
  toJSON (PushTarget u c) = object ["user_id" .= u, "conn_id" .= c]

newtype BulkPushRequest = BulkPushRequest
  { fromBulkPushRequest :: [(Notification, [PushTarget])]
  }
  deriving
    ( Eq,
      Show,
      Generic
    )

instance FromJSON BulkPushRequest where
  parseJSON = withObject "bulkpush request body" $ \hm ->
    BulkPushRequest <$> (mapM run =<< (hm .: "bulkpush_req"))
    where
      run = withObject "object with notifcation, targets" $ \hm ->
        (,) <$> (hm .: "notification") <*> (hm .: "targets")

instance ToJSON BulkPushRequest where
  toJSON (BulkPushRequest ns) = object ["bulkpush_req" .= (run <$> ns)]
    where
      run (n, ps) = object ["notification" .= n, "targets" .= ps]

data PushStatus = PushStatusOk | PushStatusGone
  deriving (Eq, Show, Bounded, Enum, Generic)

$(deriveJSON (defaultOptions {constructorTagModifier = camelTo2 '_'}) ''PushStatus)

newtype BulkPushResponse = BulkPushResponse
  { fromBulkPushResponse :: [(NotificationId, PushTarget, PushStatus)]
  }
  deriving
    ( Eq,
      Show,
      Generic
    )

instance FromJSON BulkPushResponse where
  parseJSON = withObject "bulkpush response body" $ \hm ->
    BulkPushResponse <$> (mapM run =<< (hm .: "bulkpush_resp"))
    where
      run = withObject "object with notifId, target, status" $ \hm ->
        (,,) <$> (hm .: "notif_id") <*> (hm .: "target") <*> (hm .: "status")

instance ToJSON BulkPushResponse where
  toJSON (BulkPushResponse ns) = object ["bulkpush_resp" .= (run <$> ns)]
    where
      run (n, p, s) = object ["notif_id" .= n, "target" .= p, "status" .= s]
