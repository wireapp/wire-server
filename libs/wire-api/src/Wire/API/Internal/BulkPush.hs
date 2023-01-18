-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Internal.BulkPush where

import Data.Aeson
import Data.Id
import qualified Data.Schema as S
import qualified Data.Swagger as Swagger
import Imports
import Wire.API.Internal.Notification

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
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via S.Schema PushTarget

instance S.ToSchema PushTarget where
  schema =
    S.object "PushTarget" $
      PushTarget
        <$> ptUserId S..= S.field "user_id" S.schema
        <*> ptConnId S..= S.field "conn_id" S.schema

newtype BulkPushRequest = BulkPushRequest
  { fromBulkPushRequest :: [(Notification, [PushTarget])]
  }
  deriving
    ( Eq,
      Show,
      Generic
    )
  deriving (ToJSON, FromJSON, Swagger.ToSchema) via S.Schema BulkPushRequest

instance S.ToSchema BulkPushRequest where
  schema =
    S.object "BulkPushRequest" $
      BulkPushRequest
        <$> fromBulkPushRequest S..= S.field "bulkpush_req" (S.array S.schema)

instance S.ToSchema (Notification, [PushTarget]) where
  schema =
    S.object "(Notification, [PushTarget])" $
      (,)
        <$> fst S..= S.field "notification" S.schema
        <*> snd S..= S.field "targets" (S.array S.schema)

data PushStatus = PushStatusOk | PushStatusGone
  deriving (Eq, Show, Bounded, Enum, Generic)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via S.Schema PushStatus

instance S.ToSchema PushStatus where
  schema =
    S.enum @Text "PushStatus" $
      mconcat
        [ S.element "push_status_ok" PushStatusOk,
          S.element "push_status_gone" PushStatusGone
        ]

newtype BulkPushResponse = BulkPushResponse
  { fromBulkPushResponse :: [(NotificationId, PushTarget, PushStatus)]
  }
  deriving
    ( Eq,
      Show,
      Generic
    )
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via S.Schema BulkPushResponse

instance S.ToSchema BulkPushResponse where
  schema =
    S.object "BulkPushResponse" $
      BulkPushResponse
        <$> fromBulkPushResponse S..= S.field "bulkpush_resp" (S.array S.schema)

instance S.ToSchema (NotificationId, PushTarget, PushStatus) where
  schema =
    S.object "(NotificationId, PushTarget, PushStatus)" $
      (,,)
        <$> (\(a, _, _) -> a) S..= S.field "notif_id" S.schema
        <*> (\(_, b, _) -> b) S..= S.field "target" S.schema
        <*> (\(_, _, c) -> c) S..= S.field "target" S.schema
