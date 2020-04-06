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

module Brig.InternalEvent.Types
  ( InternalNotification (..),
  )
where

import BasePrelude
import Data.Aeson
import Data.Id

data InternalNotification
  = DeleteUser !UserId
  | DeleteService !ProviderId !ServiceId
  deriving (Eq, Show)

data InternalNotificationType
  = UserDeletion
  | ServiceDeletion
  deriving (Eq, Show)

instance FromJSON InternalNotificationType where
  parseJSON = \case
    "user.delete" -> return UserDeletion
    "service.delete" -> return ServiceDeletion
    x -> fail $ "InternalNotificationType: Unknown type " <> show x

instance ToJSON InternalNotificationType where
  toJSON UserDeletion = "user.delete"
  toJSON ServiceDeletion = "service.delete"

instance FromJSON InternalNotification where
  parseJSON = withObject "InternalNotification" $ \o -> do
    t <- o .: "type"
    case (t :: InternalNotificationType) of
      UserDeletion -> DeleteUser <$> o .: "user"
      ServiceDeletion -> DeleteService <$> o .: "provider" <*> o .: "service"

instance ToJSON InternalNotification where
  toJSON (DeleteUser uid) =
    object
      [ "user" .= uid,
        "type" .= UserDeletion
      ]
  toJSON (DeleteService pid sid) =
    object
      [ "provider" .= pid,
        "service" .= sid,
        "type" .= ServiceDeletion
      ]
