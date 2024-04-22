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

module Wire.InternalEvent
  ( InternalNotification (..),
  )
where

import Data.Aeson
import Data.Aeson.Types
import Data.Id
import Imports

data InternalNotification
  = DeleteClient !ClientId !UserId !(Maybe ConnId)
  | DeleteUser !UserId
  | DeleteService !ProviderId !ServiceId
  deriving (Eq, Show)

data InternalNotificationType
  = ClientDeletion
  | UserDeletion
  | ServiceDeletion
  deriving (Eq, Show)

instance FromJSON InternalNotificationType where
  parseJSON = \case
    "client.delete" -> pure ClientDeletion
    "user.delete" -> pure UserDeletion
    "service.delete" -> pure ServiceDeletion
    x -> fail $ "InternalNotificationType: Unknown type " <> show x

instance ToJSON InternalNotificationType where
  toJSON ClientDeletion = "client.delete"
  toJSON UserDeletion = "user.delete"
  toJSON ServiceDeletion = "service.delete"

instance FromJSON InternalNotification where
  parseJSON = withObject "InternalNotification" $ \o -> do
    t <- o .: "type"
    case (t :: InternalNotificationType) of
      ClientDeletion -> DeleteClient <$> (adaptOldFormat =<< (o .: "client")) <*> o .: "user" <*> o .: "connection"
      UserDeletion -> DeleteUser <$> o .: "user"
      ServiceDeletion -> DeleteService <$> o .: "provider" <*> o .: "service"
    where
      adaptOldFormat :: Value -> Parser ClientId
      adaptOldFormat (Object ob) = ob .: "id"
      adaptOldFormat v@(String _) = parseJSON v
      adaptOldFormat _ = fail "adaptOld: "

instance ToJSON InternalNotification where
  toJSON (DeleteClient c uid con) =
    object
      [ "client" .= c,
        "user" .= uid,
        "connection" .= con,
        "type" .= ClientDeletion
      ]
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
