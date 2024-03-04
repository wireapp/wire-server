-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Routes.Internal.Galley.ConversationsIntra where

import Data.Aeson (FromJSON, ToJSON)
import Data.Id (ConvId, UserId)
import Data.OpenApi qualified as Swagger
import Data.Qualified
import Data.Schema
import Imports

data DesiredMembership = Included | Excluded
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Schema DesiredMembership

instance ToSchema DesiredMembership where
  schema =
    enum @Text "DesiredMembership" $
      mconcat
        [ element "included" Included,
          element "excluded" Excluded
        ]

data Actor = LocalActor | RemoteActor
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Schema Actor

instance ToSchema Actor where
  schema =
    enum @Text "Actor" $
      mconcat
        [ element "local_actor" LocalActor,
          element "remote_actor" RemoteActor
        ]

data UpsertOne2OneConversationRequest = UpsertOne2OneConversationRequest
  { uooLocalUser :: Local UserId,
    uooRemoteUser :: Remote UserId,
    uooActor :: Actor,
    uooActorDesiredMembership :: DesiredMembership,
    uooConvId :: Qualified ConvId
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema UpsertOne2OneConversationRequest

instance ToSchema UpsertOne2OneConversationRequest where
  schema =
    object "UpsertOne2OneConversationRequest" $
      UpsertOne2OneConversationRequest
        <$> (tUntagged . uooLocalUser) .= field "local_user" (qTagUnsafe <$> schema)
        <*> (tUntagged . uooRemoteUser) .= field "remote_user" (qTagUnsafe <$> schema)
        <*> uooActor .= field "actor" schema
        <*> uooActorDesiredMembership .= field "actor_desired_membership" schema
        <*> uooConvId .= field "conversation_id" schema
