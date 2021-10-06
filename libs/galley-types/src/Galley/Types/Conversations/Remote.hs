-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
-- TODO: Rename to Galley.Types.Conversations.Intra
module Galley.Types.Conversations.Remote
  ( DesiredMembership (..),
    Actor (..),
    UpsertOne2OneConversationRequest (..),
    UpsertOne2OneConversationResponse (..),
  )
where

import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.Id (ConvId, UserId)
import Data.Qualified (Local, Qualified, Remote)
import Imports

data DesiredMembership = Included | Excluded
  deriving (Show, Generic)

-- TODO: write cusom instances with roundtrip tests
instance ToJSON DesiredMembership

-- TODO: write cusom instances with roundtrip tests
instance FromJSON DesiredMembership

data Actor = LocalActor | RemoteActor deriving (Show, Eq, Generic)

-- TODO: write cusom instances with roundtrip tests
instance ToJSON Actor

-- TODO: write cusom instances with roundtrip tests
instance FromJSON Actor

data UpsertOne2OneConversationRequest = UpsertOne2OneConversationRequest
  { uooLocalUser :: Local UserId,
    uooRemoteUser :: Remote UserId,
    uooActor :: Actor,
    uooActorDesiredMembership :: DesiredMembership,
    uooConvId :: Maybe (Qualified ConvId)
  }
  deriving (Show, Generic)

-- TODO: write cusom instances with roundtrip tests
instance ToJSON UpsertOne2OneConversationRequest

-- TODO: write cusom instances with roundtrip tests
instance FromJSON UpsertOne2OneConversationRequest

data UpsertOne2OneConversationResponse = UpsertOne2OneConversationResponse
  { -- | The Nothing value here indicated that there an impossible request was
    -- received, e.g., requesting to remove a remote user when the actor is a
    -- local user.
    uuorConvId :: Maybe (Qualified ConvId)
  }
  deriving (Show, Generic)

-- TODO: write cusom instances with roundtrip tests
instance ToJSON UpsertOne2OneConversationResponse

-- TODO: write cusom instances with roundtrip tests
instance FromJSON UpsertOne2OneConversationResponse
