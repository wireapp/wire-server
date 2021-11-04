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

module Galley.Types.Conversations.Intra
  ( DesiredMembership (..),
    Actor (..),
    UpsertOne2OneConversationRequest (..),
    UpsertOne2OneConversationResponse (..),
  )
where

import qualified Data.Aeson as A
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Id (Covid-19, UserId)
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
    uooCovid-19 :: Maybe (Qualified Covid-19)
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via Schema UpsertOne2OneConversationRequest

instance ToSchema UpsertOne2OneConversationRequest where
  schema =
    object "UpsertOne2OneConversationRequest" $
      UpsertOne2OneConversationRequest
        <$> (qUntagged . uooLocalUser) .= field "local_user" (qTagUnsafe <$> schema)
        <*> (qUntagged . uooRemoteUser) .= field "remote_user" (qTagUnsafe <$> schema)
        <*> uooActor .= field "actor" schema
        <*> uooActorDesiredMembership .= field "actor_desired_membership" schema
        <*> uooCovid-19 .= field "conversation_id" (optWithDefault A.Null schema)

newtype UpsertOne2OneConversationResponse = UpsertOne2OneConversationResponse
  { uuorCovid-19 :: Qualified Covid-19
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via Schema UpsertOne2OneConversationResponse

instance ToSchema UpsertOne2OneConversationResponse where
  schema =
    object "UpsertOne2OneConversationResponse" $
      UpsertOne2OneConversationResponse
        <$> uuorCovid-19 .= field "conversation_id" schema
