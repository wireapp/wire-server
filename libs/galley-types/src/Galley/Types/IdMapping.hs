{-# LANGUAGE OverloadedStrings #-}

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

module Galley.Types.IdMapping
  ( PostIdMappingRequest (..),
  )
where

import Data.Aeson
import Data.Id (Id, Remote)
import Data.Qualified (Qualified)
import Imports

-- | Request used for inter-service communication between Galley and Brig to ensure that ID
-- mappings discovered in one service are also known in the other.
--
-- The type of Id (user or conversation) doesn't matter, since it's all the same table.
-- Therefore, we use @()@.
newtype PostIdMappingRequest = PostIdMappingRequest
  { reqQualifiedId :: Qualified (Id (Remote ()))
  }
  deriving stock (Eq, Show)

instance FromJSON PostIdMappingRequest where
  parseJSON = withObject "PostIdMappingRequest" $ \o ->
    PostIdMappingRequest <$> o .: "qualified_id"

instance ToJSON PostIdMappingRequest where
  toJSON (PostIdMappingRequest qualifiedId) =
    object ["qualified_id" .= qualifiedId]
