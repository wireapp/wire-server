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

module Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti
  ( TeamFeatureNoConfigMultiRequest (..),
    TeamFeatureNoConfigMultiResponse (..),
    TeamStatus (..),
  )
where

import qualified Data.Aeson as A
import Data.Id
import Data.Schema
import qualified Data.Swagger as S
import Imports
import qualified Wire.API.Team.Feature as Public

newtype TeamFeatureNoConfigMultiRequest = TeamFeatureNoConfigMultiRequest
  { teams :: [TeamId]
  }
  deriving (Show, Eq)
  deriving (A.ToJSON, A.FromJSON) via (Schema TeamFeatureNoConfigMultiRequest)

instance ToSchema TeamFeatureNoConfigMultiRequest where
  schema =
    object "TeamFeatureNoConfigMultiRequest" $
      TeamFeatureNoConfigMultiRequest
        <$> teams .= field "teams" (array schema)

newtype TeamFeatureNoConfigMultiResponse cfg = TeamFeatureNoConfigMultiResponse
  { teamsStatuses :: [TeamStatus cfg]
  }
  deriving (Show, Eq)
  deriving (A.ToJSON, A.FromJSON) via (Schema (TeamFeatureNoConfigMultiResponse cfg))

instance ToSchema (TeamFeatureNoConfigMultiResponse cfg) where
  schema =
    object "TeamFeatureNoConfigMultiResponse" $
      TeamFeatureNoConfigMultiResponse
        <$> teamsStatuses .= field "default_status" (array schema)

data TeamStatus cfg = TeamStatus
  { team :: TeamId,
    status :: Public.FeatureStatus
  }
  deriving (Show, Eq)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema (TeamStatus cfg))

instance ToSchema (TeamStatus cfg) where
  schema =
    object "TeamStatus" $
      TeamStatus
        <$> team .= field "team" schema
        <*> status .= field "status" schema
