-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

-- | Deployment licensing payload as exposed to clients via @GET /license@.
--
-- Mirrors the enterprise @Wire.Licensing.License@ JSON contract exactly
-- (the field names below are load-bearing); all-false flags are served when
-- the deployment has no valid license.
module Wire.API.License
  ( License (..),
    Meetings (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Wire.Arbitrary (Arbitrary (..), GenericUniform (..))

data Meetings = Meetings
  { invitationLinks :: Bool,
    emailsSending :: Bool,
    calendarsIntegration :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema Meetings)
  deriving (Arbitrary) via (GenericUniform Meetings)

data License = License
  { meetings :: Meetings
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema License)
  deriving (Arbitrary) via (GenericUniform License)

instance ToSchema Meetings where
  schema =
    object $
      Meetings
        <$> (.invitationLinks) .= field "invitation_links" schema
        <*> (.emailsSending) .= field "emails_sending" schema
        <*> (.calendarsIntegration) .= field "calendars_integration" schema

instance ToSchema License where
  schema = object $ License <$> (.meetings) .= field "meetings" schema
