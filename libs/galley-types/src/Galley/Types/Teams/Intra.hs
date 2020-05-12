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

module Galley.Types.Teams.Intra
  ( TeamStatus (..),
    TeamData (..),
    TeamStatusUpdate (..),
    TeamName (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Currency as Currency
import Data.Json.Util
import Data.Time (UTCTime)
import Galley.Types.Teams (Team)
import Imports

data TeamStatus
  = Active
  | PendingDelete
  | Deleted
  | Suspended
  | PendingActive
  deriving (Eq, Show, Generic)

instance ToJSON TeamStatus where
  toJSON Active = String "active"
  toJSON PendingDelete = String "pending_delete"
  toJSON Deleted = String "deleted"
  toJSON Suspended = String "suspended"
  toJSON PendingActive = String "pending_active"

instance FromJSON TeamStatus where
  parseJSON (String "active") = pure Active
  parseJSON (String "pending_delete") = pure PendingDelete
  parseJSON (String "deleted") = pure Deleted
  parseJSON (String "suspended") = pure Suspended
  parseJSON (String "pending_active") = pure PendingActive
  parseJSON other = fail $ "Unknown TeamStatus: " <> show other

data TeamData = TeamData
  { tdTeam :: !Team,
    tdStatus :: !TeamStatus,
    tdStatusTime :: !(Maybe UTCTime) -- This needs to be a Maybe due to backwards compatibility
  }
  deriving (Eq, Show, Generic)

instance ToJSON TeamData where
  toJSON (TeamData t s st) =
    object $
      "team" .= t
        # "status" .= s
        # "status_time" .= (toUTCTimeMillis <$> st)
        # []

instance FromJSON TeamData where
  parseJSON = withObject "team-data" $ \o -> do
    TeamData <$> o .: "team"
      <*> o .: "status"
      <*> o .:? "status_time"

data TeamStatusUpdate = TeamStatusUpdate
  { tuStatus :: !TeamStatus,
    tuCurrency :: !(Maybe Currency.Alpha)
    -- TODO: Remove Currency selection once billing supports currency changes after team creation
  }
  deriving (Eq, Show, Generic)

instance FromJSON TeamStatusUpdate where
  parseJSON = withObject "team-status-update" $ \o ->
    TeamStatusUpdate <$> o .: "status"
      <*> o .:? "currency"

instance ToJSON TeamStatusUpdate where
  toJSON s =
    object
      [ "status" .= tuStatus s,
        "currency" .= tuCurrency s
      ]

newtype TeamName = TeamName
  {tnName :: Text}
  deriving (Eq, Show, Generic)

deriveJSON toJSONFieldName ''TeamName
