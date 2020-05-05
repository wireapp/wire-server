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

module Galley.Types.Teams.SearchVisibility where

import Data.Aeson
import qualified Data.Text as T
import Imports

-- | If 'FeatureCustomSearchVisibility' is enabled: who can find whom in a team?
--
-- @
-- Standard:
--   Outbound:
--     Handle: can find anyone
--     Name: same team or non team users
--   Inbound:
--     Handle: can be found by anyone
--     Name: can be found by same team only
-- NoNameOutsideTeam:
--   Outbound:
--     Handle: can find anyone
--     Name: same team only
--   Inbound:
--     Handle: can be found by anyone
--     Name: can be found by same team only
-- @
data CustomSearchVisibilityType
  = SearchVisibilityStandard
  | SearchVisibilityNoNameOutsideTeam
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToJSON CustomSearchVisibilityType where
  toJSON SearchVisibilityStandard = "standard"
  toJSON SearchVisibilityNoNameOutsideTeam = "no-name-outside-team"

instance FromJSON CustomSearchVisibilityType where
  parseJSON = withText "CustomSearchVisibilityType" $ \case
    "standard" -> pure SearchVisibilityStandard
    "no-name-outside-team" -> pure SearchVisibilityNoNameOutsideTeam
    x -> fail $ "unexpected status type: " <> T.unpack x

newtype SearchVisibility = SearchVisibility {searchVisibility :: CustomSearchVisibilityType}
  deriving stock (Eq, Show, Ord, Bounded, Generic)

instance ToJSON SearchVisibility where
  toJSON s = object ["search_visibility" .= searchVisibility s]

instance FromJSON SearchVisibility where
  parseJSON = withObject "SearchVisibility" $ \o ->
    SearchVisibility <$> o .: "search_visibility"

-- Status of the feature

data CustomSearchVisibilityStatus = CustomSearchVisibilityDisabled | CustomSearchVisibilityEnabled
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToJSON CustomSearchVisibilityStatus where
  toJSON CustomSearchVisibilityEnabled = "enabled"
  toJSON CustomSearchVisibilityDisabled = "disabled"

instance FromJSON CustomSearchVisibilityStatus where
  parseJSON = withText "CustomSearchVisibilityStatus" $ \case
    "enabled" -> pure CustomSearchVisibilityEnabled
    "disabled" -> pure CustomSearchVisibilityDisabled
    x -> fail $ "unexpected status type: " <> T.unpack x

data CustomSearchVisibilityTeamConfig = CustomSearchVisibilityTeamConfig
  { customSearchVisibilityTeamConfigStatus :: !CustomSearchVisibilityStatus
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CustomSearchVisibilityTeamConfig where
  toJSON s =
    object $
      ["status" .= customSearchVisibilityTeamConfigStatus s]

instance FromJSON CustomSearchVisibilityTeamConfig where
  parseJSON = withObject "CustomSearchVisibilityTeamConfig" $ \o ->
    CustomSearchVisibilityTeamConfig <$> o .: "status"
