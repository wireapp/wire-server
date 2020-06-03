{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Wire.API.Team.SearchVisibility
  ( TeamSearchVisibility (..),
    TeamSearchVisibilityView (..),

    -- * Swagger
    modelTeamSearchVisibility,
    typeSearchVisibility,
  )
where

import Data.Aeson
import Data.String.Conversions (cs)
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text as T
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- TeamSearchVisibility

-- | Who can find whom inside and outside of a team?
-- Individual setting for one team, chosen by the admin.
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
--
-- See also: 'FeatureTeamSearchVisibility', 'TeamSearchVisibilityEnabled'.
data TeamSearchVisibility
  = SearchVisibilityStandard
  | SearchVisibilityNoNameOutsideTeam
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving (Arbitrary) via (GenericUniform TeamSearchVisibility)

typeSearchVisibility :: Doc.DataType
typeSearchVisibility =
  Doc.string . Doc.enum $
    cs . encode <$> [(minBound :: TeamSearchVisibility) ..]

instance ToJSON TeamSearchVisibility where
  toJSON SearchVisibilityStandard = "standard"
  toJSON SearchVisibilityNoNameOutsideTeam = "no-name-outside-team"

instance FromJSON TeamSearchVisibility where
  parseJSON = withText "TeamSearchVisibility" $ \case
    "standard" -> pure SearchVisibilityStandard
    "no-name-outside-team" -> pure SearchVisibilityNoNameOutsideTeam
    x -> fail $ "unexpected status type: " <> T.unpack x

--------------------------------------------------------------------------------
-- TeamSearchVisibilityView

newtype TeamSearchVisibilityView = TeamSearchVisibilityView TeamSearchVisibility
  deriving stock (Eq, Show, Ord, Bounded, Generic)
  deriving newtype (Arbitrary)

modelTeamSearchVisibility :: Doc.Model
modelTeamSearchVisibility = Doc.defineModel "TeamSearchVisibility" $ do
  Doc.description "Search visibility value for the team"
  Doc.property "search_visibility" typeSearchVisibility $ do
    Doc.description "value of visibility"

instance ToJSON TeamSearchVisibilityView where
  toJSON (TeamSearchVisibilityView s) = object ["search_visibility" .= s]

instance FromJSON TeamSearchVisibilityView where
  parseJSON = withObject "TeamSearchVisibilityView" $ \o ->
    TeamSearchVisibilityView <$> o .: "search_visibility"
