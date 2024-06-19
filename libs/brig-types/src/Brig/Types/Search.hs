{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

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

module Brig.Types.Search
  ( TeamSearchInfo (..),
    SearchVisibilityInbound (..),
    defaultSearchVisibilityInbound,
    searchVisibilityInboundFromFeatureStatus,
  )
where

import Cassandra qualified as C
import Data.Aeson
import Data.Attoparsec.ByteString
import Data.ByteString.Builder
import Data.ByteString.Conversion
import Data.ByteString.Lazy
import Data.Id (TeamId)
import Data.Text.Encoding
import Imports
import Test.QuickCheck
import Wire.API.Team.Feature

-- | Outbound search restrictions configured by team admin of the searcher. This
-- value restricts the set of user that are searched.
--
-- See 'optionallySearchWithinTeam' for the effect on full-text search.
--
-- See 'mkTeamSearchInfo' for the business logic that defines the TeamSearchInfo
-- value.
--
-- Search results might be affected by the inbound search restriction settings of
-- the searched user. ('SearchVisibilityInbound')
data TeamSearchInfo
  = -- | Only users that are not part of any team are searched
    NoTeam
  | -- | Only users from the same team as the searcher are searched
    TeamOnly TeamId
  | -- | No search restrictions, all users are searched
    AllUsers

-- | Inbound search restrictions configured by team to-be-searched. Affects only
-- full-text search (i.e. search on the display name and the handle), not exact
-- handle search.
data SearchVisibilityInbound
  = -- | The user can only be found by users from the same team
    SearchableByOwnTeam
  | -- | The user can by found by any user of any team
    SearchableByAllTeams
  deriving (Eq, Show)

instance Arbitrary SearchVisibilityInbound where
  arbitrary = elements [SearchableByOwnTeam, SearchableByAllTeams]

instance ToByteString SearchVisibilityInbound where
  builder SearchableByOwnTeam = "searchable-by-own-team"
  builder SearchableByAllTeams = "searchable-by-all-teams"

instance FromByteString SearchVisibilityInbound where
  parser =
    SearchableByOwnTeam
      <$ string "searchable-by-own-team"
        <|> SearchableByAllTeams
      <$ string "searchable-by-all-teams"

instance C.Cql SearchVisibilityInbound where
  ctype = C.Tagged C.IntColumn

  toCql SearchableByOwnTeam = C.CqlInt 0
  toCql SearchableByAllTeams = C.CqlInt 1

  fromCql (C.CqlInt 0) = pure SearchableByOwnTeam
  fromCql (C.CqlInt 1) = pure SearchableByAllTeams
  fromCql n = Left $ "Unexpected SearchVisibilityInbound: " ++ show n

defaultSearchVisibilityInbound :: SearchVisibilityInbound
defaultSearchVisibilityInbound = SearchableByOwnTeam

searchVisibilityInboundFromFeatureStatus :: FeatureStatus -> SearchVisibilityInbound
searchVisibilityInboundFromFeatureStatus FeatureStatusDisabled = SearchableByOwnTeam
searchVisibilityInboundFromFeatureStatus FeatureStatusEnabled = SearchableByAllTeams

instance ToJSON SearchVisibilityInbound where
  toJSON = String . decodeUtf8 . toStrict . toLazyByteString . builder

instance FromJSON SearchVisibilityInbound where
  parseJSON = withText "SearchVisibilityInbound" $ \str ->
    case runParser (parser @SearchVisibilityInbound) (encodeUtf8 str) of
      Left err -> fail err
      Right result -> pure result
