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

-- | Tables that are used in this module:
-- - conversation_codes
-- - custom_backend
-- - legalhold_pending_prekeys
-- - legalhold_service
-- - legalhold_whitelisted
-- - team
-- - team_member
-- update using: `rg -i -P '(?:update|from|into)\s+([A-Za-z0-9_]+)' -or '$1' --no-line-number services/galley/src/Galley/Cassandra/Queries.hs | sort | uniq`
module Galley.Cassandra.Queries
  ( selectCustomBackend,
    upsertCustomBackend,
    deleteCustomBackend,
    selectSearchVisibility,
    updateSearchVisibility,
  )
where

import Cassandra as C hiding (Value)
import Data.Domain (Domain)
import Data.Id
import Data.Misc
import Imports
import Wire.API.Team.SearchVisibility

-- Search Visibility --------------------------------------------------------

selectSearchVisibility :: PrepQuery R (Identity TeamId) (Identity (Maybe TeamSearchVisibility))
selectSearchVisibility =
  "select search_visibility from team where team = ?"

updateSearchVisibility :: PrepQuery W (TeamSearchVisibility, TeamId) ()
updateSearchVisibility =
  {- `IF EXISTS`, but that requires benchmarking -} "update team set search_visibility = ? where team = ?"

-- Custom Backend -----------------------------------------------------------

selectCustomBackend :: PrepQuery R (Identity Domain) (HttpsUrl, HttpsUrl)
selectCustomBackend =
  "select config_json_url, webapp_welcome_url from custom_backend where domain = ?"

upsertCustomBackend :: PrepQuery W (HttpsUrl, HttpsUrl, Domain) ()
upsertCustomBackend =
  "update custom_backend set config_json_url = ?, webapp_welcome_url = ? where domain = ?"

deleteCustomBackend :: PrepQuery W (Identity Domain) ()
deleteCustomBackend =
  "delete from custom_backend where domain = ?"
