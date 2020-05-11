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

module V39
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 39 "Add extra feature `same_team_only_status` field in the team_features table and in the team" $ do
  schema' [r| ALTER TABLE team_features ADD search_visibility_status int; |]
  schema' [r| ALTER TABLE team ADD search_visibility int; |]
