-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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
module Galley.Schema.V88_RemoveMemberClientAndTruncateMLSGroupMemberClient
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

-- | This migration exists because the table could have some rogue data in it
-- before MLS Draft-17 was implemented. It was not supposed to be used, but it
-- could've been. This migration just deletes old data. This could break some
-- conversations/users in unknown ways. But those are most likely test users.
migration :: Migration
migration = Migration 88 "Remove member_client and Truncate mls_group_member_client" $ do
  schema'
    [r|TRUNCATE TABLE mls_group_member_client|]
  schema'
    [r|DROP TABLE IF EXISTS member_client|]
