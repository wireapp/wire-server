-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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
module Galley.Schema.V101_ConversationLowerGCGracePeriod
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 101 "set gc_grace_period for conversation related tables to 1 day" $ do
    schema'
      [r| ALTER TABLE conversation WITH gc_grace_seconds = 86400 |]

    schema'
      [r| ALTER TABLE mls_group_member_client WITH gc_grace_seconds = 86400 |]

    schema'
      [r| ALTER TABLE subconversation WITH gc_grace_seconds = 86400 |]

    schema'
      [r| ALTER TABLE member WITH gc_grace_seconds = 86400 |]

    schema'
      [r| ALTER TABLE user WITH gc_grace_seconds = 86400 |]

    schema'
      [r| ALTER TABLE member_remote_user WITH gc_grace_seconds = 86400 |]

    schema'
      [r| ALTER TABLE team_conv WITH gc_grace_seconds = 86400 |]
