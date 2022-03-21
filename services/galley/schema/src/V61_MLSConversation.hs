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

module V61_MLSConversation
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 61 "Add MLS fields to conversation and create a group ID to conversation ID mapping table" $ do
    schema'
      [r| ALTER TABLE conversation ADD (
            protocol int,
            group_id blob
          )
        |]
    schema'
      [r| CREATE TABLE group_id_conv_id (
            group_id blob PRIMARY KEY,
            conv_id uuid,
            domain text
          )
        |]
