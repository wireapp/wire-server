-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module V53_AddRemoteConvStatus (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

-- This migration adds fields that track remote conversation status for a local user.
migration :: Migration
migration =
  Migration 53 "Add fields for remote conversation status (hidden/archived/muted)" $
    schema'
      [r|
      ALTER TABLE user_remote_conv ADD (
        hidden            boolean,
        hidden_ref        text,
        otr_archived      boolean,
        otr_archived_ref  text,
        otr_muted_status  int,
        otr_muted_ref     text
      )
    |]
