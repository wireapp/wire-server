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

module V10
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 10 "Switch to leveled compaction" $ do
  void $
    schema'
      [r|
        alter columnfamily user with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily user_keys with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily activation_keys with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily password_reset with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily push with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily connection with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily invitation with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  void $
    schema'
      [r|
        alter columnfamily invitee_info with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
