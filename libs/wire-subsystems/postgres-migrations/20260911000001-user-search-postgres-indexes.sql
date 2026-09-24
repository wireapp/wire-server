-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

-- Prefix-search indexes for @wire_user.name_normalized@ and @handle@
-- (companion to 20260911000000-user-search-postgres.sql).  These run outside a
-- transaction (CREATE INDEX CONCURRENTLY); see
-- 'Wire.PostgresMigrations.nonTransactionMigrations'.
CREATE INDEX CONCURRENTLY IF NOT EXISTS wire_user_name_normalized_pattern_idx ON wire_user (name_normalized text_pattern_ops);
CREATE INDEX CONCURRENTLY IF NOT EXISTS wire_user_lower_handle_pattern_idx ON wire_user (lower(handle) text_pattern_ops);

CREATE INDEX CONCURRENTLY IF NOT EXISTS wire_user_team_idx ON wire_user (team);
