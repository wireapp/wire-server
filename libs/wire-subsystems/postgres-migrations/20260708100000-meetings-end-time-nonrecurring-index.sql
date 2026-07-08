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

-- Partial index for the non-recurring branch of getOldMeetingsImpl, so the
-- cleanup worker can find expired non-recurring meetings ordered by end_time
-- without scanning not-yet-expired recurring meetings (which carry an old
-- end_time but a recurrence window still open in the future). See WPB-26823.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_meetings_end_time_nonrecurring
    ON meetings (end_time)
    WHERE recurrence_frequency IS NULL;
