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

-- Partial expression index for the recurring-meeting branches of
-- listMeetingsByUserImpl, listMeetingsByConversationImpl and
-- getOldMeetingsImpl, which filter/order on
-- GREATEST(end_time, recurrence_until). Non-recurring meetings keep using
-- idx_meetings_end_time.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_meetings_recurrence_eff_end
    ON meetings (GREATEST(end_time, recurrence_until))
    WHERE recurrence_frequency IS NOT NULL
      AND recurrence_interval IS NOT NULL
      AND recurrence_until IS NOT NULL;
