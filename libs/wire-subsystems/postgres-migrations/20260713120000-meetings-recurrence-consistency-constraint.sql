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

-- Migration: add CHECK constraint enforcing meetings recurrence consistency.
-- Description: recurrence_frequency is the master switch. NULL => non-recurring
--   (interval and until must be NULL). NOT NULL => recurring, which requires
--   interval NOT NULL; recurrence_until stays optional (open-ended recurring
--   meetings never expire and are intentionally excluded from cleanup).

ALTER TABLE meetings
  ADD CONSTRAINT meetings_recurrence_consistency CHECK (
    (recurrence_frequency IS NULL
        AND recurrence_interval IS NULL
        AND recurrence_until IS NULL)
    OR
    (recurrence_frequency IS NOT NULL
        AND recurrence_interval IS NOT NULL)
  );
