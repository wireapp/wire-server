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

CREATE TABLE IF NOT EXISTS connection (
  "left" uuid NOT NULL,
  "right" uuid NOT NULL,
  status int NOT NULL,
  last_update timestamptz NOT NULL,
  conv uuid,
  PRIMARY KEY ("left", "right")
);

CREATE TABLE IF NOT EXISTS connection_remote (
  "left" uuid NOT NULL,
  right_domain text NOT NULL,
  right_user uuid NOT NULL,
  status int NOT NULL,
  last_update timestamptz NOT NULL,
  conv_domain text,
  conv_id uuid,
  PRIMARY KEY ("left", right_domain, right_user)
);

-- Parity with the Cassandra secondary index on connection_remote.right_domain.
CREATE INDEX IF NOT EXISTS connection_remote_right_domain_idx ON connection_remote (right_domain);
