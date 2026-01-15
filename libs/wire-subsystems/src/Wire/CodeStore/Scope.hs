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

module Wire.CodeStore.Scope where

import Cassandra hiding (Value)
import Imports
import Wire.API.PostgresMarshall

data Scope = ReusableCode
  deriving (Eq, Show, Generic)

instance Cql Scope where
  ctype = Tagged IntColumn

  toCql ReusableCode = CqlInt 1

  fromCql (CqlInt 1) = pure ReusableCode
  fromCql _ = Left "unknown Scope"

instance PostgresMarshall  Int32 Scope where
  postgresMarshall = todo

instance PostgresUnmarshall Int32 Scope where
  postgresUnmarshall = todo
