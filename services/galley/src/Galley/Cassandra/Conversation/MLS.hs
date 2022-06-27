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

module Galley.Cassandra.Conversation.MLS where

import Cassandra
import Data.Id
import Data.Time
import qualified Galley.Cassandra.Queries as Cql
import Galley.Data.Types
import Imports

acquireCommitLock :: ConvId -> NominalDiffTime -> Client LockAcquired
acquireCommitLock convId ttl = do
  rows <-
    retry x5 $
      trans
        Cql.acquireCommitLock
        ( params
            LocalQuorum
            (convId, round ttl)
        )
  pure $
    if null rows
      then NotAcquired
      else Acquired

releaseCommitLock :: ConvId -> Client ()
releaseCommitLock convId =
  retry x5 $
    write
      Cql.releaseCommitLock
      ( params
          LocalQuorum
          (Identity convId)
      )
