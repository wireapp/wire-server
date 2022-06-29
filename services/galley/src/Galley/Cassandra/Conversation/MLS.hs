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
import Cassandra.Settings (fromRow)
import Data.Time
import qualified Galley.Cassandra.Queries as Cql
import Galley.Data.Types
import Galley.Types
import Imports
import Wire.API.MLS.Message

acquireCommitLock :: GroupId -> Epoch -> NominalDiffTime -> Client LockAcquired
acquireCommitLock groupId epoch ttl = do
  rows <-
    retry x5 $
      trans
        Cql.acquireCommitLock
        ( params
            LocalQuorum
            (groupId, epoch, round ttl)
        )
  pure $
    if checkTransSuccess rows
      then Acquired
      else NotAcquired

releaseCommitLock :: GroupId -> Epoch -> Client ()
releaseCommitLock groupId epoch =
  retry x5 $
    write
      Cql.releaseCommitLock
      ( params
          LocalQuorum
          (groupId, epoch)
      )

checkTransSuccess :: [Row] -> Bool
checkTransSuccess [] = False
checkTransSuccess (row : _) = either (const False) (fromMaybe False) $ fromRow 0 row
