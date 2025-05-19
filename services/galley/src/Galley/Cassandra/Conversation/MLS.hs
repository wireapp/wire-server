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

module Galley.Cassandra.Conversation.MLS
  ( acquireCommitLock,
    releaseCommitLock,
    lookupMLSClients,
    lookupMLSClientLeafIndices,
  )
where

import Cassandra
import Cassandra.Settings
import Control.Arrow
import Data.Time
import Galley.API.MLS.Types
import Galley.Cassandra.Queries qualified as Cql
import Galley.Data.Types
import Imports
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.MLS.LeafNode

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
          { serialConsistency = Just LocalSerialConsistency
          }
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

lookupMLSClientLeafIndices :: GroupId -> Client (ClientMap LeafIndex, IndexMap)
lookupMLSClientLeafIndices groupId = do
  entries <- retry x5 (query Cql.lookupMLSClients (params LocalQuorum (Identity groupId)))
  pure $ (mkClientMap &&& mkIndexMap) entries

lookupMLSClients :: GroupId -> Client (ClientMap LeafIndex)
lookupMLSClients = fmap fst . lookupMLSClientLeafIndices
