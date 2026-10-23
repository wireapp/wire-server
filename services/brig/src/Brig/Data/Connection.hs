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

-- | Brig-side helpers around the 'Wire.ConnectionStore' effect. All CQL lives
-- in @Wire.ConnectionStore.Cassandra@; this module only re-exports the effect
-- operations and provides combinators that need brig-side context.
module Brig.Data.Connection
  ( -- * Re-exports (effect operations and conversions)
    module W,

    -- * Brig-side helpers
    updateConnection,
    lookupContactList,
    lookupLocalConnections,
  )
where

import Brig.Data.Types (ResultPage, cassandraResultPage)
import Data.Id (UserId)
import Data.Qualified (Local)
import Data.Range (Range)
import Imports hiding (local)
import Polysemy
import Polysemy.Input
import Wire.API.Connection
  ( RelationWithHistory (AcceptedWithHistory),
    UserConnection (..),
    relationDropHistory,
  )
import Wire.ConnectionStore hiding (lookupLocalConnections)
import Wire.ConnectionStore qualified as W
import Wire.Sem.Paging.Cassandra (InternalPaging)
import Wire.Util (qualifyLocal)

updateConnection ::
  ( Member (ConnectionStore InternalPaging) r,
    Member (Input (Local ())) r
  ) =>
  UserConnection ->
  RelationWithHistory ->
  Sem r UserConnection
updateConnection c status = do
  self <- qualifyLocal (ucFrom c)
  now <- updateConnectionStatus self (ucTo c) status
  pure $
    c
      { ucStatus = relationDropHistory status,
        ucLastUpdate = now
      }

-- | See 'lookupContactListWithRelation'.
lookupContactList ::
  (Member (ConnectionStore InternalPaging) r) =>
  UserId ->
  Sem r [UserId]
lookupContactList u =
  fst <$$> (filter ((== AcceptedWithHistory) . snd) <$> lookupContactListWithRelation u)

-- | For a given user 'A', lookup their outgoing connections (A -> X) to other users.
lookupLocalConnections ::
  (Member (ConnectionStore InternalPaging) r) =>
  Local UserId ->
  Maybe UserId ->
  Range 1 500 Int32 ->
  Sem r (ResultPage UserConnection)
lookupLocalConnections lfrom start size =
  cassandraResultPage <$> W.lookupLocalConnections lfrom start size
