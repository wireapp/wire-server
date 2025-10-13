{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.ConnectionStore
  ( ConnectionStore (..),
    -- * Operations
    insertConnection,
    updateConnection,
    updateConnectionStatus,
    lookupConnection,
    lookupLocalConnectionsPage,
    lookupRemoteConnectionsPage,
    lookupRelationWithHistory,
    lookupLocalConnections,
    lookupConnectionStatus,
    lookupConnectionStatus',
    lookupContactList,
    lookupContactListWithRelation,
    lookupLocalConnectionStatuses,
    lookupRemoteConnectionStatuses,
    lookupAllStatuses,
    remoteConnectedUsersPaginated,
    countConnections,
    deleteConnections,
    deleteRemoteConnections,
  )
where

import Cassandra (PageWithState, PagingState)
import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.Qualified (Local, Qualified, Remote)
import Data.Range
import Imports
import Polysemy
import Wire.API.Connection (Relation, RelationWithHistory, UserConnection)
import Wire.API.Routes.Internal.Brig.Connection (ConnectionStatus, ConnectionStatusV2)
import Wire.ConnectionStore.Types (ResultPage)
import Wire.Sem.Paging (Page, PagingBounds)
import Wire.Sem.Paging qualified as P

data ConnectionStore p m a where
  -- Insert/Update operations
  InsertConnection ::
    Local UserId ->
    Qualified UserId ->
    RelationWithHistory ->
    Qualified ConvId ->
    ConnectionStore p m UserConnection
  UpdateConnection ::
    UserConnection ->
    RelationWithHistory ->
    ConnectionStore p m UserConnection
  UpdateConnectionStatus ::
    Local UserId ->
    Qualified UserId ->
    RelationWithHistory ->
    ConnectionStore p m UTCTimeMillis
  -- Lookup operations
  LookupConnection ::
    Local UserId ->
    Qualified UserId ->
    ConnectionStore p m (Maybe UserConnection)
  LookupRelationWithHistory ::
    Local UserId ->
    Qualified UserId ->
    ConnectionStore p m (Maybe RelationWithHistory)
  LookupLocalConnections ::
    Local UserId ->
    Maybe UserId ->
    Range 1 500 Int32 ->
    ConnectionStore p m (ResultPage UserConnection)
  LookupLocalConnectionsPage ::
    Local UserId ->
    Maybe PagingState ->
    Range 1 1000 Int32 ->
    ConnectionStore p m (PageWithState UserConnection)
  LookupRemoteConnectionsPage ::
    Local UserId ->
    Maybe PagingState ->
    Int32 ->
    ConnectionStore p m (PageWithState UserConnection)
  -- Status operations
  LookupConnectionStatus ::
    [UserId] ->
    [UserId] ->
    ConnectionStore p m [ConnectionStatus]
  LookupConnectionStatus' ::
    [UserId] ->
    ConnectionStore p m [ConnectionStatus]
  LookupLocalConnectionStatuses ::
    [UserId] ->
    Local [UserId] ->
    ConnectionStore p m [ConnectionStatusV2]
  LookupRemoteConnectionStatuses ::
    [UserId] ->
    Remote [UserId] ->
    ConnectionStore p m [ConnectionStatusV2]
  LookupAllStatuses ::
    Local [UserId] ->
    ConnectionStore p m [ConnectionStatusV2]
  -- Contact list operations
  LookupContactList ::
    UserId ->
    ConnectionStore p m [UserId]
  LookupContactListWithRelation ::
    UserId ->
    ConnectionStore p m [(UserId, RelationWithHistory)]
  -- Pagination
  RemoteConnectedUsersPaginated ::
    Local UserId ->
    Maybe (P.PagingState p (Remote UserConnection)) ->
    PagingBounds p (Remote UserConnection) ->
    ConnectionStore p m (Page p (Remote UserConnection))
  -- Counting
  CountConnections ::
    Local UserId ->
    [Relation] ->
    ConnectionStore p m Int64
  -- Deletion
  DeleteConnections ::
    UserId ->
    ConnectionStore p m ()
  DeleteRemoteConnections ::
    Remote UserId ->
    Range 1 1000 [UserId] ->
    ConnectionStore p m ()

makeSem ''ConnectionStore
