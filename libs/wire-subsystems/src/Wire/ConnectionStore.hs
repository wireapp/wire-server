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
{-# LANGUAGE TemplateHaskell #-}

module Wire.ConnectionStore where

import Cassandra qualified as Cas
import Data.Domain (Domain)
import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.Qualified
import Data.Range
import Imports
import Polysemy
import Wire.API.Connection
import Wire.API.Routes.Internal.Brig.Connection
import Wire.Sem.Paging (Page, PagingBounds, PagingState)

-- | One constructor per CQL-issuing operation of the (former)
-- 'Brig.Data.Connection' module.
data ConnectionStore p m a where
  InsertConnection ::
    Local UserId ->
    Qualified UserId ->
    RelationWithHistory ->
    Qualified ConvId ->
    ConnectionStore p m UserConnection
  UpdateConnectionStatus ::
    Local UserId ->
    Qualified UserId ->
    RelationWithHistory ->
    ConnectionStore p m UTCTimeMillis
  DeleteConnections :: UserId -> ConnectionStore p m ()
  DeleteRemoteConnections :: Remote UserId -> Range 1 1000 [UserId] -> ConnectionStore p m ()
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
    ConnectionStore p m (Cas.Page UserConnection)
  LookupLocalConnectionsPage ::
    Local UserId ->
    Maybe Cas.PagingState ->
    Range 1 1000 Int32 ->
    ConnectionStore p m (Cas.PageWithState ByteString UserConnection)
  LookupRemoteConnectionsPage ::
    Local UserId ->
    Maybe Cas.PagingState ->
    Int32 ->
    ConnectionStore p m (Cas.PageWithState ByteString UserConnection)
  LookupConnectionStatus :: [UserId] -> [UserId] -> ConnectionStore p m [ConnectionStatus]
  LookupConnectionStatus' :: [UserId] -> ConnectionStore p m [ConnectionStatus]
  LookupLocalConnectionStatuses ::
    [UserId] -> Local [UserId] -> ConnectionStore p m [ConnectionStatusV2]
  LookupRemoteConnectionStatuses ::
    [UserId] -> Remote [UserId] -> ConnectionStore p m [ConnectionStatusV2]
  LookupAllStatuses :: Local [UserId] -> ConnectionStore p m [ConnectionStatusV2]
  RemoteConnectedUsersPaginated ::
    Local UserId ->
    Maybe (PagingState p (Remote UserConnection)) ->
    PagingBounds p (Remote UserConnection) ->
    ConnectionStore p m (Page p (Remote UserConnection))
  LookupContactListWithRelation ::
    UserId -> ConnectionStore p m [(UserId, RelationWithHistory)]
  CountConnections :: Local UserId -> [Relation] -> ConnectionStore p m Int64

makeSem ''ConnectionStore

-- Conversions shared by the Cassandra and Postgres interpreters.

toLocalUserConnection ::
  Local x ->
  (UserId, UserId, RelationWithHistory, UTCTimeMillis, Maybe ConvId) ->
  UserConnection
toLocalUserConnection loc (l, r, relationDropHistory -> rel, time, cid) =
  UserConnection l (tUntagged (qualifyAs loc r)) rel time (fmap (tUntagged . qualifyAs loc) cid)

toRemoteUserConnection ::
  Local UserId ->
  (Domain, UserId, RelationWithHistory, UTCTimeMillis, Domain, ConvId) ->
  UserConnection
toRemoteUserConnection l (rDomain, r, relationDropHistory -> rel, time, cDomain, cid) =
  UserConnection (tUnqualified l) (Qualified r rDomain) rel time (Just $ Qualified cid cDomain)

toConnectionStatus :: (UserId, UserId, RelationWithHistory) -> ConnectionStatus
toConnectionStatus (l, r, relationDropHistory -> rel) = ConnectionStatus l r rel

toConnectionStatusV2 :: UserId -> Domain -> UserId -> RelationWithHistory -> ConnectionStatusV2
toConnectionStatusV2 from toDomain toUser relWithHistory =
  ConnectionStatusV2 from (Qualified toUser toDomain) (relationDropHistory relWithHistory)
