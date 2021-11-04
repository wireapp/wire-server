-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Cassandra.ConversationList
  ( interpretConversationListToCassandra,
    interpretRemoteConversationListToCassandra,
    interpretLegacyConversationListToCassandra,
  )
where

import Cassandra
import Data.Id
import Data.Qualified
import Data.Range
import Galley.Cassandra.Paging
import Galley.Cassandra.Store
import Galley.Data.Instances ()
import qualified Galley.Data.Queries as Cql
import Galley.Data.ResultSet
import Galley.Effects.ListItems
import Imports hiding (max)
import Polysemy
import qualified Polysemy.Reader as P

-- | Deprecated, use 'localConversationIdsPageFrom'
conversationIdsFrom ::
  UserId ->
  Maybe ConvId ->
  Range 1 1000 Int32 ->
  Client (ResultSet ConvId)
conversationIdsFrom usr start (fromRange -> max) =
  mkResultSet . strip . fmap runIdentity <$> case start of
    Just c -> paginate Cql.selectUserConvsFrom (paramsP LocalQuorum (usr, c) (max + 1))
    Nothing -> paginate Cql.selectUserConvs (paramsP LocalQuorum (Identity usr) (max + 1))
  where
    strip p = p {result = take (fromIntegral max) (result p)}

localConversationIdsPageFrom ::
  UserId ->
  Maybe PagingState ->
  Range 1 1000 Int32 ->
  Client (PageWithState ConvId)
localConversationIdsPageFrom usr pagingState (fromRange -> max) =
  fmap runIdentity <$> paginateWithState Cql.selectUserConvs (paramsPagingState LocalQuorum (Identity usr) max pagingState)

remoteConversationIdsPageFrom ::
  UserId ->
  Maybe PagingState ->
  Int32 ->
  Client (PageWithState (Remote ConvId))
remoteConversationIdsPageFrom usr pagingState max =
  uncurry toRemoteUnsafe <$$> paginateWithState Cql.selectUserRemoteConvs (paramsPagingState LocalQuorum (Identity usr) max pagingState)

interpretConversationListToCassandra ::
  Members '[Embed IO, P.Reader ClientState] r =>
  Sem (ListItems CassandraPaging ConvId ': r) a ->
  Sem r a
interpretConversationListToCassandra = interpret $ \case
  ListItems uid ps max -> embedClient $ localConversationIdsPageFrom uid ps max

interpretRemoteConversationListToCassandra ::
  Members '[Embed IO, P.Reader ClientState] r =>
  Sem (ListItems CassandraPaging (Remote ConvId) ': r) a ->
  Sem r a
interpretRemoteConversationListToCassandra = interpret $ \case
  ListItems uid ps max -> embedClient $ remoteConversationIdsPageFrom uid ps (fromRange max)

interpretLegacyConversationListToCassandra ::
  Members '[Embed IO, P.Reader ClientState] r =>
  Sem (ListItems LegacyPaging ConvId ': r) a ->
  Sem r a
interpretLegacyConversationListToCassandra = interpret $ \case
  ListItems uid ps max -> embedClient $ conversationIdsFrom uid ps max
