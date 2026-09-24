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

-- | ElasticSearch adapter for 'UserSearchStore'.
--
-- Delegates to the 'IndexedUserStore' effect (the ElasticSearch-backed
-- implementation), converting 'UserDoc' results to contacts exactly like the
-- pre-rework local search did.  This keeps ElasticSearch the default search
-- backend while 'Wire.UserSearchStore.Postgres' is being rolled out.
module Wire.UserSearchStore.ElasticSearch
  ( interpretUserSearchStoreElasticSearch,
  )
where

import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.API.User (fromName)
import Wire.API.User.Search
import Wire.IndexedUserStore (IndexedUserStore)
import Wire.IndexedUserStore qualified as IndexedUserStore
import Wire.UserSearch.Types
import Wire.UserSearchStore

interpretUserSearchStoreElasticSearch ::
  (Member IndexedUserStore r) =>
  InterpreterFor UserSearchStore r
interpretUserSearchStoreElasticSearch = interpret \case
  SearchUsers luid mtid info q n mtypes ->
    fmap (fmap (docToContact luid)) (IndexedUserStore.searchUsers (tUnqualified luid) mtid info q n mtypes)
  PaginateTeamMembers filters maxResults paging ->
    fmap (userDocToTeamContact []) <$> IndexedUserStore.paginateTeamMembers filters maxResults paging
  GetTeamSize tid -> IndexedUserStore.getTeamSize tid
  SearchUsersFederated {} ->
    error "Wire.UserSearchStore.ElasticSearch: federated search goes through Brig.User.Search.SearchIndex when searchBackend=elasticsearch"
  SetTeamSearchVisibilityInbound tid vis ->
    IndexedUserStore.updateTeamSearchVisibilityInbound tid vis

docToContact :: Local UserId -> UserDoc -> Contact
docToContact luid userDoc =
  runIdentity $
    userDocToContact
      (tUntagged $ qualifyAs luid userDoc.udId)
      (Identity . maybe "" fromName)
      userDoc
