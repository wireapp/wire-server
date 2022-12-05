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

module Test.Wire.API.Golden.Manual.SearchResultContact where

import Imports
import Test.Wire.API.Golden.Manual.Contact (testObject_Contact_1, testObject_Contact_2)
import Wire.API.User.Search (Contact (..), FederatedUserSearchPolicy (FullSearch), PagingState (..), SearchResult (..))

testObject_SearchResultContact_1 :: SearchResult Contact
testObject_SearchResultContact_1 =
  SearchResult
    { searchFound = 2,
      searchReturned = 2,
      searchTook = 100,
      searchResults = [testObject_Contact_1, testObject_Contact_2],
      searchPolicy = FullSearch,
      searchPagingState = Nothing,
      searchHasMore = Nothing
    }

testObject_SearchResultContact_2 :: SearchResult Contact
testObject_SearchResultContact_2 =
  SearchResult
    { searchFound = 2,
      searchReturned = 2,
      searchTook = 100,
      searchResults = [testObject_Contact_1, testObject_Contact_2],
      searchPolicy = FullSearch,
      searchPagingState = Just $ PagingState "WzE2Njk5OTQ5MzIyNjdd",
      searchHasMore = Just False
    }
