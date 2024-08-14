{-# LANGUAGE OverloadedLists #-}

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

module Test.Wire.API.Golden.Generated.SearchResult_20TeamContact_user where

import Data.Id (Id (Id))
import Data.Json.Util (readUTCTimeMillis)
import Data.UUID qualified as UUID (fromString)
import Imports (Bool (False, True), Maybe (Just, Nothing), fromJust)
import Wire.API.Team.Role (Role (RoleAdmin, RoleExternalPartner, RoleMember, RoleOwner))
import Wire.API.User
import Wire.API.User.Search (FederatedUserSearchPolicy (ExactHandleSearch, FullSearch), PagingState (..), SearchResult (..), Sso (..), TeamContact (..))

testObject_SearchResult_20TeamContact_user_1 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_1 =
  SearchResult
    { searchFound = -4,
      searchReturned = 2,
      searchTook = 0,
      searchResults =
        [ TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Nothing,
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T20:48:17.263Z")),
              teamContactManagedBy = Nothing,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleAdmin,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T17:17:18.225Z")),
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Nothing,
              teamContactRole = Just RoleExternalPartner,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f"),
              teamContactEmailUnvalidated = Nothing
            }
        ],
      searchPolicy = FullSearch,
      searchPagingState = Just (PagingState "WzE2Njk5OTQ5MzIyNjdd"),
      searchHasMore = Nothing
    }

testObject_SearchResult_20TeamContact_user_2 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_2 =
  SearchResult
    { searchFound = -5,
      searchReturned = 4,
      searchTook = 6,
      searchResults = [],
      searchPolicy = FullSearch,
      searchPagingState = Nothing,
      searchHasMore = Just True
    }

testObject_SearchResult_20TeamContact_user_3 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_3 =
  SearchResult
    { searchFound = -5,
      searchReturned = -2,
      searchTook = -7,
      searchResults =
        [ TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              teamContactName = "",
              teamContactColorId = Nothing,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              teamContactEmail = Nothing,
              teamContactCreatedAt = Nothing,
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Nothing,
              teamContactRole = Just RoleAdmin,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Nothing,
              teamContactEmail = Nothing,
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T04:59:07.086Z")),
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Just "",
              teamContactRole = Nothing,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Nothing,
              teamContactEmail = Nothing,
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T05:39:37.370Z")),
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Just "",
              teamContactRole = Nothing,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            }
        ],
      searchPolicy = FullSearch,
      searchPagingState = Nothing,
      searchHasMore = Just False
    }

testObject_SearchResult_20TeamContact_user_4 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_4 =
  SearchResult
    { searchFound = -2,
      searchReturned = 4,
      searchTook = 2,
      searchResults =
        [ TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
              teamContactName = "",
              teamContactColorId = Nothing,
              teamContactHandle = Nothing,
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Nothing,
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Nothing,
              teamContactRole = Just RoleOwner,
              teamContactScimExternalId = Nothing,
              teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f"),
              teamContactEmailUnvalidated = Just (unsafeEmailAddress "some" "example")
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Nothing,
              teamContactEmail = Nothing,
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T01:29:06.597Z")),
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleAdmin,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f"),
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              teamContactName = "",
              teamContactColorId = Nothing,
              teamContactHandle = Nothing,
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              teamContactEmail = Nothing,
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T17:38:20.677Z")),
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleExternalPartner,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f"),
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Nothing,
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Nothing,
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleExternalPartner,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f"),
              teamContactEmailUnvalidated = Nothing
            }
        ],
      searchPolicy = ExactHandleSearch,
      searchPagingState = Nothing,
      searchHasMore = Nothing
    }

testObject_SearchResult_20TeamContact_user_5 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_5 =
  SearchResult
    { searchFound = -2,
      searchReturned = -3,
      searchTook = -7,
      searchResults =
        [ TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Nothing,
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T12:39:20.984Z")),
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleExternalPartner,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            }
        ],
      searchPolicy = FullSearch,
      searchPagingState = Nothing,
      searchHasMore = Nothing
    }

testObject_SearchResult_20TeamContact_user_6 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_6 =
  SearchResult
    { searchFound = -4,
      searchReturned = -7,
      searchTook = -4,
      searchResults =
        [ TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              teamContactEmail = Nothing,
              teamContactCreatedAt = Nothing,
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleOwner,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Nothing,
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Nothing,
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleOwner,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Just (unsafeEmailAddress "some" "example")
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
              teamContactName = "",
              teamContactColorId = Nothing,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T10:59:12.538Z")),
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Just "",
              teamContactRole = Nothing,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T23:24:12.000Z")),
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleOwner,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Nothing,
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Nothing,
              teamContactManagedBy = Nothing,
              teamContactSAMLIdp = Just "",
              teamContactRole = Nothing,
              teamContactScimExternalId = Nothing,
              teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f"),
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              teamContactName = "",
              teamContactColorId = Nothing,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              teamContactEmail = Nothing,
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T19:59:50.883Z")),
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleMember,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Nothing,
              teamContactEmail = Nothing,
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T13:56:02.433Z")),
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Nothing,
              teamContactRole = Just RoleAdmin,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Nothing,
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T01:45:42.970Z")),
              teamContactManagedBy = Nothing,
              teamContactSAMLIdp = Just "",
              teamContactRole = Nothing,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f"),
              teamContactEmailUnvalidated = Just (unsafeEmailAddress "some" "example")
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              teamContactName = "",
              teamContactColorId = Nothing,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              teamContactEmail = Nothing,
              teamContactCreatedAt = Nothing,
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Nothing,
              teamContactRole = Just RoleExternalPartner,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Just (unsafeEmailAddress "some" "example")
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T23:36:06.671Z")),
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleOwner,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Just (unsafeEmailAddress "some" "example")
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              teamContactEmail = Nothing,
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T14:01:50.906Z")),
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleMember,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Just (unsafeEmailAddress "some" "example")
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Nothing,
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleExternalPartner,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f"),
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Nothing,
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Nothing,
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Nothing,
              teamContactRole = Just RoleExternalPartner,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            }
        ],
      searchPolicy = FullSearch,
      searchPagingState = Nothing,
      searchHasMore = Nothing
    }

testObject_SearchResult_20TeamContact_user_7 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_7 =
  SearchResult
    { searchFound = 1,
      searchReturned = 5,
      searchTook = 5,
      searchResults =
        [ TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
              teamContactName = "",
              teamContactColorId = Nothing,
              teamContactHandle = Just "",
              teamContactTeam = Nothing,
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T19:22:39.660Z")),
              teamContactManagedBy = Nothing,
              teamContactSAMLIdp = Just "",
              teamContactRole = Nothing,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Nothing,
              teamContactEmail = Nothing,
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T19:42:55.525Z")),
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleMember,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Nothing,
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleExternalPartner,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Just (unsafeEmailAddress "some" "example")
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Nothing,
              teamContactEmail = Nothing,
              teamContactCreatedAt = Nothing,
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Just "",
              teamContactRole = Nothing,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Just (unsafeEmailAddress "some" "example")
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              teamContactEmail = Nothing,
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T00:45:08.016Z")),
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleMember,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Just (unsafeEmailAddress "some" "example")
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              teamContactEmail = Nothing,
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T21:18:46.647Z")),
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleExternalPartner,
              teamContactScimExternalId = Nothing,
              teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f"),
              teamContactEmailUnvalidated = Nothing
            }
        ],
      searchPolicy = FullSearch,
      searchPagingState = Nothing,
      searchHasMore = Nothing
    }

testObject_SearchResult_20TeamContact_user_8 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_8 =
  SearchResult
    { searchFound = 7,
      searchReturned = 2,
      searchTook = -7,
      searchResults =
        [ TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
              teamContactName = "",
              teamContactColorId = Nothing,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Nothing,
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleOwner,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Nothing,
              teamContactTeam = Nothing,
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T13:46:22.701Z")),
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Nothing,
              teamContactRole = Just RoleOwner,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T09:25:11.685Z")),
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Just "",
              teamContactRole = Nothing,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T11:37:20.763Z")),
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Just "",
              teamContactRole = Nothing,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            }
        ],
      searchPolicy = FullSearch,
      searchPagingState = Nothing,
      searchHasMore = Nothing
    }

testObject_SearchResult_20TeamContact_user_9 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_9 =
  SearchResult
    { searchFound = 2,
      searchReturned = 3,
      searchTook = -3,
      searchResults =
        [ TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Nothing,
              teamContactEmail = Nothing,
              teamContactCreatedAt = Nothing,
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleMember,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T16:22:05.429Z")),
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Just "",
              teamContactRole = Nothing,
              teamContactScimExternalId = Nothing,
              teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f"),
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              teamContactEmail = Nothing,
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T17:19:11.439Z")),
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleMember,
              teamContactScimExternalId = Nothing,
              teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f"),
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              teamContactName = "",
              teamContactColorId = Nothing,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              teamContactEmail = Nothing,
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T05:44:15.175Z")),
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleExternalPartner,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f"),
              teamContactEmailUnvalidated = Nothing
            }
        ],
      searchPolicy = FullSearch,
      searchPagingState = Nothing,
      searchHasMore = Nothing
    }

testObject_SearchResult_20TeamContact_user_10 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_10 =
  SearchResult
    { searchFound = -3,
      searchReturned = -3,
      searchTook = -4,
      searchResults = [],
      searchPolicy = FullSearch,
      searchPagingState = Nothing,
      searchHasMore = Nothing
    }

testObject_SearchResult_20TeamContact_user_11 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_11 =
  SearchResult
    { searchFound = -5,
      searchReturned = 7,
      searchTook = 1,
      searchResults =
        [ TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              teamContactName = "",
              teamContactColorId = Nothing,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              teamContactEmail = Nothing,
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T23:32:15.171Z")),
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleExternalPartner,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Nothing,
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T09:36:08.567Z")),
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleOwner,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              teamContactName = "",
              teamContactColorId = Nothing,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T11:56:16.082Z")),
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleAdmin,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              teamContactName = "",
              teamContactColorId = Nothing,
              teamContactHandle = Just "",
              teamContactTeam = Nothing,
              teamContactEmail = Nothing,
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T00:23:34.413Z")),
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Just "",
              teamContactRole = Nothing,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Nothing,
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Nothing,
              teamContactRole = Nothing,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              teamContactName = "",
              teamContactColorId = Nothing,
              teamContactHandle = Just "",
              teamContactTeam = Nothing,
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T02:39:28.838Z")),
              teamContactManagedBy = Nothing,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleExternalPartner,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Nothing,
              teamContactManagedBy = Nothing,
              teamContactSAMLIdp = Nothing,
              teamContactRole = Just RoleOwner,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T01:15:59.694Z")),
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Just "",
              teamContactRole = Nothing,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f"),
              teamContactEmailUnvalidated = Nothing
            }
        ],
      searchPolicy = FullSearch,
      searchPagingState = Nothing,
      searchHasMore = Nothing
    }

testObject_SearchResult_20TeamContact_user_12 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_12 =
  SearchResult
    { searchFound = 0,
      searchReturned = 0,
      searchTook = 0,
      searchResults =
        [ TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Nothing,
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T06:59:36.374Z")),
              teamContactManagedBy = Nothing,
              teamContactSAMLIdp = Just "",
              teamContactRole = Nothing,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f"),
              teamContactEmailUnvalidated = Nothing
            }
        ],
      searchPolicy = FullSearch,
      searchPagingState = Nothing,
      searchHasMore = Nothing
    }

testObject_SearchResult_20TeamContact_user_13 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_13 =
  SearchResult
    { searchFound = -6,
      searchReturned = 3,
      searchTook = 1,
      searchResults =
        [ TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Nothing,
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T17:55:15.951Z")),
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleMember,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              teamContactName = "",
              teamContactColorId = Nothing,
              teamContactHandle = Nothing,
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T05:08:55.558Z")),
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleMember,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T11:18:47.121Z")),
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Just "",
              teamContactRole = Nothing,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            }
        ],
      searchPolicy = FullSearch,
      searchPagingState = Nothing,
      searchHasMore = Nothing
    }

testObject_SearchResult_20TeamContact_user_14 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_14 =
  SearchResult
    { searchFound = 1,
      searchReturned = 4,
      searchTook = -4,
      searchResults =
        [ TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Nothing,
              teamContactTeam = Nothing,
              teamContactEmail = Nothing,
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T06:35:15.745Z")),
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleAdmin,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            }
        ],
      searchPolicy = FullSearch,
      searchPagingState = Nothing,
      searchHasMore = Nothing
    }

testObject_SearchResult_20TeamContact_user_15 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_15 =
  SearchResult
    { searchFound = 2,
      searchReturned = 6,
      searchTook = -6,
      searchResults =
        [ TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
              teamContactName = "",
              teamContactColorId = Nothing,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Nothing,
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Nothing,
              teamContactRole = Just RoleOwner,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f"),
              teamContactEmailUnvalidated = Nothing
            }
        ],
      searchPolicy = FullSearch,
      searchPagingState = Nothing,
      searchHasMore = Nothing
    }

testObject_SearchResult_20TeamContact_user_16 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_16 =
  SearchResult
    { searchFound = 2,
      searchReturned = 2,
      searchTook = -5,
      searchResults =
        [ TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Nothing,
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T23:38:23.560Z")),
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleAdmin,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f"),
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              teamContactName = "",
              teamContactColorId = Nothing,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Nothing,
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Nothing,
              teamContactRole = Just RoleAdmin,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            },
          TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
              teamContactName = "",
              teamContactColorId = Just 0,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T18:46:45.154Z")),
              teamContactManagedBy = Just ManagedByScim,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleAdmin,
              teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            }
        ],
      searchPolicy = FullSearch,
      searchPagingState = Nothing,
      searchHasMore = Nothing
    }

testObject_SearchResult_20TeamContact_user_17 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_17 =
  SearchResult
    { searchFound = -7,
      searchReturned = -5,
      searchTook = 4,
      searchResults =
        [ TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              teamContactName = "",
              teamContactColorId = Nothing,
              teamContactHandle = Just "",
              teamContactTeam = Nothing,
              teamContactEmail = Nothing,
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T02:22:14.746Z")),
              teamContactManagedBy = Just ManagedByWire,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleExternalPartner,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            }
        ],
      searchPolicy = FullSearch,
      searchPagingState = Nothing,
      searchHasMore = Nothing
    }

testObject_SearchResult_20TeamContact_user_18 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_18 =
  SearchResult
    { searchFound = 1,
      searchReturned = -7,
      searchTook = -7,
      searchResults =
        [ TeamContact
            { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              teamContactName = "",
              teamContactColorId = Nothing,
              teamContactHandle = Just "",
              teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              teamContactEmail = Just (unsafeEmailAddress "some" "example"),
              teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T12:35:16.437Z")),
              teamContactManagedBy = Nothing,
              teamContactSAMLIdp = Just "",
              teamContactRole = Just RoleOwner,
              teamContactScimExternalId = Nothing,
              teamContactSso = Nothing,
              teamContactEmailUnvalidated = Nothing
            }
        ],
      searchPolicy = FullSearch,
      searchPagingState = Nothing,
      searchHasMore = Nothing
    }

testObject_SearchResult_20TeamContact_user_19 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_19 =
  SearchResult
    { searchFound = -6,
      searchReturned = -1,
      searchTook = -2,
      searchResults = [],
      searchPolicy = FullSearch,
      searchPagingState = Nothing,
      searchHasMore = Nothing
    }

testObject_SearchResult_20TeamContact_user_20 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_20 =
  SearchResult
    { searchFound = -6,
      searchReturned = -5,
      searchTook = 1,
      searchResults = [],
      searchPolicy = FullSearch,
      searchPagingState = Nothing,
      searchHasMore = Nothing
    }
