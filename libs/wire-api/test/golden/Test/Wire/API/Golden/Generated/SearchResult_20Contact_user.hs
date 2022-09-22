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

module Test.Wire.API.Golden.Generated.SearchResult_20Contact_user where

import Data.Domain (Domain (Domain, _domainText))
import Data.Id (Id (Id))
import Data.Qualified (Qualified (Qualified, qDomain, qUnqualified))
import qualified Data.UUID as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust)
import Wire.API.User.Search (Contact (..), FederatedUserSearchPolicy (ExactHandleSearch, FullSearch), SearchResult (..))

testObject_SearchResult_20Contact_user_1 :: SearchResult Contact
testObject_SearchResult_20Contact_user_1 =
  SearchResult {searchFound = -6, searchReturned = 0, searchTook = 1, searchResults = [], searchPolicy = FullSearch}

testObject_SearchResult_20Contact_user_2 :: SearchResult Contact
testObject_SearchResult_20Contact_user_2 =
  SearchResult {searchFound = -4, searchReturned = 6, searchTook = -5, searchResults = [], searchPolicy = FullSearch}

testObject_SearchResult_20Contact_user_3 :: SearchResult Contact
testObject_SearchResult_20Contact_user_3 =
  SearchResult
    { searchFound = 4,
      searchReturned = 0,
      searchTook = 7,
      searchResults =
        [ Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
                    qDomain = Domain {_domainText = "guh.e"}
                  },
              contactName = "",
              contactColorId = Nothing,
              contactHandle = Just "",
              contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
            }
        ],
      searchPolicy = FullSearch
    }

testObject_SearchResult_20Contact_user_4 :: SearchResult Contact
testObject_SearchResult_20Contact_user_4 =
  SearchResult
    { searchFound = -5,
      searchReturned = -7,
      searchTook = 3,
      searchResults =
        [ Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
                    qDomain = Domain {_domainText = "2.60--1n1.ds"}
                  },
              contactName = "",
              contactColorId = Nothing,
              contactHandle = Nothing,
              contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
                    qDomain = Domain {_domainText = "onrg.u"}
                  },
              contactName = "",
              contactColorId = Nothing,
              contactHandle = Just "",
              contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
                    qDomain = Domain {_domainText = "660.v1.8z2.a-4dv.y"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Nothing,
              contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
                    qDomain = Domain {_domainText = "t102d9m3.tb-dryc9.ws300w5xc4"}
                  },
              contactName = "",
              contactColorId = Nothing,
              contactHandle = Just "",
              contactTeam = Nothing
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
                    qDomain = Domain {_domainText = "54up.l8h-b-g-i.x-c.9-7.we35781l0b"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Just "",
              contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
                    qDomain = Domain {_domainText = "a.h9-1"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Just "",
              contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
            }
        ],
      searchPolicy = FullSearch
    }

testObject_SearchResult_20Contact_user_5 :: SearchResult Contact
testObject_SearchResult_20Contact_user_5 =
  SearchResult
    { searchFound = -6,
      searchReturned = -6,
      searchTook = -1,
      searchResults =
        [ Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
                    qDomain = Domain {_domainText = "1b-y90e265f.l-c"}
                  },
              contactName = "z",
              contactColorId = Just 1,
              contactHandle = Just "",
              contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
            }
        ],
      searchPolicy = FullSearch
    }

testObject_SearchResult_20Contact_user_6 :: SearchResult Contact
testObject_SearchResult_20Contact_user_6 =
  SearchResult {searchFound = -5, searchReturned = -4, searchTook = 5, searchResults = [], searchPolicy = FullSearch}

testObject_SearchResult_20Contact_user_7 :: SearchResult Contact
testObject_SearchResult_20Contact_user_7 =
  SearchResult
    { searchFound = 7,
      searchReturned = 0,
      searchTook = -6,
      searchResults =
        [ Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
                    qDomain = Domain {_domainText = "1386---3-nddry.o"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Just "",
              contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
                    qDomain = Domain {_domainText = "j-cz923pu.l6.73-6.qq05n.4ig.dl3"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Nothing,
              contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            }
        ],
      searchPolicy = FullSearch
    }

testObject_SearchResult_20Contact_user_8 :: SearchResult Contact
testObject_SearchResult_20Contact_user_8 =
  SearchResult
    { searchFound = -7,
      searchReturned = -5,
      searchTook = -7,
      searchResults =
        [ Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
                    qDomain = Domain {_domainText = "6n.n08ejr-a"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Just "",
              contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            }
        ],
      searchPolicy = FullSearch
    }

testObject_SearchResult_20Contact_user_9 :: SearchResult Contact
testObject_SearchResult_20Contact_user_9 =
  SearchResult {searchFound = -5, searchReturned = -6, searchTook = 3, searchResults = [], searchPolicy = FullSearch}

testObject_SearchResult_20Contact_user_10 :: SearchResult Contact
testObject_SearchResult_20Contact_user_10 =
  SearchResult {searchFound = 0, searchReturned = -7, searchTook = -5, searchResults = [], searchPolicy = FullSearch}

testObject_SearchResult_20Contact_user_11 :: SearchResult Contact
testObject_SearchResult_20Contact_user_11 =
  SearchResult
    { searchFound = -1,
      searchReturned = 3,
      searchTook = -7,
      searchResults =
        [ Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
                    qDomain = Domain {_domainText = "bza.j"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Nothing,
              contactTeam = Nothing
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
                    qDomain = Domain {_domainText = "zwv.u6-f"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Just "",
              contactTeam = Nothing
            }
        ],
      searchPolicy = ExactHandleSearch
    }

testObject_SearchResult_20Contact_user_12 :: SearchResult Contact
testObject_SearchResult_20Contact_user_12 =
  SearchResult {searchFound = 7, searchReturned = 5, searchTook = 3, searchResults = [], searchPolicy = ExactHandleSearch}

testObject_SearchResult_20Contact_user_13 :: SearchResult Contact
testObject_SearchResult_20Contact_user_13 =
  SearchResult
    { searchFound = 3,
      searchReturned = 2,
      searchTook = -1,
      searchResults =
        [ Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
                    qDomain = Domain {_domainText = "795n1zf6-he8-97ur4w.o7r---053"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Just "",
              contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
                    qDomain = Domain {_domainText = "v-t6qc.e.so7jqwv"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Just "",
              contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
                    qDomain = Domain {_domainText = "335.a3.p49c--e-fjz337"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Just "",
              contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
                    qDomain = Domain {_domainText = "g.g3n"}
                  },
              contactName = "",
              contactColorId = Nothing,
              contactHandle = Just "",
              contactTeam = Nothing
            }
        ],
      searchPolicy = ExactHandleSearch
    }

testObject_SearchResult_20Contact_user_14 :: SearchResult Contact
testObject_SearchResult_20Contact_user_14 =
  SearchResult
    { searchFound = 1,
      searchReturned = 6,
      searchTook = 2,
      searchResults =
        [ Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
                    qDomain = Domain {_domainText = "c00y0ks9-6.q"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Just "",
              contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
                    qDomain = Domain {_domainText = "g.44.s3dq77"}
                  },
              contactName = "",
              contactColorId = Nothing,
              contactHandle = Just "",
              contactTeam = Nothing
            }
        ],
      searchPolicy = ExactHandleSearch
    }

testObject_SearchResult_20Contact_user_15 :: SearchResult Contact
testObject_SearchResult_20Contact_user_15 =
  SearchResult {searchFound = 3, searchReturned = 2, searchTook = 4, searchResults = [], searchPolicy = ExactHandleSearch}

testObject_SearchResult_20Contact_user_16 :: SearchResult Contact
testObject_SearchResult_20Contact_user_16 =
  SearchResult {searchFound = -4, searchReturned = 4, searchTook = -7, searchResults = [], searchPolicy = ExactHandleSearch}

testObject_SearchResult_20Contact_user_17 :: SearchResult Contact
testObject_SearchResult_20Contact_user_17 =
  SearchResult {searchFound = 6, searchReturned = -1, searchTook = -1, searchResults = [], searchPolicy = ExactHandleSearch}

testObject_SearchResult_20Contact_user_18 :: SearchResult Contact
testObject_SearchResult_20Contact_user_18 =
  SearchResult {searchFound = -4, searchReturned = 0, searchTook = -5, searchResults = [], searchPolicy = ExactHandleSearch}

testObject_SearchResult_20Contact_user_19 :: SearchResult Contact
testObject_SearchResult_20Contact_user_19 =
  SearchResult
    { searchFound = 4,
      searchReturned = 2,
      searchTook = -5,
      searchResults =
        [ Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
                    qDomain = Domain {_domainText = "5de.v-6"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Just "",
              contactTeam = Nothing
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
                    qDomain = Domain {_domainText = "z76.kcuxql-9"}
                  },
              contactName = "",
              contactColorId = Nothing,
              contactHandle = Just "",
              contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
            }
        ],
      searchPolicy = ExactHandleSearch
    }

testObject_SearchResult_20Contact_user_20 :: SearchResult Contact
testObject_SearchResult_20Contact_user_20 =
  SearchResult
    { searchFound = 7,
      searchReturned = 6,
      searchTook = -1,
      searchResults =
        [ Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
                    qDomain = Domain {_domainText = "66h.j"}
                  },
              contactName = "",
              contactColorId = Nothing,
              contactHandle = Just "",
              contactTeam = Nothing
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
                    qDomain = Domain {_domainText = "7s.k881-q-42"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Just "",
              contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
                    qDomain = Domain {_domainText = "1ux.dy"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Just "",
              contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
                    qDomain = Domain {_domainText = "o.xi"}
                  },
              contactName = "",
              contactColorId = Nothing,
              contactHandle = Just "",
              contactTeam = Nothing
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
                    qDomain = Domain {_domainText = "x5c.v"}
                  },
              contactName = "",
              contactColorId = Nothing,
              contactHandle = Just "",
              contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
                    qDomain = Domain {_domainText = "9p-8z5.i"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Just "",
              contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
                    qDomain = Domain {_domainText = "h1t7.9.j492"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Just "",
              contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
                    qDomain = Domain {_domainText = "p9.y"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Just "",
              contactTeam = Nothing
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
                    qDomain = Domain {_domainText = "saz.d0v8"}
                  },
              contactName = "",
              contactColorId = Nothing,
              contactHandle = Just "",
              contactTeam = Nothing
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
                    qDomain = Domain {_domainText = "gpz.28--u.1646.v5"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Just "",
              contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
                    qDomain = Domain {_domainText = "8p.5.x11-s"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Just "",
              contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
                    qDomain = Domain {_domainText = "q4x5z.mwi3"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Just "",
              contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
            },
          Contact
            { contactQualifiedId =
                Qualified
                  { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
                    qDomain = Domain {_domainText = "38.b7"}
                  },
              contactName = "",
              contactColorId = Just 0,
              contactHandle = Nothing,
              contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            }
        ],
      searchPolicy = ExactHandleSearch
    }
