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

module Test.Wire.API.Golden.Generated.UserClientMap_20Int_user where

import Data.Id (ClientId (ClientId, client), Id (Id))
import Data.UUID qualified as UUID (fromString)
import GHC.Exts (IsList (fromList))
import Imports (Int, fromJust)
import Wire.API.Message (UserClientMap (..))

testObject_UserClientMap_20Int_user_1 :: UserClientMap Int
testObject_UserClientMap_20Int_user_1 =
  UserClientMap
    { userClientMap =
        fromList
          [ ( Id (fromJust (UUID.fromString "00000552-0000-7ae9-0000-5c84000004dd")),
              fromList
                [ (ClientId {client = "0"}, 0),
                  (ClientId {client = "1"}, -1),
                  (ClientId {client = "3"}, -3),
                  (ClientId {client = "4"}, 3)
                ]
            )
          ]
    }

testObject_UserClientMap_20Int_user_2 :: UserClientMap Int
testObject_UserClientMap_20Int_user_2 =
  UserClientMap
    { userClientMap =
        fromList
          [ ( Id (fromJust (UUID.fromString "00000025-0000-0025-0000-00660000001d")),
              fromList [(ClientId {client = "3"}, 5), (ClientId {client = "9"}, 0)]
            ),
            ( Id (fromJust (UUID.fromString "00000038-0000-004a-0000-007700000000")),
              fromList [(ClientId {client = "dfd"}, -8)]
            )
          ]
    }

testObject_UserClientMap_20Int_user_3 :: UserClientMap Int
testObject_UserClientMap_20Int_user_3 =
  UserClientMap
    { userClientMap =
        fromList
          [ ( Id (fromJust (UUID.fromString "0000000e-0000-001d-0000-00160000000b")),
              fromList [(ClientId {client = "0"}, 0), (ClientId {client = "1"}, 0)]
            ),
            ( Id (fromJust (UUID.fromString "00000013-0000-000d-0000-000600000007")),
              fromList [(ClientId {client = "4"}, -5), (ClientId {client = "9"}, 1)]
            ),
            ( Id (fromJust (UUID.fromString "00000018-0000-0020-0000-000e00000002")),
              fromList [(ClientId {client = "0"}, 0), (ClientId {client = "1"}, 0)]
            )
          ]
    }

testObject_UserClientMap_20Int_user_4 :: UserClientMap Int
testObject_UserClientMap_20Int_user_4 =
  UserClientMap
    { userClientMap =
        fromList
          [ (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000100000000")), fromList []),
            ( Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000500000002")),
              fromList [(ClientId {client = "0"}, 1), (ClientId {client = "1"}, 1)]
            ),
            ( Id (fromJust (UUID.fromString "00000003-0000-0005-0000-000800000001")),
              fromList [(ClientId {client = "0"}, 0), (ClientId {client = "1"}, 0)]
            ),
            (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000300000008")), fromList []),
            ( Id (fromJust (UUID.fromString "00000007-0000-0008-0000-000100000006")),
              fromList [(ClientId {client = "0"}, -2), (ClientId {client = "3"}, 1)]
            )
          ]
    }

testObject_UserClientMap_20Int_user_5 :: UserClientMap Int
testObject_UserClientMap_20Int_user_5 =
  UserClientMap
    { userClientMap =
        fromList
          [ ( Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000400000005")),
              fromList [(ClientId {client = "17"}, -7)]
            ),
            ( Id (fromJust (UUID.fromString "00000003-0000-0006-0000-000100000000")),
              fromList [(ClientId {client = "2"}, 4)]
            ),
            (Id (fromJust (UUID.fromString "00000006-0000-0008-0000-000600000003")), fromList []),
            ( Id (fromJust (UUID.fromString "00000008-0000-0008-0000-000300000008")),
              fromList [(ClientId {client = "0"}, 0), (ClientId {client = "1"}, 1)]
            )
          ]
    }

testObject_UserClientMap_20Int_user_6 :: UserClientMap Int
testObject_UserClientMap_20Int_user_6 =
  UserClientMap
    { userClientMap =
        fromList
          [ ( Id (fromJust (UUID.fromString "0000004d-0000-001f-0000-006300000073")),
              fromList [(ClientId {client = "1"}, 0), (ClientId {client = "2"}, -3), (ClientId {client = "4"}, -1)]
            ),
            ( Id (fromJust (UUID.fromString "0000007c-0000-0075-0000-006000000025")),
              fromList [(ClientId {client = "8"}, 0), (ClientId {client = "c"}, 3), (ClientId {client = "f"}, 1)]
            )
          ]
    }

testObject_UserClientMap_20Int_user_7 :: UserClientMap Int
testObject_UserClientMap_20Int_user_7 =
  UserClientMap
    { userClientMap =
        fromList
          [ ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000")),
              fromList [(ClientId {client = "0"}, 0)]
            ),
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")), fromList []),
            ( Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002")),
              fromList [(ClientId {client = "0"}, -2)]
            ),
            ( Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
              fromList [(ClientId {client = "0"}, 0), (ClientId {client = "1"}, 0)]
            ),
            ( Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001")),
              fromList [(ClientId {client = "0"}, -1), (ClientId {client = "1"}, 0)]
            ),
            ( Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001")),
              fromList [(ClientId {client = "2"}, 1)]
            ),
            (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002")), fromList [])
          ]
    }

testObject_UserClientMap_20Int_user_8 :: UserClientMap Int
testObject_UserClientMap_20Int_user_8 = UserClientMap {userClientMap = fromList []}

testObject_UserClientMap_20Int_user_9 :: UserClientMap Int
testObject_UserClientMap_20Int_user_9 =
  UserClientMap
    { userClientMap =
        fromList
          [ (Id (fromJust (UUID.fromString "00000054-0000-003b-0000-00210000005f")), fromList []),
            ( Id (fromJust (UUID.fromString "00000065-0000-0040-0000-005f00000064")),
              fromList [(ClientId {client = "0"}, 2), (ClientId {client = "1"}, -2), (ClientId {client = "2"}, -2)]
            )
          ]
    }

testObject_UserClientMap_20Int_user_10 :: UserClientMap Int
testObject_UserClientMap_20Int_user_10 =
  UserClientMap
    { userClientMap =
        fromList
          [ ( Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000200000003")),
              fromList [(ClientId {client = "b"}, -1)]
            ),
            (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000000000000")), fromList []),
            ( Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000400000000")),
              fromList [(ClientId {client = "0"}, 0), (ClientId {client = "1"}, -1)]
            ),
            ( Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000200000003")),
              fromList [(ClientId {client = "0"}, 0), (ClientId {client = "1"}, 0)]
            ),
            ( Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000300000003")),
              fromList [(ClientId {client = "7"}, -5)]
            ),
            ( Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000400000000")),
              fromList [(ClientId {client = "1"}, 2)]
            )
          ]
    }

testObject_UserClientMap_20Int_user_11 :: UserClientMap Int
testObject_UserClientMap_20Int_user_11 =
  UserClientMap
    { userClientMap =
        fromList
          [ ( Id (fromJust (UUID.fromString "00000001-0000-001b-0000-000200000004")),
              fromList [(ClientId {client = "38"}, -3)]
            ),
            ( Id (fromJust (UUID.fromString "00000007-0000-001b-0000-000700000017")),
              fromList [(ClientId {client = "1"}, 0), (ClientId {client = "2"}, 2), (ClientId {client = "3"}, -3)]
            ),
            ( Id (fromJust (UUID.fromString "0000000e-0000-001b-0000-000800000011")),
              fromList [(ClientId {client = "0"}, 0), (ClientId {client = "1"}, -1)]
            )
          ]
    }

testObject_UserClientMap_20Int_user_12 :: UserClientMap Int
testObject_UserClientMap_20Int_user_12 =
  UserClientMap
    { userClientMap =
        fromList
          [ ( Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000600000005")),
              fromList [(ClientId {client = "1"}, -2), (ClientId {client = "2"}, -1)]
            ),
            ( Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000700000006")),
              fromList [(ClientId {client = "5"}, 6)]
            ),
            ( Id (fromJust (UUID.fromString "00000007-0000-0000-0000-000000000005")),
              fromList [(ClientId {client = "3"}, 3), (ClientId {client = "4"}, -3)]
            ),
            ( Id (fromJust (UUID.fromString "00000007-0000-0000-0000-000100000005")),
              fromList [(ClientId {client = "0"}, 0), (ClientId {client = "1"}, 0)]
            ),
            ( Id (fromJust (UUID.fromString "00000008-0000-0007-0000-000000000005")),
              fromList [(ClientId {client = "0"}, 0), (ClientId {client = "1"}, 0)]
            )
          ]
    }

testObject_UserClientMap_20Int_user_13 :: UserClientMap Int
testObject_UserClientMap_20Int_user_13 =
  UserClientMap
    { userClientMap =
        fromList
          [ (Id (fromJust (UUID.fromString "00000005-0000-0005-0000-000400000004")), fromList []),
            ( Id (fromJust (UUID.fromString "00000008-0000-000d-0000-00040000000d")),
              fromList [(ClientId {client = "9"}, -3), (ClientId {client = "d"}, -5)]
            ),
            ( Id (fromJust (UUID.fromString "00000008-0000-0011-0000-000b0000000f")),
              fromList [(ClientId {client = "98"}, 1)]
            )
          ]
    }

testObject_UserClientMap_20Int_user_14 :: UserClientMap Int
testObject_UserClientMap_20Int_user_14 =
  UserClientMap
    { userClientMap =
        fromList
          [ ( Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000001")),
              fromList [(ClientId {client = "0"}, 0)]
            ),
            ( Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000300000004")),
              fromList [(ClientId {client = "0"}, 0), (ClientId {client = "1"}, 0)]
            ),
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002")), fromList []),
            ( Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000000000000")),
              fromList [(ClientId {client = "0"}, 1), (ClientId {client = "1"}, -1)]
            ),
            ( Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000100000004")),
              fromList [(ClientId {client = "0"}, 1), (ClientId {client = "1"}, -2)]
            ),
            ( Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000000000003")),
              fromList [(ClientId {client = "0"}, 0), (ClientId {client = "1"}, 0)]
            )
          ]
    }

testObject_UserClientMap_20Int_user_15 :: UserClientMap Int
testObject_UserClientMap_20Int_user_15 =
  UserClientMap
    { userClientMap =
        fromList
          [ (Id (fromJust (UUID.fromString "00000000-0000-0006-0000-000800000007")), fromList []),
            ( Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000500000000")),
              fromList [(ClientId {client = "c"}, -2)]
            ),
            ( Id (fromJust (UUID.fromString "00000005-0000-0000-0000-000600000002")),
              fromList [(ClientId {client = "1b"}, -7)]
            ),
            (Id (fromJust (UUID.fromString "00000008-0000-0004-0000-000000000002")), fromList [])
          ]
    }

testObject_UserClientMap_20Int_user_16 :: UserClientMap Int
testObject_UserClientMap_20Int_user_16 =
  UserClientMap
    { userClientMap =
        fromList
          [ (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")), fromList []),
            ( Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              fromList [(ClientId {client = "0"}, -1), (ClientId {client = "1"}, -1)]
            ),
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001")), fromList []),
            ( Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001")),
              fromList [(ClientId {client = "2"}, -3)]
            ),
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")), fromList []),
            ( Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
              fromList [(ClientId {client = "1"}, 2)]
            ),
            ( Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002")),
              fromList [(ClientId {client = "0"}, 0), (ClientId {client = "1"}, 0)]
            ),
            ( Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001")),
              fromList [(ClientId {client = "0"}, -1)]
            ),
            ( Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000")),
              fromList [(ClientId {client = "3"}, 1)]
            )
          ]
    }

testObject_UserClientMap_20Int_user_17 :: UserClientMap Int
testObject_UserClientMap_20Int_user_17 =
  UserClientMap
    { userClientMap =
        fromList
          [ ( Id (fromJust (UUID.fromString "00000000-0000-000f-0000-000400000014")),
              fromList [(ClientId {client = "0"}, 2), (ClientId {client = "1"}, 1)]
            ),
            ( Id (fromJust (UUID.fromString "00000001-0000-001c-0000-00170000001c")),
              fromList [(ClientId {client = "0"}, 2), (ClientId {client = "1"}, 0), (ClientId {client = "2"}, 0)]
            ),
            ( Id (fromJust (UUID.fromString "00000014-0000-0018-0000-001f00000002")),
              fromList [(ClientId {client = "0"}, 0), (ClientId {client = "1"}, 0)]
            )
          ]
    }

testObject_UserClientMap_20Int_user_18 :: UserClientMap Int
testObject_UserClientMap_20Int_user_18 =
  UserClientMap
    { userClientMap =
        fromList
          [ (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000800000008")), fromList []),
            ( Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000500000002")),
              fromList [(ClientId {client = "0"}, 0), (ClientId {client = "1"}, 0)]
            ),
            ( Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000100000001")),
              fromList [(ClientId {client = "2"}, -1), (ClientId {client = "4"}, 3)]
            ),
            ( Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000800000008")),
              fromList [(ClientId {client = "0"}, 1)]
            ),
            ( Id (fromJust (UUID.fromString "00000008-0000-0008-0000-000700000003")),
              fromList [(ClientId {client = "1"}, 2), (ClientId {client = "2"}, -2)]
            )
          ]
    }

testObject_UserClientMap_20Int_user_19 :: UserClientMap Int
testObject_UserClientMap_20Int_user_19 =
  UserClientMap
    { userClientMap =
        fromList
          [ (Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000800000002")), fromList []),
            (Id (fromJust (UUID.fromString "00000002-0000-0007-0000-000100000002")), fromList []),
            ( Id (fromJust (UUID.fromString "00000002-0000-0008-0000-000300000006")),
              fromList [(ClientId {client = "12"}, 6)]
            ),
            (Id (fromJust (UUID.fromString "00000002-0000-0008-0000-000300000008")), fromList [])
          ]
    }

testObject_UserClientMap_20Int_user_20 :: UserClientMap Int
testObject_UserClientMap_20Int_user_20 =
  UserClientMap
    { userClientMap =
        fromList
          [ (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")), fromList []),
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")), fromList []),
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")), fromList []),
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")), fromList []),
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")), fromList []),
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")), fromList []),
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")), fromList []),
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")), fromList []),
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")), fromList []),
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")), fromList []),
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")), fromList []),
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")), fromList []),
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")), fromList []),
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")), fromList []),
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")), fromList [])
          ]
    }
