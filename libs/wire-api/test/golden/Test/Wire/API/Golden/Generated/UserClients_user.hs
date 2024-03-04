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

module Test.Wire.API.Golden.Generated.UserClients_user where

import Data.Id
import Data.UUID qualified as UUID (fromString)
import GHC.Exts (IsList (fromList))
import Imports (fromJust)
import Wire.API.Message (UserClients (..))

testObject_UserClients_user_1 :: UserClients
testObject_UserClients_user_1 =
  UserClients
    { userClients =
        fromList
          [ (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")), fromList []),
            ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
              fromList [ClientId 0, ClientId 1]
            ),
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")), fromList []),
            ( Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
              fromList [ClientId 0, ClientId 1]
            ),
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")), fromList []),
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")), fromList []),
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")), fromList []),
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")), fromList []),
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")), fromList []),
            ( Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              fromList [ClientId 1]
            ),
            ( Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              fromList [ClientId 0]
            )
          ]
    }

testObject_UserClients_user_2 :: UserClients
testObject_UserClients_user_2 =
  UserClients
    { userClients =
        fromList
          [ (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")), fromList []),
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")), fromList []),
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

testObject_UserClients_user_3 :: UserClients
testObject_UserClients_user_3 =
  UserClients
    { userClients =
        fromList
          [ ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              fromList [ClientId 0]
            ),
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")), fromList []),
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")), fromList []),
            ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
              fromList [ClientId 0]
            ),
            ( Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
              fromList [ClientId 1]
            ),
            ( Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              fromList [ClientId 1]
            ),
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")), fromList []),
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")), fromList []),
            ( Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              fromList [ClientId 0, ClientId 1]
            ),
            ( Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              fromList [ClientId 1]
            ),
            ( Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
              fromList [ClientId 0]
            )
          ]
    }

testObject_UserClients_user_4 :: UserClients
testObject_UserClients_user_4 = UserClients {userClients = fromList []}

testObject_UserClients_user_5 :: UserClients
testObject_UserClients_user_5 = UserClients {userClients = fromList []}

testObject_UserClients_user_6 :: UserClients
testObject_UserClients_user_6 = UserClients {userClients = fromList []}

testObject_UserClients_user_7 :: UserClients
testObject_UserClients_user_7 =
  UserClients
    { userClients =
        fromList
          [ (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")), fromList []),
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001")), fromList []),
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")), fromList []),
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")), fromList []),
            ( Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001")),
              fromList [ClientId 0]
            ),
            ( Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              fromList [ClientId 0, ClientId 1]
            ),
            ( Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002")),
              fromList [ClientId 2]
            ),
            ( Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002")),
              fromList [ClientId 2]
            ),
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000")), fromList []),
            ( Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002")),
              fromList [ClientId 0, ClientId 1]
            ),
            ( Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001")),
              fromList [ClientId 0, ClientId 1]
            )
          ]
    }

testObject_UserClients_user_8 :: UserClients
testObject_UserClients_user_8 =
  UserClients
    { userClients =
        fromList
          [ (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")), fromList []),
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000")), fromList []),
            ( Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              fromList [ClientId 1]
            ),
            ( Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              fromList [ClientId 3]
            ),
            ( Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000002")),
              fromList [ClientId 0, ClientId 1]
            ),
            ( Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000")),
              fromList [ClientId 2]
            ),
            ( Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000")),
              fromList [ClientId 3]
            ),
            ( Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001")),
              fromList [ClientId 0, ClientId 1]
            )
          ]
    }

testObject_UserClients_user_9 :: UserClients
testObject_UserClients_user_9 =
  UserClients
    { userClients =
        fromList
          [ ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002")),
              fromList [ClientId 0, ClientId 1]
            ),
            (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000300000003")), fromList []),
            ( Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000100000001")),
              fromList [ClientId 0xa]
            ),
            ( Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000200000004")),
              fromList [ClientId 2]
            ),
            (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000200000002")), fromList [])
          ]
    }

testObject_UserClients_user_10 :: UserClients
testObject_UserClients_user_10 =
  UserClients
    { userClients =
        fromList
          [ ( Id (fromJust (UUID.fromString "00007d35-0000-33bd-0000-377d000074fb")),
              fromList [ClientId 0x9824c7]
            )
          ]
    }

testObject_UserClients_user_11 :: UserClients
testObject_UserClients_user_11 =
  UserClients
    { userClients =
        fromList
          [ ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002")),
              fromList [ClientId 2]
            ),
            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")), fromList []),
            ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001")),
              fromList [ClientId 0]
            ),
            ( Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              fromList [ClientId 0]
            ),
            ( Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001")),
              fromList [ClientId 0, ClientId 1]
            ),
            ( Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
              fromList [ClientId 2]
            ),
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")), fromList []),
            ( Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002")),
              fromList [ClientId 0]
            ),
            ( Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000")),
              fromList [ClientId 0, ClientId 1]
            ),
            ( Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001")),
              fromList [ClientId 0, ClientId 1]
            )
          ]
    }

testObject_UserClients_user_12 :: UserClients
testObject_UserClients_user_12 =
  UserClients
    { userClients =
        fromList
          [ ( Id (fromJust (UUID.fromString "00000731-0000-23a5-0000-23bc00003dd5")),
              fromList
                [ ClientId 1,
                  ClientId 7,
                  ClientId 8,
                  ClientId 0xb,
                  ClientId 0xe
                ]
            )
          ]
    }

testObject_UserClients_user_13 :: UserClients
testObject_UserClients_user_13 =
  UserClients
    { userClients =
        fromList
          [ (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000d00000008")), fromList []),
            (Id (fromJust (UUID.fromString "00000005-0000-001d-0000-000c0000001c")), fromList []),
            ( Id (fromJust (UUID.fromString "0000000e-0000-000a-0000-00160000001a")),
              fromList [ClientId 1, ClientId 2]
            )
          ]
    }

testObject_UserClients_user_14 :: UserClients
testObject_UserClients_user_14 =
  UserClients
    { userClients =
        fromList
          [ ( Id (fromJust (UUID.fromString "00001130-0000-01e5-0000-3c24000015c6")),
              fromList [ClientId 0x18, ClientId 0x1f, ClientId 0xa]
            )
          ]
    }

testObject_UserClients_user_15 :: UserClients
testObject_UserClients_user_15 =
  UserClients
    { userClients =
        fromList
          [ ( Id (fromJust (UUID.fromString "000004e9-0000-307b-0000-1ab300006689")),
              fromList [ClientId 0x6c, ClientId 0x946]
            )
          ]
    }

testObject_UserClients_user_16 :: UserClients
testObject_UserClients_user_16 =
  UserClients
    { userClients =
        fromList
          [ (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000700000007")), fromList []),
            ( Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000600000007")),
              fromList [ClientId 0, ClientId 1]
            ),
            ( Id (fromJust (UUID.fromString "00000002-0000-0008-0000-000300000000")),
              fromList [ClientId 2, ClientId 4]
            ),
            (Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000100000006")), fromList [])
          ]
    }

testObject_UserClients_user_17 :: UserClients
testObject_UserClients_user_17 =
  UserClients
    { userClients =
        fromList
          [ ( Id (fromJust (UUID.fromString "00000011-0000-0007-0000-000f0000001c")),
              fromList [ClientId 4, ClientId 0xc]
            ),
            (Id (fromJust (UUID.fromString "0000001f-0000-000b-0000-001700000005")), fromList []),
            ( Id (fromJust (UUID.fromString "0000001f-0000-0011-0000-000800000010")),
              fromList [ClientId 2, ClientId 3, ClientId 4]
            )
          ]
    }

testObject_UserClients_user_18 :: UserClients
testObject_UserClients_user_18 =
  UserClients
    { userClients =
        fromList
          [ ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              fromList [ClientId 1]
            ),
            ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
              fromList [ClientId 1]
            ),
            ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
              fromList [ClientId 0, ClientId 1]
            ),
            ( Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              fromList [ClientId 0]
            ),
            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")), fromList []),
            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")), fromList []),
            ( Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              fromList [ClientId 0]
            ),
            ( Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              fromList [ClientId 1]
            ),
            ( Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
              fromList [ClientId 1]
            ),
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")), fromList []),
            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")), fromList [])
          ]
    }

testObject_UserClients_user_19 :: UserClients
testObject_UserClients_user_19 =
  UserClients
    { userClients = fromList [(Id (fromJust (UUID.fromString "000025db-0000-66c4-0000-7f3f00001ba5")), fromList [])]
    }

testObject_UserClients_user_20 :: UserClients
testObject_UserClients_user_20 = UserClients {userClients = fromList []}
