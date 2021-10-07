{-# LANGUAGE OverloadedLists #-}

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

module Test.Wire.API.Golden.Manual.UserClientPrekeyMap where

import Data.Id
import qualified Data.UUID as UUID (fromString)
import GHC.Exts (IsList (fromList))
import Imports
import Test.Wire.API.Golden.Generated.Prekey_user
import Wire.API.User.Client (UserClientPrekeyMap, mkUserClientPrekeyMap)

testObject_UserClientPrekeyMap_1 :: UserClientPrekeyMap
testObject_UserClientPrekeyMap_1 =
  (mkUserClientPrekeyMap . fromList)
    [ ( (Id (fromJust (UUID.fromString "00000552-0000-7ae9-0000-5c84000004dd"))),
        fromList
          [ (ClientId {client = "0"}, Just testObject_Prekey_user_1),
            (ClientId {client = "1"}, Just testObject_Prekey_user_2),
            (ClientId {client = "3"}, Nothing),
            (ClientId {client = "4"}, Just testObject_Prekey_user_14)
          ]
      )
    ]

testObject_UserClientPrekeyMap_2 :: UserClientPrekeyMap
testObject_UserClientPrekeyMap_2 =
  (mkUserClientPrekeyMap . fromList)
    [ ( (Id (fromJust (UUID.fromString "00000025-0000-0025-0000-00660000001d"))),
        fromList [(ClientId {client = "3"}, Just testObject_Prekey_user_7), (ClientId {client = "9"}, Just testObject_Prekey_user_9)]
      ),
      ( (Id (fromJust (UUID.fromString "00000038-0000-004a-0000-007700000000"))),
        fromList [(ClientId {client = "dfd"}, Just testObject_Prekey_user_15)]
      )
    ]

testObject_UserClientPrekeyMap_3 :: UserClientPrekeyMap
testObject_UserClientPrekeyMap_3 =
  (mkUserClientPrekeyMap . fromList)
    [ ( (Id (fromJust (UUID.fromString "0000000e-0000-001d-0000-00160000000b"))),
        fromList [(ClientId {client = "0"}, Just testObject_Prekey_user_11), (ClientId {client = "1"}, Just testObject_Prekey_user_1)]
      ),
      ( (Id (fromJust (UUID.fromString "00000013-0000-000d-0000-000600000007"))),
        fromList [(ClientId {client = "4"}, Just testObject_Prekey_user_6), (ClientId {client = "9"}, Just testObject_Prekey_user_8)]
      ),
      ( (Id (fromJust (UUID.fromString "00000018-0000-0020-0000-000e00000002"))),
        fromList [(ClientId {client = "0"}, Just testObject_Prekey_user_10), (ClientId {client = "1"}, Just testObject_Prekey_user_5)]
      )
    ]

testObject_UserClientPrekeyMap_4 :: UserClientPrekeyMap
testObject_UserClientPrekeyMap_4 =
  (mkUserClientPrekeyMap . fromList)
    [ ((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000100000000"))), fromList []),
      ( (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000500000002"))),
        fromList [(ClientId {client = "0"}, Just testObject_Prekey_user_1), (ClientId {client = "1"}, Just testObject_Prekey_user_20)]
      ),
      ( (Id (fromJust (UUID.fromString "00000003-0000-0005-0000-000800000001"))),
        fromList [(ClientId {client = "0"}, Just testObject_Prekey_user_6), (ClientId {client = "1"}, Just testObject_Prekey_user_4)]
      ),
      ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000300000008"))), fromList []),
      ( (Id (fromJust (UUID.fromString "00000007-0000-0008-0000-000100000006"))),
        fromList [(ClientId {client = "0"}, Just testObject_Prekey_user_13), (ClientId {client = "3"}, Just testObject_Prekey_user_9)]
      )
    ]

testObject_UserClientPrekeyMap_5 :: UserClientPrekeyMap
testObject_UserClientPrekeyMap_5 =
  (mkUserClientPrekeyMap . fromList)
    [ ( (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000400000005"))),
        fromList [(ClientId {client = "17"}, Nothing)]
      ),
      ( (Id (fromJust (UUID.fromString "00000003-0000-0006-0000-000100000000"))),
        fromList [(ClientId {client = "2"}, Just testObject_Prekey_user_5)]
      ),
      ((Id (fromJust (UUID.fromString "00000006-0000-0008-0000-000600000003"))), fromList []),
      ( (Id (fromJust (UUID.fromString "00000008-0000-0008-0000-000300000008"))),
        fromList [(ClientId {client = "0"}, Just testObject_Prekey_user_19), (ClientId {client = "1"}, Just testObject_Prekey_user_18)]
      )
    ]

testObject_UserClientPrekeyMap_6 :: UserClientPrekeyMap
testObject_UserClientPrekeyMap_6 =
  (mkUserClientPrekeyMap . fromList)
    [ ( (Id (fromJust (UUID.fromString "0000004d-0000-001f-0000-006300000073"))),
        fromList [(ClientId {client = "1"}, Just testObject_Prekey_user_7), (ClientId {client = "2"}, Just testObject_Prekey_user_8), (ClientId {client = "4"}, Just testObject_Prekey_user_2)]
      ),
      ( (Id (fromJust (UUID.fromString "0000007c-0000-0075-0000-006000000025"))),
        fromList [(ClientId {client = "8"}, Just testObject_Prekey_user_19), (ClientId {client = "c"}, Nothing), (ClientId {client = "f"}, Just testObject_Prekey_user_3)]
      )
    ]

testObject_UserClientPrekeyMap_7 :: UserClientPrekeyMap
testObject_UserClientPrekeyMap_7 =
  (mkUserClientPrekeyMap . fromList)
    [ ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))),
        fromList [(ClientId {client = "0"}, Just testObject_Prekey_user_19)]
      ),
      ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), fromList []),
      ( (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))),
        fromList [(ClientId {client = "0"}, Nothing)]
      ),
      ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
        fromList [(ClientId {client = "0"}, Just testObject_Prekey_user_19), (ClientId {client = "1"}, Just testObject_Prekey_user_19)]
      ),
      ( (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))),
        fromList [(ClientId {client = "0"}, Just testObject_Prekey_user_2), (ClientId {client = "1"}, Just testObject_Prekey_user_19)]
      ),
      ( (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))),
        fromList [(ClientId {client = "2"}, Just testObject_Prekey_user_18)]
      ),
      ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))), fromList [])
    ]

testObject_UserClientPrekeyMap_8 :: UserClientPrekeyMap
testObject_UserClientPrekeyMap_8 = mkUserClientPrekeyMap mempty

testObject_UserClientPrekeyMap_9 :: UserClientPrekeyMap
testObject_UserClientPrekeyMap_9 =
  (mkUserClientPrekeyMap . fromList)
    [ ((Id (fromJust (UUID.fromString "00000054-0000-003b-0000-00210000005f"))), fromList []),
      ( (Id (fromJust (UUID.fromString "00000065-0000-0040-0000-005f00000064"))),
        fromList [(ClientId {client = "0"}, Just testObject_Prekey_user_13), (ClientId {client = "1"}, Just testObject_Prekey_user_2), (ClientId {client = "2"}, Nothing)]
      )
    ]
