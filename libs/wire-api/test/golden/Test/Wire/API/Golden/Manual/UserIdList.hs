module Test.Wire.API.Golden.Manual.UserIdList where

import Data.Id (Id (Id))
import qualified Data.UUID as UUID
import Imports
import Wire.API.User (UserIdList (..))

testObject_UserIdList_1 :: UserIdList
testObject_UserIdList_1 =
  UserIdList
    [ Id (fromJust (UUID.fromString "0000304a-0000-0d5e-0000-3fac00003993")),
      Id (fromJust (UUID.fromString "00003c90-0000-2207-0000-5249000018b1")),
      Id (fromJust (UUID.fromString "000016ee-0000-1c33-0000-6684000050e6")),
      Id (fromJust (UUID.fromString "0000366d-0000-7f19-0000-4153000039a6")),
      Id (fromJust (UUID.fromString "00002f85-0000-30dc-0000-4cb700001c44")),
      Id (fromJust (UUID.fromString "000056c8-0000-0828-0000-0a31000012b6")),
      Id (fromJust (UUID.fromString "00001d2d-0000-74ae-0000-44fc00000eba")),
      Id (fromJust (UUID.fromString "00001b2c-0000-651e-0000-12d9000068dd")),
      Id (fromJust (UUID.fromString "00006a07-0000-7703-0000-6c1000002889")),
      Id (fromJust (UUID.fromString "00001e50-0000-2dd8-0000-0c7a000053f0")),
      Id (fromJust (UUID.fromString "00003842-0000-2193-0000-275c00004421"))
    ]

testObject_UserIdList_2 :: UserIdList
testObject_UserIdList_2 =
  UserIdList []
