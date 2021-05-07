{-# LANGUAGE OverloadedLists #-}

module Test.Wire.API.Golden.Generated.AssetKey_user where

import Data.Id (Id (Id))
import qualified Data.UUID as UUID (fromString)
import Imports (fromJust)
import Wire.API.Asset
  ( AssetKey (..),
    AssetRetention
      ( AssetEternal,
        AssetEternalInfrequentAccess,
        AssetExpiring,
        AssetPersistent,
        AssetVolatile
      ),
  )

testObject_AssetKey_user_1 :: AssetKey
testObject_AssetKey_user_1 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000006-0000-0079-0000-003b00000074"))) AssetEternalInfrequentAccess

testObject_AssetKey_user_2 :: AssetKey
testObject_AssetKey_user_2 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000003-0000-0022-0000-00450000003a"))) AssetVolatile

testObject_AssetKey_user_3 :: AssetKey
testObject_AssetKey_user_3 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000024-0000-004b-0000-004b00000058"))) AssetExpiring

testObject_AssetKey_user_4 :: AssetKey
testObject_AssetKey_user_4 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000068-0000-0026-0000-006e00000047"))) AssetEternal

testObject_AssetKey_user_5 :: AssetKey
testObject_AssetKey_user_5 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000066-0000-0009-0000-00410000003a"))) AssetPersistent

testObject_AssetKey_user_6 :: AssetKey
testObject_AssetKey_user_6 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000006-0000-0057-0000-005b00000042"))) AssetVolatile

testObject_AssetKey_user_7 :: AssetKey
testObject_AssetKey_user_7 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000002e-0000-0075-0000-00500000003e"))) AssetVolatile

testObject_AssetKey_user_8 :: AssetKey
testObject_AssetKey_user_8 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000048-0000-006d-0000-00420000002c"))) AssetExpiring

testObject_AssetKey_user_9 :: AssetKey
testObject_AssetKey_user_9 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000006d-0000-006f-0000-000d0000003f"))) AssetEternalInfrequentAccess

testObject_AssetKey_user_10 :: AssetKey
testObject_AssetKey_user_10 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000011-0000-0001-0000-00210000000a"))) AssetEternalInfrequentAccess

testObject_AssetKey_user_11 :: AssetKey
testObject_AssetKey_user_11 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000076-0000-0017-0000-00740000004c"))) AssetPersistent

testObject_AssetKey_user_12 :: AssetKey
testObject_AssetKey_user_12 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000069-0000-0025-0000-000300000003"))) AssetEternalInfrequentAccess

testObject_AssetKey_user_13 :: AssetKey
testObject_AssetKey_user_13 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000004e-0000-005a-0000-007e00000054"))) AssetPersistent

testObject_AssetKey_user_14 :: AssetKey
testObject_AssetKey_user_14 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000001e-0000-001e-0000-000200000012"))) AssetExpiring

testObject_AssetKey_user_15 :: AssetKey
testObject_AssetKey_user_15 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000007e-0000-005a-0000-00270000005b"))) AssetVolatile

testObject_AssetKey_user_16 :: AssetKey
testObject_AssetKey_user_16 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000003a-0000-0063-0000-00640000004b"))) AssetEternal

testObject_AssetKey_user_17 :: AssetKey
testObject_AssetKey_user_17 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000056-0000-0020-0000-001b0000002e"))) AssetEternal

testObject_AssetKey_user_18 :: AssetKey
testObject_AssetKey_user_18 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000060-0000-001b-0000-000800000022"))) AssetEternal

testObject_AssetKey_user_19 :: AssetKey
testObject_AssetKey_user_19 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000000c-0000-0040-0000-004000000058"))) AssetEternalInfrequentAccess

testObject_AssetKey_user_20 :: AssetKey
testObject_AssetKey_user_20 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000036-0000-0045-0000-00610000002a"))) AssetExpiring
