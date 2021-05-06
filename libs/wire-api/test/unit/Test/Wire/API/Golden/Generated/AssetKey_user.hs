{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.AssetKey_user where
import Data.Id ( Id(Id) )
import Imports ( fromJust )
import qualified Data.UUID as UUID ( fromString )
import Wire.API.Asset
    ( AssetKey(..),
      AssetRetention(AssetPersistent, AssetEternal,
                     AssetEternalInfrequentAccess, AssetVolatile, AssetExpiring) )

testObject_AssetKey_user_1 :: AssetKey
testObject_AssetKey_user_1 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000048-0000-001c-0000-001800000012"))) AssetEternalInfrequentAccess
testObject_AssetKey_user_2 :: AssetKey
testObject_AssetKey_user_2 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000001b-0000-0067-0000-006900000060"))) AssetVolatile
testObject_AssetKey_user_3 :: AssetKey
testObject_AssetKey_user_3 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000005f-0000-0078-0000-001b00000043"))) AssetPersistent
testObject_AssetKey_user_4 :: AssetKey
testObject_AssetKey_user_4 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000047-0000-006c-0000-00640000006b"))) AssetEternal
testObject_AssetKey_user_5 :: AssetKey
testObject_AssetKey_user_5 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000006c-0000-0015-0000-007e00000006"))) AssetVolatile
testObject_AssetKey_user_6 :: AssetKey
testObject_AssetKey_user_6 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000046-0000-0004-0000-007800000014"))) AssetVolatile
testObject_AssetKey_user_7 :: AssetKey
testObject_AssetKey_user_7 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000026-0000-005a-0000-005e00000051"))) AssetPersistent
testObject_AssetKey_user_8 :: AssetKey
testObject_AssetKey_user_8 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000000e-0000-001b-0000-002500000036"))) AssetEternalInfrequentAccess
testObject_AssetKey_user_9 :: AssetKey
testObject_AssetKey_user_9 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000038-0000-004a-0000-001c00000013"))) AssetEternalInfrequentAccess
testObject_AssetKey_user_10 :: AssetKey
testObject_AssetKey_user_10 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000000d-0000-0015-0000-00070000001e"))) AssetPersistent
testObject_AssetKey_user_11 :: AssetKey
testObject_AssetKey_user_11 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000005-0000-006a-0000-00270000007f"))) AssetEternal
testObject_AssetKey_user_12 :: AssetKey
testObject_AssetKey_user_12 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000010-0000-0072-0000-00500000003c"))) AssetEternalInfrequentAccess
testObject_AssetKey_user_13 :: AssetKey
testObject_AssetKey_user_13 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000005b-0000-001d-0000-000a00000023"))) AssetVolatile
testObject_AssetKey_user_14 :: AssetKey
testObject_AssetKey_user_14 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000000e-0000-0078-0000-006600000076"))) AssetPersistent
testObject_AssetKey_user_15 :: AssetKey
testObject_AssetKey_user_15 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000071-0000-0012-0000-006700000003"))) AssetExpiring
testObject_AssetKey_user_16 :: AssetKey
testObject_AssetKey_user_16 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000000-0000-001d-0000-002000000013"))) AssetVolatile
testObject_AssetKey_user_17 :: AssetKey
testObject_AssetKey_user_17 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000001b-0000-0080-0000-00330000003f"))) AssetExpiring
testObject_AssetKey_user_18 :: AssetKey
testObject_AssetKey_user_18 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000061-0000-0030-0000-00200000001f"))) AssetPersistent
testObject_AssetKey_user_19 :: AssetKey
testObject_AssetKey_user_19 = AssetKeyV3 (Id (fromJust (UUID.fromString "0000007b-0000-003e-0000-007500000003"))) AssetPersistent
testObject_AssetKey_user_20 :: AssetKey
testObject_AssetKey_user_20 = AssetKeyV3 (Id (fromJust (UUID.fromString "00000079-0000-0035-0000-005000000072"))) AssetPersistent
