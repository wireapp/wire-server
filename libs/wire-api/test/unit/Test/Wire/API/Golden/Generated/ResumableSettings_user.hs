{-# LANGUAGE OverloadedLists #-}

module Test.Wire.API.Golden.Generated.ResumableSettings_user where

import Codec.MIME.Type (Type (..))
import qualified Codec.MIME.Type as MIME (MIMEType (Image))
import Imports (Bool (False, True))
import Wire.API.Asset
  ( AssetRetention
      ( AssetEternal,
        AssetEternalInfrequentAccess,
        AssetExpiring,
        AssetPersistent,
        AssetVolatile
      ),
    ResumableSettings,
    mkResumableSettings,
  )

testObject_ResumableSettings_user_1 :: ResumableSettings
testObject_ResumableSettings_user_1 = (mkResumableSettings (AssetExpiring) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))

testObject_ResumableSettings_user_2 :: ResumableSettings
testObject_ResumableSettings_user_2 = (mkResumableSettings (AssetEternal) (True) (Type {mimeType = MIME.Image "png", mimeParams = []}))

testObject_ResumableSettings_user_3 :: ResumableSettings
testObject_ResumableSettings_user_3 = (mkResumableSettings (AssetVolatile) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))

testObject_ResumableSettings_user_4 :: ResumableSettings
testObject_ResumableSettings_user_4 = (mkResumableSettings (AssetEternalInfrequentAccess) (True) (Type {mimeType = MIME.Image "png", mimeParams = []}))

testObject_ResumableSettings_user_5 :: ResumableSettings
testObject_ResumableSettings_user_5 = (mkResumableSettings (AssetPersistent) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))

testObject_ResumableSettings_user_6 :: ResumableSettings
testObject_ResumableSettings_user_6 = (mkResumableSettings (AssetPersistent) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))

testObject_ResumableSettings_user_7 :: ResumableSettings
testObject_ResumableSettings_user_7 = (mkResumableSettings (AssetEternalInfrequentAccess) (True) (Type {mimeType = MIME.Image "png", mimeParams = []}))

testObject_ResumableSettings_user_8 :: ResumableSettings
testObject_ResumableSettings_user_8 = (mkResumableSettings (AssetPersistent) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))

testObject_ResumableSettings_user_9 :: ResumableSettings
testObject_ResumableSettings_user_9 = (mkResumableSettings (AssetEternal) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))

testObject_ResumableSettings_user_10 :: ResumableSettings
testObject_ResumableSettings_user_10 = (mkResumableSettings (AssetEternal) (True) (Type {mimeType = MIME.Image "png", mimeParams = []}))

testObject_ResumableSettings_user_11 :: ResumableSettings
testObject_ResumableSettings_user_11 = (mkResumableSettings (AssetEternal) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))

testObject_ResumableSettings_user_12 :: ResumableSettings
testObject_ResumableSettings_user_12 = (mkResumableSettings (AssetVolatile) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))

testObject_ResumableSettings_user_13 :: ResumableSettings
testObject_ResumableSettings_user_13 = (mkResumableSettings (AssetEternal) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))

testObject_ResumableSettings_user_14 :: ResumableSettings
testObject_ResumableSettings_user_14 = (mkResumableSettings (AssetEternal) (True) (Type {mimeType = MIME.Image "png", mimeParams = []}))

testObject_ResumableSettings_user_15 :: ResumableSettings
testObject_ResumableSettings_user_15 = (mkResumableSettings (AssetVolatile) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))

testObject_ResumableSettings_user_16 :: ResumableSettings
testObject_ResumableSettings_user_16 = (mkResumableSettings (AssetVolatile) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))

testObject_ResumableSettings_user_17 :: ResumableSettings
testObject_ResumableSettings_user_17 = (mkResumableSettings (AssetVolatile) (True) (Type {mimeType = MIME.Image "png", mimeParams = []}))

testObject_ResumableSettings_user_18 :: ResumableSettings
testObject_ResumableSettings_user_18 = (mkResumableSettings (AssetVolatile) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))

testObject_ResumableSettings_user_19 :: ResumableSettings
testObject_ResumableSettings_user_19 = (mkResumableSettings (AssetExpiring) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))

testObject_ResumableSettings_user_20 :: ResumableSettings
testObject_ResumableSettings_user_20 = (mkResumableSettings (AssetVolatile) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))
