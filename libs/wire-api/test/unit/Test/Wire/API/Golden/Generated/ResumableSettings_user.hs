{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ResumableSettings_user where
import Codec.MIME.Type ( Type(..) )
import qualified Codec.MIME.Type as MIME ( MIMEType(Image) )
import Imports ( Bool(False, True) )
import Wire.API.Asset
    ( mkResumableSettings,
      AssetRetention(AssetEternal, AssetVolatile, AssetExpiring,
                     AssetEternalInfrequentAccess, AssetPersistent),
      ResumableSettings )

testObject_ResumableSettings_user_1 :: ResumableSettings
testObject_ResumableSettings_user_1 = (mkResumableSettings (AssetVolatile) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))
testObject_ResumableSettings_user_2 :: ResumableSettings
testObject_ResumableSettings_user_2 = (mkResumableSettings (AssetEternal) (True) (Type {mimeType = MIME.Image "png", mimeParams = []}))
testObject_ResumableSettings_user_3 :: ResumableSettings
testObject_ResumableSettings_user_3 = (mkResumableSettings (AssetEternalInfrequentAccess) (True) (Type {mimeType = MIME.Image "png", mimeParams = []}))
testObject_ResumableSettings_user_4 :: ResumableSettings
testObject_ResumableSettings_user_4 = (mkResumableSettings (AssetPersistent) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))
testObject_ResumableSettings_user_5 :: ResumableSettings
testObject_ResumableSettings_user_5 = (mkResumableSettings (AssetPersistent) (True) (Type {mimeType = MIME.Image "png", mimeParams = []}))
testObject_ResumableSettings_user_6 :: ResumableSettings
testObject_ResumableSettings_user_6 = (mkResumableSettings (AssetPersistent) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))
testObject_ResumableSettings_user_7 :: ResumableSettings
testObject_ResumableSettings_user_7 = (mkResumableSettings (AssetPersistent) (True) (Type {mimeType = MIME.Image "png", mimeParams = []}))
testObject_ResumableSettings_user_8 :: ResumableSettings
testObject_ResumableSettings_user_8 = (mkResumableSettings (AssetVolatile) (True) (Type {mimeType = MIME.Image "png", mimeParams = []}))
testObject_ResumableSettings_user_9 :: ResumableSettings
testObject_ResumableSettings_user_9 = (mkResumableSettings (AssetVolatile) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))
testObject_ResumableSettings_user_10 :: ResumableSettings
testObject_ResumableSettings_user_10 = (mkResumableSettings (AssetEternalInfrequentAccess) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))
testObject_ResumableSettings_user_11 :: ResumableSettings
testObject_ResumableSettings_user_11 = (mkResumableSettings (AssetPersistent) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))
testObject_ResumableSettings_user_12 :: ResumableSettings
testObject_ResumableSettings_user_12 = (mkResumableSettings (AssetExpiring) (True) (Type {mimeType = MIME.Image "png", mimeParams = []}))
testObject_ResumableSettings_user_13 :: ResumableSettings
testObject_ResumableSettings_user_13 = (mkResumableSettings (AssetVolatile) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))
testObject_ResumableSettings_user_14 :: ResumableSettings
testObject_ResumableSettings_user_14 = (mkResumableSettings (AssetEternalInfrequentAccess) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))
testObject_ResumableSettings_user_15 :: ResumableSettings
testObject_ResumableSettings_user_15 = (mkResumableSettings (AssetEternalInfrequentAccess) (True) (Type {mimeType = MIME.Image "png", mimeParams = []}))
testObject_ResumableSettings_user_16 :: ResumableSettings
testObject_ResumableSettings_user_16 = (mkResumableSettings (AssetEternal) (True) (Type {mimeType = MIME.Image "png", mimeParams = []}))
testObject_ResumableSettings_user_17 :: ResumableSettings
testObject_ResumableSettings_user_17 = (mkResumableSettings (AssetExpiring) (True) (Type {mimeType = MIME.Image "png", mimeParams = []}))
testObject_ResumableSettings_user_18 :: ResumableSettings
testObject_ResumableSettings_user_18 = (mkResumableSettings (AssetEternalInfrequentAccess) (True) (Type {mimeType = MIME.Image "png", mimeParams = []}))
testObject_ResumableSettings_user_19 :: ResumableSettings
testObject_ResumableSettings_user_19 = (mkResumableSettings (AssetPersistent) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))
testObject_ResumableSettings_user_20 :: ResumableSettings
testObject_ResumableSettings_user_20 = (mkResumableSettings (AssetEternal) (False) (Type {mimeType = MIME.Image "png", mimeParams = []}))
