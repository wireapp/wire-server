{-# LANGUAGE OverloadedLists #-}

module Test.Wire.API.Golden.Generated.AssetSettings_user where

import Control.Lens ((.~))
import Imports (Bool (False, True), Maybe (Just, Nothing), (&))
import Wire.API.Asset
  ( AssetRetention
      ( AssetEternal,
        AssetEternalInfrequentAccess,
        AssetExpiring,
        AssetPersistent,
        AssetVolatile
      ),
    AssetSettings,
    defAssetSettings,
    setAssetPublic,
    setAssetRetention,
  )

testObject_AssetSettings_user_1 :: AssetSettings
testObject_AssetSettings_user_1 = (defAssetSettings & setAssetPublic .~ (True) & setAssetRetention .~ (Just AssetExpiring))

testObject_AssetSettings_user_2 :: AssetSettings
testObject_AssetSettings_user_2 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Just AssetExpiring))

testObject_AssetSettings_user_3 :: AssetSettings
testObject_AssetSettings_user_3 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Nothing))

testObject_AssetSettings_user_4 :: AssetSettings
testObject_AssetSettings_user_4 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Nothing))

testObject_AssetSettings_user_5 :: AssetSettings
testObject_AssetSettings_user_5 = (defAssetSettings & setAssetPublic .~ (True) & setAssetRetention .~ (Nothing))

testObject_AssetSettings_user_6 :: AssetSettings
testObject_AssetSettings_user_6 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Just AssetEternalInfrequentAccess))

testObject_AssetSettings_user_7 :: AssetSettings
testObject_AssetSettings_user_7 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Just AssetPersistent))

testObject_AssetSettings_user_8 :: AssetSettings
testObject_AssetSettings_user_8 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Just AssetPersistent))

testObject_AssetSettings_user_9 :: AssetSettings
testObject_AssetSettings_user_9 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Nothing))

testObject_AssetSettings_user_10 :: AssetSettings
testObject_AssetSettings_user_10 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Just AssetPersistent))

testObject_AssetSettings_user_11 :: AssetSettings
testObject_AssetSettings_user_11 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Just AssetEternalInfrequentAccess))

testObject_AssetSettings_user_12 :: AssetSettings
testObject_AssetSettings_user_12 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Just AssetPersistent))

testObject_AssetSettings_user_13 :: AssetSettings
testObject_AssetSettings_user_13 = (defAssetSettings & setAssetPublic .~ (True) & setAssetRetention .~ (Nothing))

testObject_AssetSettings_user_14 :: AssetSettings
testObject_AssetSettings_user_14 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Just AssetEternal))

testObject_AssetSettings_user_15 :: AssetSettings
testObject_AssetSettings_user_15 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Just AssetVolatile))

testObject_AssetSettings_user_16 :: AssetSettings
testObject_AssetSettings_user_16 = (defAssetSettings & setAssetPublic .~ (True) & setAssetRetention .~ (Just AssetPersistent))

testObject_AssetSettings_user_17 :: AssetSettings
testObject_AssetSettings_user_17 = (defAssetSettings & setAssetPublic .~ (True) & setAssetRetention .~ (Just AssetExpiring))

testObject_AssetSettings_user_18 :: AssetSettings
testObject_AssetSettings_user_18 = (defAssetSettings & setAssetPublic .~ (False) & setAssetRetention .~ (Just AssetEternalInfrequentAccess))

testObject_AssetSettings_user_19 :: AssetSettings
testObject_AssetSettings_user_19 = (defAssetSettings & setAssetPublic .~ (True) & setAssetRetention .~ (Just AssetEternalInfrequentAccess))

testObject_AssetSettings_user_20 :: AssetSettings
testObject_AssetSettings_user_20 = (defAssetSettings & setAssetPublic .~ (True) & setAssetRetention .~ (Just AssetEternal))
