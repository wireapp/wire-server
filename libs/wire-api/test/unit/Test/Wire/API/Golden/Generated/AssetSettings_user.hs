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
testObject_AssetSettings_user_1 =
  defAssetSettings & setAssetPublic .~ True & setAssetRetention .~ Just AssetExpiring

testObject_AssetSettings_user_2 :: AssetSettings
testObject_AssetSettings_user_2 =
  defAssetSettings & setAssetPublic .~ False & setAssetRetention .~ Just AssetExpiring

testObject_AssetSettings_user_3 :: AssetSettings
testObject_AssetSettings_user_3 = defAssetSettings & setAssetPublic .~ False & setAssetRetention .~ Nothing

testObject_AssetSettings_user_5 :: AssetSettings
testObject_AssetSettings_user_5 = defAssetSettings & setAssetPublic .~ True & setAssetRetention .~ Nothing

testObject_AssetSettings_user_6 :: AssetSettings
testObject_AssetSettings_user_6 =
  defAssetSettings & setAssetPublic .~ False & setAssetRetention .~ (Just AssetEternalInfrequentAccess)

testObject_AssetSettings_user_10 :: AssetSettings
testObject_AssetSettings_user_10 =
  defAssetSettings & setAssetPublic .~ False & setAssetRetention .~ Just AssetPersistent

testObject_AssetSettings_user_14 :: AssetSettings
testObject_AssetSettings_user_14 =
  defAssetSettings & setAssetPublic .~ False & setAssetRetention .~ (Just AssetEternal)

testObject_AssetSettings_user_15 :: AssetSettings
testObject_AssetSettings_user_15 =
  defAssetSettings & setAssetPublic .~ False & setAssetRetention .~ (Just AssetVolatile)

testObject_AssetSettings_user_16 :: AssetSettings
testObject_AssetSettings_user_16 =
  defAssetSettings & setAssetPublic .~ True & setAssetRetention .~ (Just AssetPersistent)

testObject_AssetSettings_user_19 :: AssetSettings
testObject_AssetSettings_user_19 =
  defAssetSettings & setAssetPublic .~ True & setAssetRetention .~ (Just AssetEternalInfrequentAccess)

testObject_AssetSettings_user_20 :: AssetSettings
testObject_AssetSettings_user_20 =
  defAssetSettings & setAssetPublic .~ True & setAssetRetention .~ (Just AssetEternal)
