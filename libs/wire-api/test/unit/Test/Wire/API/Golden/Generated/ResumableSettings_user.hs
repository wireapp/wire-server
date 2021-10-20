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
testObject_ResumableSettings_user_1 =
  mkResumableSettings AssetExpiring False (Type {mimeType = MIME.Image "png", mimeParams = []})

testObject_ResumableSettings_user_2 :: ResumableSettings
testObject_ResumableSettings_user_2 =
  mkResumableSettings AssetEternal True (Type {mimeType = MIME.Image "png", mimeParams = []})

testObject_ResumableSettings_user_4 :: ResumableSettings
testObject_ResumableSettings_user_4 =
  mkResumableSettings AssetEternalInfrequentAccess True (Type {mimeType = MIME.Image "png", mimeParams = []})

testObject_ResumableSettings_user_5 :: ResumableSettings
testObject_ResumableSettings_user_5 =
  mkResumableSettings AssetPersistent False (Type {mimeType = MIME.Image "png", mimeParams = []})

testObject_ResumableSettings_user_17 :: ResumableSettings
testObject_ResumableSettings_user_17 =
  mkResumableSettings AssetVolatile True (Type {mimeType = MIME.Image "png", mimeParams = []})
