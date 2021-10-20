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
testObject_AssetKey_user_1 =
  AssetKeyV3 (Id (fromJust (UUID.fromString "00000006-0000-0079-0000-003b00000074"))) AssetEternalInfrequentAccess

testObject_AssetKey_user_2 :: AssetKey
testObject_AssetKey_user_2 =
  AssetKeyV3 (Id (fromJust (UUID.fromString "00000003-0000-0022-0000-00450000003a"))) AssetVolatile

testObject_AssetKey_user_3 :: AssetKey
testObject_AssetKey_user_3 =
  AssetKeyV3 (Id (fromJust (UUID.fromString "00000024-0000-004b-0000-004b00000058"))) AssetExpiring

testObject_AssetKey_user_4 :: AssetKey
testObject_AssetKey_user_4 =
  AssetKeyV3 (Id (fromJust (UUID.fromString "00000068-0000-0026-0000-006e00000047"))) AssetEternal

testObject_AssetKey_user_5 :: AssetKey
testObject_AssetKey_user_5 =
  AssetKeyV3 (Id (fromJust (UUID.fromString "00000066-0000-0009-0000-00410000003a"))) AssetPersistent
