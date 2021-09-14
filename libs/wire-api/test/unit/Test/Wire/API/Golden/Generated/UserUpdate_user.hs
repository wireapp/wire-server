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
module Test.Wire.API.Golden.Generated.UserUpdate_user where

import Imports (Maybe (Just, Nothing))
import Wire.API.User
  ( Asset (ImageAsset),
    AssetSize (AssetComplete, AssetPreview),
    ColourId (ColourId, fromColourId),
    Name (Name, fromName),
    Pict (Pict, fromPict),
    UserUpdate (..),
  )

testObject_UserUpdate_user_1 :: UserUpdate
testObject_UserUpdate_user_1 =
  UserUpdate
    { uupName = Nothing,
      uupPict = Nothing,
      uupAssets = Nothing,
      uupAccentId = Nothing
    }

testObject_UserUpdate_user_2 :: UserUpdate
testObject_UserUpdate_user_2 =
  UserUpdate
    { uupName = Just (Name {fromName = "~\RSK\1033973w\EMd\156648\59199g"}),
      uupPict = Just (Pict {fromPict = []}),
      uupAssets = Just [ImageAsset "" (Just AssetComplete)],
      uupAccentId = Just (ColourId {fromColourId = 3})
    }
