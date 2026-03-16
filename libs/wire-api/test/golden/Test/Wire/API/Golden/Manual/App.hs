-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Manual.App where

import Data.Misc
import Data.Range
import Imports
import Wire.API.User

testObject_NewApp_1 :: NewApp
testObject_NewApp_1 =
  NewApp
    (either undefined id $ mkName "good name")
    mempty
    defaultAccentId
    Other
    (unsafeRange "good description")
    (plainTextPassword6Unsafe "good password")

testObject_GetApp_1 :: GetApp
testObject_GetApp_1 =
  GetApp
    (either undefined id $ mkName "good name")
    mempty
    defaultAccentId
    Other
    (unsafeRange "good description")

testObject_PutApp_1 :: PutApp
testObject_PutApp_1 =
  PutApp
    (Just (either undefined id $ mkName "good name"))
    (Just mempty)
    (Just defaultAccentId)
    (Just Other)
    (Just (unsafeRange "good description"))

testObject_PutApp_2 :: PutApp
testObject_PutApp_2 =
  PutApp
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
