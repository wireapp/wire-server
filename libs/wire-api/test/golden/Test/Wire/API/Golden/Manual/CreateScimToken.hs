-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Manual.CreateScimToken where

import Data.Code
import Data.Misc (plainTextPassword6Unsafe)
import Data.Range (unsafeRange)
import Data.Text.Ascii (AsciiChars (validate))
import Imports (Maybe (Just, Nothing), fromRight, undefined)
import Wire.API.User.Scim (CreateScimToken (..))

testObject_CreateScimToken_1 :: CreateScimToken
testObject_CreateScimToken_1 =
  CreateScimToken
    "description"
    (Just (plainTextPassword6Unsafe "very-geheim"))
    (Just (Value {asciiValue = unsafeRange (fromRight undefined (validate "123456"))}))

testObject_CreateScimToken_2 :: CreateScimToken
testObject_CreateScimToken_2 =
  CreateScimToken
    "description2"
    (Just (plainTextPassword6Unsafe "secret"))
    Nothing

testObject_CreateScimToken_3 :: CreateScimToken
testObject_CreateScimToken_3 =
  CreateScimToken
    "description3"
    Nothing
    (Just (Value {asciiValue = unsafeRange (fromRight undefined (validate "654321"))}))

testObject_CreateScimToken_4 :: CreateScimToken
testObject_CreateScimToken_4 =
  CreateScimToken
    "description4"
    Nothing
    Nothing
