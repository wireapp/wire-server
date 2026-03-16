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
import Test.Wire.API.Golden.Generated.UserProfile_user
import Web.HttpApiData
import Wire.API.User
import Wire.API.User.Auth (SomeUserToken)

someToken :: SomeUserToken
someToken = either undefined id $ parseUrlPiece "DTHdPvHSFolvyGVvuaexZ9DKptwnxTSn8UhKc-6A9q34s4q0YY3_CgpYxDMr56crHrW79EPwKu2BLwQkFT7wBw==.v=1.k=1.d=1773661988.t=u.l=.u=ac638199-8816-439f-88dd-8e206c9b5baa.r=fa16d9df"

testObject_NewApp_1 :: NewApp
testObject_NewApp_1 =
  NewApp
    (either undefined id $ mkName "good name")
    mempty
    defaultAccentId
    Other
    (unsafeRange "good description")
    (plainTextPassword6Unsafe "good password")

testObject_CreatedApp_1 :: CreatedApp
testObject_CreatedApp_1 =
  CreatedApp testObject_UserProfile_user_2 someToken

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
