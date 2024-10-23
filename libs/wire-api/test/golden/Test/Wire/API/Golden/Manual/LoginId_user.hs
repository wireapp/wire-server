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

module Test.Wire.API.Golden.Manual.LoginId_user where

import Data.Handle (parseHandle)
import Data.Maybe
import Wire.API.User
import Wire.API.User.Auth (LoginId (..))

testObject_LoginId_user_1 :: LoginId
testObject_LoginId_user_1 =
  LoginByEmail
    (unsafeEmailAddress "some" "example")

testObject_LoginId_user_2 :: LoginId
testObject_LoginId_user_2 =
  LoginByEmail
    ( unsafeEmailAddress "some" "example"
    )

testObject_LoginId_user_3 :: LoginId
testObject_LoginId_user_3 = LoginByHandle (fromJust (parseHandle "7a8gg3v98"))

testObject_LoginId_user_4 :: LoginId
testObject_LoginId_user_4 = LoginByHandle (fromJust (parseHandle "lb"))

testObject_LoginId_user_5 :: LoginId
testObject_LoginId_user_5 =
  LoginByHandle (fromJust (parseHandle "z58-6fbjhtx11d8t6oplyijpkc2.fp_lf3kpk3_.qle4iecjun2xd0tpcordlg2bwv636v3cthpgwah3undqmuofgzp8ry6gc6g-n-kxnj7sl6771hxou7-t_ps_lu_t3.4ukz6dh6fkjq2i3aggtkbpzbd1162.qv.rbtb6e.90-xpayg65z9t9lk2aur452zcs9a"))

testObject_LoginId_user_6 :: LoginId
testObject_LoginId_user_6 =
  LoginByEmail (unsafeEmailAddress "some" "example")
