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
module Test.Wire.API.Golden.Generated.InvitationCode_user where

import Data.Text.Ascii (AsciiChars (validate))
import Imports (fromRight, undefined)
import Wire.API.User (InvitationCode (..))

testObject_InvitationCode_user_1 :: InvitationCode
testObject_InvitationCode_user_1 =
  InvitationCode {fromInvitationCode = fromRight undefined (validate "RUne0vse27qsm5jxGmL0xQaeuEOqcqr65rU=")}

testObject_InvitationCode_user_2 :: InvitationCode
testObject_InvitationCode_user_2 =
  InvitationCode {fromInvitationCode = fromRight undefined (validate "j-G82ks0MYiz_gOEUvVpWa3V6bpuP5UcUhc7")}

testObject_InvitationCode_user_3 :: InvitationCode
testObject_InvitationCode_user_3 = InvitationCode {fromInvitationCode = fromRight undefined (validate "")}

testObject_InvitationCode_user_4 :: InvitationCode
testObject_InvitationCode_user_4 = InvitationCode {fromInvitationCode = fromRight undefined (validate "0y-7KQ==")}

testObject_InvitationCode_user_5 :: InvitationCode
testObject_InvitationCode_user_5 =
  InvitationCode {fromInvitationCode = fromRight undefined (validate "-Oj_2VAtOI_kSg==")}
