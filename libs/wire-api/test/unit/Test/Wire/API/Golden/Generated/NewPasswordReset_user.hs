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

module Test.Wire.API.Golden.Generated.NewPasswordReset_user where

import Imports (Either (Left, Right))
import Wire.API.User
  ( Email (Email, emailDomain, emailLocal),
    Phone (Phone, fromPhone),
  )
import Wire.API.User.Password (NewPasswordReset (..))

testObject_NewPasswordReset_user_1 :: NewPasswordReset
testObject_NewPasswordReset_user_1 = NewPasswordReset (Left (Email {emailLocal = "\1007057b\1098950\&9#\34943\DLEX2o\6661\171973\60563t", emailDomain = "\1080376\60900\DC1\41907s\f\98453}\CAN\SO\n8\SUBz\169687\n\154344Zdb#\SUB4IM8\67225+"}))

testObject_NewPasswordReset_user_2 :: NewPasswordReset
testObject_NewPasswordReset_user_2 = NewPasswordReset (Right (Phone {fromPhone = "+529329682"}))

testObject_NewPasswordReset_user_3 :: NewPasswordReset
testObject_NewPasswordReset_user_3 = NewPasswordReset (Right (Phone {fromPhone = "+41719978"}))

testObject_NewPasswordReset_user_4 :: NewPasswordReset
testObject_NewPasswordReset_user_4 = NewPasswordReset (Right (Phone {fromPhone = "+607957193"}))

testObject_NewPasswordReset_user_5 :: NewPasswordReset
testObject_NewPasswordReset_user_5 = NewPasswordReset (Right (Phone {fromPhone = "+83279556464710"}))

testObject_NewPasswordReset_user_6 :: NewPasswordReset
testObject_NewPasswordReset_user_6 = NewPasswordReset (Left (Email {emailLocal = "\152884", emailDomain = "pkTt\1001860,K\1102090C\53037\&2\1035134\1067347s\n\r\1067827\1098299+\41929\DEL:\GS[\194887MbEC\NUL"}))

testObject_NewPasswordReset_user_7 :: NewPasswordReset
testObject_NewPasswordReset_user_7 = NewPasswordReset (Left (Email {emailLocal = "N\189885V'}\985226\a3", emailDomain = "*\SYNjF\18337\"~Z\58036\41350z\138497bN\131493\8948)I3\t\EOT\1042981\1077394,\DC4"}))

testObject_NewPasswordReset_user_8 :: NewPasswordReset
testObject_NewPasswordReset_user_8 = NewPasswordReset (Left (Email {emailLocal = "(a\34126'CKj\ESC\EM\1051534", emailDomain = "?\986742D\135082\1012625\&7\1076206eh\18902gS\1090140}\1073865n_"}))

testObject_NewPasswordReset_user_9 :: NewPasswordReset
testObject_NewPasswordReset_user_9 = NewPasswordReset (Left (Email {emailLocal = "\ETXji\b\a\995206\1001044\120664'\8103k\RS+", emailDomain = "\FS:\ETX\f\1071180\&5\22603t\135200>\174985IE\1065671M\DC2g\SUBAO\159061\&3\"\1000816H\54341c\129145\44991\&6"}))

testObject_NewPasswordReset_user_10 :: NewPasswordReset
testObject_NewPasswordReset_user_10 = NewPasswordReset (Left (Email {emailLocal = "P\1065495m#\bo\n?n\170449\RSnr\"^c\1033506\\'g\53693l", emailDomain = "/?\17268\1093472\SUBt\ETXv"}))

testObject_NewPasswordReset_user_11 :: NewPasswordReset
testObject_NewPasswordReset_user_11 = NewPasswordReset (Right (Phone {fromPhone = "+009509628647"}))

testObject_NewPasswordReset_user_12 :: NewPasswordReset
testObject_NewPasswordReset_user_12 = NewPasswordReset (Left (Email {emailLocal = "9G\144799", emailDomain = "\986254\SYN\1003426\182313\SI\STX\US\NAKgP \987001"}))

testObject_NewPasswordReset_user_13 :: NewPasswordReset
testObject_NewPasswordReset_user_13 = NewPasswordReset (Right (Phone {fromPhone = "+33232954574312"}))

testObject_NewPasswordReset_user_14 :: NewPasswordReset
testObject_NewPasswordReset_user_14 = NewPasswordReset (Right (Phone {fromPhone = "+314850099"}))

testObject_NewPasswordReset_user_15 :: NewPasswordReset
testObject_NewPasswordReset_user_15 = NewPasswordReset (Left (Email {emailLocal = "\139234\21486\ETX 9\ESC0!\ETX\1007793\ETXxBxL=DL\25894/\r\7651", emailDomain = "$56f!/"}))

testObject_NewPasswordReset_user_16 :: NewPasswordReset
testObject_NewPasswordReset_user_16 = NewPasswordReset (Left (Email {emailLocal = "w\SOHspQ(\25060\EOT\"\\\ETXrbE\n5\111158D", emailDomain = "ps!\t\178810"}))

testObject_NewPasswordReset_user_17 :: NewPasswordReset
testObject_NewPasswordReset_user_17 = NewPasswordReset (Right (Phone {fromPhone = "+560530602858"}))

testObject_NewPasswordReset_user_18 :: NewPasswordReset
testObject_NewPasswordReset_user_18 = NewPasswordReset (Right (Phone {fromPhone = "+2603603795"}))

testObject_NewPasswordReset_user_19 :: NewPasswordReset
testObject_NewPasswordReset_user_19 = NewPasswordReset (Right (Phone {fromPhone = "+002938255629"}))

testObject_NewPasswordReset_user_20 :: NewPasswordReset
testObject_NewPasswordReset_user_20 = NewPasswordReset (Right (Phone {fromPhone = "+77098859488192"}))
