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

module Test.Wire.API.Golden.Generated.Activate_user where

import Data.Text.Ascii (AsciiChars (validate))
import Imports (Bool (False, True), fromRight, undefined)
import Wire.API.User
  ( Email (Email, emailDomain, emailLocal),
    Phone (Phone, fromPhone),
  )
import Wire.API.User.Activation
  ( Activate (..),
    ActivationCode (ActivationCode, fromActivationCode),
    ActivationKey (ActivationKey, fromActivationKey),
    ActivationTarget (ActivateEmail, ActivateKey, ActivatePhone),
  )

testObject_Activate_user_1 :: Activate
testObject_Activate_user_1 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+45520903"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("HUUpJQ==")))}, activateDryrun = True}

testObject_Activate_user_2 :: Activate
testObject_Activate_user_2 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("e3sm9EjNmzA=")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("fg==")))}, activateDryrun = False}

testObject_Activate_user_3 :: Activate
testObject_Activate_user_3 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+44508058"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("OAbwDkw=")))}, activateDryrun = True}

testObject_Activate_user_4 :: Activate
testObject_Activate_user_4 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+97751884"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("811p-743Gvpi")))}, activateDryrun = False}

testObject_Activate_user_5 :: Activate
testObject_Activate_user_5 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "\1002810\NUL\1075125", emailDomain = "k\\\SOHa\SYN*\176499"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("")))}, activateDryrun = False}

testObject_Activate_user_6 :: Activate
testObject_Activate_user_6 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "\1104323i>\1007870Ha!", emailDomain = ""}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("FXrNll0Kqg==")))}, activateDryrun = False}

testObject_Activate_user_7 :: Activate
testObject_Activate_user_7 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("jQ==")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("8yl3qERc")))}, activateDryrun = False}

testObject_Activate_user_8 :: Activate
testObject_Activate_user_8 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+3276478697350"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("NF20Avw=")))}, activateDryrun = True}

testObject_Activate_user_9 :: Activate
testObject_Activate_user_9 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("DkV9xQ==")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("61wG")))}, activateDryrun = True}

testObject_Activate_user_10 :: Activate
testObject_Activate_user_10 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("1szizA==")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("kcvCq2A=")))}, activateDryrun = False}

testObject_Activate_user_11 :: Activate
testObject_Activate_user_11 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "\ETX4\SUB", emailDomain = ""}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("MZpmmg==")))}, activateDryrun = False}

testObject_Activate_user_12 :: Activate
testObject_Activate_user_12 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("V3mr5D4=")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("sScBopoNTb0=")))}, activateDryrun = True}

testObject_Activate_user_13 :: Activate
testObject_Activate_user_13 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("haH9_sUNFw==")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("ysvb")))}, activateDryrun = False}

testObject_Activate_user_14 :: Activate
testObject_Activate_user_14 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+13340815619"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("hQ==")))}, activateDryrun = True}

testObject_Activate_user_15 :: Activate
testObject_Activate_user_15 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "\22308W[\1041599G\996204]{\n", emailDomain = " V8\992253\NAK*"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("biTZ")))}, activateDryrun = False}

testObject_Activate_user_16 :: Activate
testObject_Activate_user_16 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+77635104433"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("5W4=")))}, activateDryrun = True}

testObject_Activate_user_17 :: Activate
testObject_Activate_user_17 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+556856857856"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("ShjEcgx6P0Hs")))}, activateDryrun = False}

testObject_Activate_user_18 :: Activate
testObject_Activate_user_18 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "2\1107376B\1099134\ETX2\US\1080331", emailDomain = "v\SOH\SO\1007855/e"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("xRvktQ==")))}, activateDryrun = False}

testObject_Activate_user_19 :: Activate
testObject_Activate_user_19 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("1fCrdg==")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("")))}, activateDryrun = False}

testObject_Activate_user_20 :: Activate
testObject_Activate_user_20 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+893051142276"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("7PtclAevMzA=")))}, activateDryrun = False}
