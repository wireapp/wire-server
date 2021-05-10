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

module Test.Wire.API.Golden.Generated.PropertyValue_user where

import Data.Aeson
  ( Value (Array, Bool, Null, Number, Object, String),
  )
import GHC.Exts (IsList (fromList))
import Imports (Bool (False, True))
import Wire.API.Properties (PropertyValue (..))

testObject_PropertyValue_user_1 :: PropertyValue
testObject_PropertyValue_user_1 = PropertyValue {propertyValueJson = Number (7.0e-14)}

testObject_PropertyValue_user_2 :: PropertyValue
testObject_PropertyValue_user_2 = PropertyValue {propertyValueJson = Object (fromList [("M\ETX\ENQ\af\176612\n\155245|n", String "\178208\bwS"), ("\17080Z%\1025245\DELrgT^", Number (-100.0)), ("fgH", Bool False)])}

testObject_PropertyValue_user_3 :: PropertyValue
testObject_PropertyValue_user_3 = PropertyValue {propertyValueJson = Object (fromList [("", Number (7.0e-5)), ("/\vT", Bool True), ("^}Y{Nt", Bool False)])}

testObject_PropertyValue_user_4 :: PropertyValue
testObject_PropertyValue_user_4 = PropertyValue {propertyValueJson = Array [Null, Bool True, String "", String "7g", Number (0.2), String "t", Bool True, Bool True, Bool True, Null, Bool True, Number (200.0), Bool False, Bool True, Null]}

testObject_PropertyValue_user_5 :: PropertyValue
testObject_PropertyValue_user_5 = PropertyValue {propertyValueJson = Array [Bool False]}

testObject_PropertyValue_user_6 :: PropertyValue
testObject_PropertyValue_user_6 = PropertyValue {propertyValueJson = Array [Number (2.0e-9)]}

testObject_PropertyValue_user_7 :: PropertyValue
testObject_PropertyValue_user_7 = PropertyValue {propertyValueJson = Object (fromList [("", Number (1.0e-5)), ("\STXy", Null), ("r\1035660\"TD", String "\54670"), ("\f]X", Number (2.0e-5)), ("f\DC2\1059734", Number (1.0e-2)), ("\987855", String "\997146\RS\CANu\n")])}

testObject_PropertyValue_user_8 :: PropertyValue
testObject_PropertyValue_user_8 = PropertyValue {propertyValueJson = Null}

testObject_PropertyValue_user_9 :: PropertyValue
testObject_PropertyValue_user_9 = PropertyValue {propertyValueJson = Array [Bool True, String "F", Null, String "I\NAK", String "(", Bool True, String "", Bool True, Bool True, Null, Number (0.0), Number (200.0)]}

testObject_PropertyValue_user_10 :: PropertyValue
testObject_PropertyValue_user_10 = PropertyValue {propertyValueJson = Bool False}

testObject_PropertyValue_user_11 :: PropertyValue
testObject_PropertyValue_user_11 = PropertyValue {propertyValueJson = Object (fromList [("0", Number (-4.0e7)), ("\873", Bool False), ("\1048918\71091\133971Q3\SOH\"", Null), ("\1089049n", Bool False)])}

testObject_PropertyValue_user_12 :: PropertyValue
testObject_PropertyValue_user_12 = PropertyValue {propertyValueJson = Array [Bool True, String "\a\1001598", Bool False, Bool True, String "\159573", String "", Null, Null, Number (2.0), String "\188130\ACK", Bool False]}

testObject_PropertyValue_user_13 :: PropertyValue
testObject_PropertyValue_user_13 = PropertyValue {propertyValueJson = Array [Null, Bool False, String "*\SOHO", Number (0.1), String "\SOp\12293\1075098"]}

testObject_PropertyValue_user_14 :: PropertyValue
testObject_PropertyValue_user_14 = PropertyValue {propertyValueJson = Object (fromList [])}

testObject_PropertyValue_user_15 :: PropertyValue
testObject_PropertyValue_user_15 = PropertyValue {propertyValueJson = Object (fromList [("dN", Null), ("0", Bool True), ("", Bool True), ("K", Bool True), ("\1024195\"", Null), ("\1044199", Bool True), ("D\ACK", String "\r")])}

testObject_PropertyValue_user_16 :: PropertyValue
testObject_PropertyValue_user_16 = PropertyValue {propertyValueJson = Null}

testObject_PropertyValue_user_17 :: PropertyValue
testObject_PropertyValue_user_17 = PropertyValue {propertyValueJson = Array [Number (100.0), Bool False, String "#\\\993660`r\1110635"]}

testObject_PropertyValue_user_18 :: PropertyValue
testObject_PropertyValue_user_18 = PropertyValue {propertyValueJson = Object (fromList [("\1078290\SOHeD\991615", Number (3.0e-5)), (")\ESCt\10112", Number (-4.0e-3)), ("\60512%9", Bool False), ("", Number (0.1)), ("\DC3", Number (400.0)), ("{ *f~", Number (-400.0))])}

testObject_PropertyValue_user_19 :: PropertyValue
testObject_PropertyValue_user_19 = PropertyValue {propertyValueJson = Array [Null, Null, Bool False, Bool False, String "\r\ETX", Bool True, Bool False, Number (2.0e-2), String "e", Number (2.0e-2), String "K"]}

testObject_PropertyValue_user_20 :: PropertyValue
testObject_PropertyValue_user_20 = PropertyValue {propertyValueJson = Number (1.2e-21)}
