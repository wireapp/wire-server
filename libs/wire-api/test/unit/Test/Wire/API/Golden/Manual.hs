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

module Test.Wire.API.Golden.Manual where

import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Test.Wire.API.Golden.Manual.QualifiedUserClientPrekeyMap
import Test.Wire.API.Golden.Manual.UserClientPrekeyMap
import Test.Wire.API.Golden.Runner

tests :: TestTree
tests =
  testGroup
    "Manual golden tests"
    [ testCase ("UserClientPrekeyMap") $
        testObjects
          [ (testObject_UserClientPrekeyMap_1, "testObject_UserClientPrekeyMap_1.json"),
            (testObject_UserClientPrekeyMap_2, "testObject_UserClientPrekeyMap_2.json"),
            (testObject_UserClientPrekeyMap_3, "testObject_UserClientPrekeyMap_3.json"),
            (testObject_UserClientPrekeyMap_4, "testObject_UserClientPrekeyMap_4.json"),
            (testObject_UserClientPrekeyMap_5, "testObject_UserClientPrekeyMap_5.json"),
            (testObject_UserClientPrekeyMap_6, "testObject_UserClientPrekeyMap_6.json"),
            (testObject_UserClientPrekeyMap_7, "testObject_UserClientPrekeyMap_7.json"),
            (testObject_UserClientPrekeyMap_8, "testObject_UserClientPrekeyMap_8.json"),
            (testObject_UserClientPrekeyMap_9, "testObject_UserClientPrekeyMap_9.json"),
            (testObject_UserClientPrekeyMap_10, "testObject_UserClientPrekeyMap_10.json"),
            (testObject_UserClientPrekeyMap_11, "testObject_UserClientPrekeyMap_11.json"),
            (testObject_UserClientPrekeyMap_12, "testObject_UserClientPrekeyMap_12.json"),
            (testObject_UserClientPrekeyMap_13, "testObject_UserClientPrekeyMap_13.json"),
            (testObject_UserClientPrekeyMap_14, "testObject_UserClientPrekeyMap_14.json"),
            (testObject_UserClientPrekeyMap_15, "testObject_UserClientPrekeyMap_15.json"),
            (testObject_UserClientPrekeyMap_16, "testObject_UserClientPrekeyMap_16.json"),
            (testObject_UserClientPrekeyMap_17, "testObject_UserClientPrekeyMap_17.json"),
            (testObject_UserClientPrekeyMap_18, "testObject_UserClientPrekeyMap_18.json"),
            (testObject_UserClientPrekeyMap_19, "testObject_UserClientPrekeyMap_19.json"),
            (testObject_UserClientPrekeyMap_20, "testObject_UserClientPrekeyMap_20.json")
          ],
      testCase ("QualifiedUserClientPrekeyMap") $
        testObjects
          [ (testObject_QualifiedUserClientPrekeyMap_1, "testObject_QualifiedUserClientPrekeyMap_1.json"),
            (testObject_QualifiedUserClientPrekeyMap_2, "testObject_QualifiedUserClientPrekeyMap_2.json")
          ]
    ]
