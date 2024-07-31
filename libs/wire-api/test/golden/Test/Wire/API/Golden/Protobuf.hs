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

module Test.Wire.API.Golden.Protobuf where

import Imports
import Proto.Otr qualified
import Test.Tasty
import Test.Tasty.HUnit
import Test.Wire.API.Golden.Generated.QualifiedNewOtrMessage_user
import Test.Wire.API.Golden.Runner

tests :: TestTree
tests =
  testGroup
    "Protobuf golden tests"
    [ testCase "QualifiedNewOtrMessage" $
        protoTestObjects @Proto.Otr.QualifiedNewOtrMessage
          [ ( testObject_QualifiedNewOtrMessage_user_1,
              "testObject_QualifiedNewOtrMessage_user_1.protobuf"
            ),
            ( testObject_QualifiedNewOtrMessage_user_2,
              "testObject_QualifiedNewOtrMessage_user_2.protobuf"
            ),
            ( testObject_QualifiedNewOtrMessage_user_3,
              "testObject_QualifiedNewOtrMessage_user_3.protobuf"
            ),
            ( testObject_QualifiedNewOtrMessage_user_4,
              "testObject_QualifiedNewOtrMessage_user_4.protobuf"
            ),
            ( testObject_QualifiedNewOtrMessage_user_5,
              "testObject_QualifiedNewOtrMessage_user_5.protobuf"
            ),
            ( testObject_QualifiedNewOtrMessage_user_6,
              "testObject_QualifiedNewOtrMessage_user_6.protobuf"
            ),
            ( testObject_QualifiedNewOtrMessage_user_7,
              "testObject_QualifiedNewOtrMessage_user_7.protobuf"
            ),
            ( testObject_QualifiedNewOtrMessage_user_8,
              "testObject_QualifiedNewOtrMessage_user_8.protobuf"
            ),
            ( testObject_QualifiedNewOtrMessage_user_9,
              "testObject_QualifiedNewOtrMessage_user_9.protobuf"
            ),
            ( testObject_QualifiedNewOtrMessage_user_10,
              "testObject_QualifiedNewOtrMessage_user_10.protobuf"
            ),
            ( testObject_QualifiedNewOtrMessage_user_11,
              "testObject_QualifiedNewOtrMessage_user_11.protobuf"
            ),
            ( testObject_QualifiedNewOtrMessage_user_12,
              "testObject_QualifiedNewOtrMessage_user_12.protobuf"
            ),
            ( testObject_QualifiedNewOtrMessage_user_13,
              "testObject_QualifiedNewOtrMessage_user_13.protobuf"
            ),
            ( testObject_QualifiedNewOtrMessage_user_14,
              "testObject_QualifiedNewOtrMessage_user_14.protobuf"
            ),
            ( testObject_QualifiedNewOtrMessage_user_15,
              "testObject_QualifiedNewOtrMessage_user_15.protobuf"
            ),
            ( testObject_QualifiedNewOtrMessage_user_16,
              "testObject_QualifiedNewOtrMessage_user_16.protobuf"
            ),
            ( testObject_QualifiedNewOtrMessage_user_17,
              "testObject_QualifiedNewOtrMessage_user_17.protobuf"
            ),
            ( testObject_QualifiedNewOtrMessage_user_18,
              "testObject_QualifiedNewOtrMessage_user_18.protobuf"
            ),
            ( testObject_QualifiedNewOtrMessage_user_19,
              "testObject_QualifiedNewOtrMessage_user_19.protobuf"
            ),
            ( testObject_QualifiedNewOtrMessage_user_20,
              "testObject_QualifiedNewOtrMessage_user_20.protobuf"
            )
          ]
    ]
