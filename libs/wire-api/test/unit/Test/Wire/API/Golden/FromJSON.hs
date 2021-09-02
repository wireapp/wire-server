-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.FromJSON where

import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Test.Wire.API.Golden.Generated.NewConvUnmanaged_user
import Test.Wire.API.Golden.Generated.NewOtrMessage_user
import Test.Wire.API.Golden.Generated.RmClient_user
import Test.Wire.API.Golden.Generated.SimpleMember_user
import Test.Wire.API.Golden.Runner
import Wire.API.Conversation (Conversation)
import Wire.API.User.Client (RmClient)

tests :: TestTree
tests =
  testGroup
    "FromJSON golden tests"
    [ testCase "NewOtrMessage" $
        testFromJSONObjects
          [(testObject_NewOtrMessage_user_1, "testObject_NewOtrMessage_user_1.json")],
      testCase "SimpleMember" $
        testFromJSONObjects
          [(testObject_SimpleMember_user_1, "testObject_SimpleMember_user_1.json")],
      testCase "NewConv" $
        testFromJSONObjects
          [ (testObject_NewConvUnmanaged_user_1, "testObject_NewConvUnmanaged_user_1.json"),
            (testObject_NewConvUnmanaged_user_21, "testObject_NewConvUnmanaged_user_21.json")
          ],
      testCase "RmClient" $
        testFromJSONObjects
          [(testObject_RmClient_user_4, "testObject_RmClient_user_4.json")],
      testCase "RmClient failure" $
        testFromJSONFailure @RmClient "testObject_RmClient_failure.json",
      testCase "QualifiedConversationId" $
        testFromJSONFailure @Conversation "testObject_Conversation_qualifiedId.json"
    ]
