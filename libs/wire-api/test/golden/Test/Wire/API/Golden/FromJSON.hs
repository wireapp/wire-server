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

module Test.Wire.API.Golden.FromJSON where

import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Test.Wire.API.Golden.Generated.Invite_user (testObject_Invite_user_2)
import Test.Wire.API.Golden.Generated.MemberUpdateData_user
import Test.Wire.API.Golden.Generated.NewOtrMessage_user
import Test.Wire.API.Golden.Generated.RmClient_user
import Test.Wire.API.Golden.Generated.SimpleMember_user
import Test.Wire.API.Golden.Runner
import Wire.API.Conversation (Conversation, MemberUpdate, OtherMemberUpdate)
import Wire.API.User (NewUser, NewUserPublic)
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
          [ (testObject_SimpleMember_user_2, "testObject_SimpleMember_user_2.json"),
            (testObject_SimpleMember_user_2, "testObject_SimpleMember_user_2-2.json")
          ],
      testCase
        "RmClient"
        $ testFromJSONObjects
          [(testObject_RmClient_user_4, "testObject_RmClient_user_4.json")],
      testCase "RmClient failure" $
        testFromJSONFailure @RmClient "testObject_RmClient_failure.json",
      testCase "QualifiedConversationId" $
        testFromJSONFailure @Conversation "testObject_Conversation_qualifiedId.json",
      testCase "Invite" $
        testFromJSONObject testObject_Invite_user_2 "testObject_Invite_user_2.json",
      testCase "MemberUpdate" $
        testFromJSONFailureWithMsg @MemberUpdate
          ( Just $
              "One of { 'otr_muted_ref', 'otr_archived', 'otr_archived_ref', \
              \'hidden', 'hidden_ref', 'conversation_role'} required."
          )
          "testObject_MemberUpdate_user_3.json",
      testCase "MemberUpdateData" $
        testFromJSONObject
          testObject_MemberUpdateData_user_1
          "testObject_MemberUpdateData_user_1.json",
      testCase "OtherMemberUpdate" $
        testFromJSONFailure @OtherMemberUpdate "testObject_OtherMemberUpdate_user_2.json",
      testGroup "NewUser: failure" $
        [ testCase "testObject_NewUser_user_3-2.json" $
            testFromJSONFailureWithMsg @NewUser
              (Just "Only users without an identity can expire")
              "testObject_NewUser_user_3-2.json",
          testCase "testObject_NewUser_user_5-2.json" $
            testFromJSONFailureWithMsg @NewUser
              (Just "all team users must set a password on creation")
              "testObject_NewUser_user_5-2.json",
          testCase "testObject_NewUser_user_6-3.json" $
            testFromJSONFailureWithMsg @NewUser
              (Just "sso_id, team_id must be either both present or both absent.")
              "testObject_NewUser_user_6-3.json",
          testCase "testObject_NewUser_user_7.json" $
            testFromJSONFailureWithMsg @NewUser
              (Just "Users cannot be registered with a phone number anymore")
              "testObject_NewUser_user_7.json",
          testCase "testObject_NewUser_user_8.json" $
            testFromJSONFailureWithMsg @NewUser
              (Just "Users cannot be registered with a phone number anymore")
              "testObject_NewUser_user_8.json"
        ],
      testGroup "NewUserPublic: failure" $
        [ testCase "testObject_NewUserPublic_user_1-2.json" $
            testFromJSONFailureWithMsg @NewUserPublic
              (Just "it is not allowed to provide a UUID for the users here.")
              "testObject_NewUserPublic_user_1-2.json",
          testCase "testObject_NewUserPublic_user_1-3.json" $
            testFromJSONFailureWithMsg @NewUserPublic
              (Just "only managed-by-Wire users can be created here.")
              "testObject_NewUserPublic_user_1-3.json"
        ]
    ]
