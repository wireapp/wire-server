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

module Test.Wire.API.Federation.Golden.GoldenSpec where

import Imports
import Test.Hspec
import qualified Test.Wire.API.Federation.Golden.ConversationMemberUpdate as ConversationMemberUpdate
import qualified Test.Wire.API.Federation.Golden.LeaveConversationRequest as LeaveConversationRequest
import qualified Test.Wire.API.Federation.Golden.LeaveConversationResponse as LeaveConversationResponse
import qualified Test.Wire.API.Federation.Golden.MessageSendResponse as MessageSendResponse
import Test.Wire.API.Federation.Golden.Runner (testObjects)

spec :: Spec
spec =
  describe "Golden tests" $ do
    testObjects
      [ (MessageSendResponse.testObject_MessageSendReponse1, "testObject_MessageSendReponse1.json"),
        (MessageSendResponse.testObject_MessageSendReponse2, "testObject_MessageSendReponse2.json"),
        (MessageSendResponse.testObject_MessageSendReponse3, "testObject_MessageSendReponse3.json"),
        (MessageSendResponse.testObject_MessageSendReponse4, "testObject_MessageSendReponse4.json"),
        (MessageSendResponse.testObject_MessageSendReponse5, "testObject_MessageSendReponse5.json")
      ]
    testObjects [(LeaveConversationRequest.testObject_LeaveConversationRequest1, "testObject_LeaveConversationRequest1.json")]
    testObjects
      [ (ConversationMemberUpdate.testObject_ConversationMemberUpdate1, "testObject_ConversationMemberUpdate1.json"),
        (ConversationMemberUpdate.testObject_ConversationMemberUpdate2, "testObject_ConversationMemberUpdate2.json")
      ]
    testObjects
      [ (LeaveConversationResponse.testObject_LeaveConversationResponse1, "testObject_LeaveConversationResponse1.json"),
        (LeaveConversationResponse.testObject_LeaveConversationResponse2, "testObject_LeaveConversationResponse2.json"),
        (LeaveConversationResponse.testObject_LeaveConversationResponse3, "testObject_LeaveConversationResponse3.json"),
        (LeaveConversationResponse.testObject_LeaveConversationResponse4, "testObject_LeaveConversationResponse4.json"),
        (LeaveConversationResponse.testObject_LeaveConversationResponse5, "testObject_LeaveConversationResponse5.json"),
        (LeaveConversationResponse.testObject_LeaveConversationResponse6, "testObject_LeaveConversationResponse6.json"),
        (LeaveConversationResponse.testObject_LeaveConversationResponse7, "testObject_LeaveConversationResponse7.json"),
        (LeaveConversationResponse.testObject_LeaveConversationResponse8, "testObject_LeaveConversationResponse8.json"),
        (LeaveConversationResponse.testObject_LeaveConversationResponse9, "testObject_LeaveConversationResponse9.json")
      ]
