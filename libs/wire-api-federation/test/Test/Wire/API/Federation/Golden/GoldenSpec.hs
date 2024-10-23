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

module Test.Wire.API.Federation.Golden.GoldenSpec where

import Imports
import Test.Hspec
import Test.Wire.API.Federation.Golden.ConversationCreated qualified as ConversationCreated
import Test.Wire.API.Federation.Golden.ConversationUpdate qualified as ConversationUpdate
import Test.Wire.API.Federation.Golden.GetOne2OneConversationResponse qualified as GetOne2OneConversationResponse
import Test.Wire.API.Federation.Golden.LeaveConversationRequest qualified as LeaveConversationRequest
import Test.Wire.API.Federation.Golden.LeaveConversationResponse qualified as LeaveConversationResponse
import Test.Wire.API.Federation.Golden.MLSMessageSendingStatus qualified as MLSMessageSendingStatus
import Test.Wire.API.Federation.Golden.MessageSendResponse qualified as MessageSendResponse
import Test.Wire.API.Federation.Golden.NewConnectionRequest qualified as NewConnectionRequest
import Test.Wire.API.Federation.Golden.NewConnectionResponse qualified as NewConnectionResponse
import Test.Wire.API.Federation.Golden.Runner (testObjects)

spec :: Spec
spec =
  describe "Golden tests" $ do
    testObjects
      [ (MessageSendResponse.testObject_MessageSendResponse1, "testObject_MessageSendResponse1.json"),
        (MessageSendResponse.testObject_MessageSendResponse2, "testObject_MessageSendResponse2.json"),
        (MessageSendResponse.testObject_MessageSendResponse3, "testObject_MessageSendResponse3.json"),
        (MessageSendResponse.testObject_MessageSendResponse4, "testObject_MessageSendResponse4.json"),
        (MessageSendResponse.testObject_MessageSendResponse5, "testObject_MessageSendResponse5.json"),
        (MessageSendResponse.testObject_MessageSendResponse6, "testObject_MessageSendResponse6.json")
      ]
    testObjects
      [ (MLSMessageSendingStatus.testObject_MLSMessageSendingStatus1, "testObject_MLSMessageSendingStatus1.json"),
        (MLSMessageSendingStatus.testObject_MLSMessageSendingStatus2, "testObject_MLSMessageSendingStatus2.json"),
        (MLSMessageSendingStatus.testObject_MLSMessageSendingStatus3, "testObject_MLSMessageSendingStatus3.json")
      ]
    testObjects [(LeaveConversationRequest.testObject_LeaveConversationRequest1, "testObject_LeaveConversationRequest1.json")]
    testObjects
      [ (ConversationUpdate.testObject_ConversationUpdate1V0, "testObject_ConversationUpdate1V0.json"),
        (ConversationUpdate.testObject_ConversationUpdate2V0, "testObject_ConversationUpdate2V0.json")
      ]
    testObjects
      [ (ConversationUpdate.testObject_ConversationUpdate1, "testObject_ConversationUpdate1.json"),
        (ConversationUpdate.testObject_ConversationUpdate2, "testObject_ConversationUpdate2.json")
      ]
    testObjects
      [ (LeaveConversationResponse.testObject_LeaveConversationResponse1, "testObject_LeaveConversationResponse1.json"),
        (LeaveConversationResponse.testObject_LeaveConversationResponse2, "testObject_LeaveConversationResponse2.json"),
        (LeaveConversationResponse.testObject_LeaveConversationResponse3, "testObject_LeaveConversationResponse3.json"),
        (LeaveConversationResponse.testObject_LeaveConversationResponse8, "testObject_LeaveConversationResponse8.json")
      ]
    testObjects
      [ (NewConnectionRequest.testObject_NewConnectionRequest1, "testObject_NewConnectionRequest1.json"),
        (NewConnectionRequest.testObject_NewConnectionRequest2, "testObject_NewConnectionRequest2.json")
      ]
    testObjects
      [ (NewConnectionResponse.testObject_NewConnectionResponse1, "testObject_NewConnectionResponse1.json"),
        (NewConnectionResponse.testObject_NewConnectionResponse2, "testObject_NewConnectionResponse2.json"),
        (NewConnectionResponse.testObject_NewConnectionResponse3, "testObject_NewConnectionResponse3.json"),
        (NewConnectionResponse.testObject_NewConnectionResponse4, "testObject_NewConnectionResponse4.json")
      ]
    testObjects
      [ (ConversationCreated.testObject_ConversationCreated1, "testObject_ConversationCreated1.json"),
        (ConversationCreated.testObject_ConversationCreated2, "testObject_ConversationCreated2.json")
      ]
    testObjects
      [ (GetOne2OneConversationResponse.testObject_GetOne2OneConversationResponseV2Ok, "testObject_GetOne2OneConversationResponseV2Ok.json"),
        (GetOne2OneConversationResponse.testObject_GetOne2OneConversationResponseV2BackendMismatch, "testObject_GetOne2OneConversationResponseV2BackendMismatch.json"),
        (GetOne2OneConversationResponse.testObject_GetOne2OneConversationResponseV2NotConnected, "testObject_GetOne2OneConversationResponseV2NotConnected.json")
      ]
    testObjects
      [ (GetOne2OneConversationResponse.testObject_GetOne2OneConversationResponseOk, "testObject_GetOne2OneConversationResponseOk.json"),
        (GetOne2OneConversationResponse.testObject_GetOne2OneConversationResponseBackendMismatch, "testObject_GetOne2OneConversationResponseBackendMismatch.json"),
        (GetOne2OneConversationResponse.testObject_GetOne2OneConversationResponseNotConnected, "testObject_GetOne2OneConversationResponseNotConnected.json")
      ]
